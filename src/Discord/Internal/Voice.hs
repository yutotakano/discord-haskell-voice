{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Discord.Internal.Voice
Description : Strictly for internal use only. See Discord.Voice for the public interface.
Copyright   : (c) Yuto Takano (2021)
License     : MIT
Maintainer  : moa17stock@gmail.com

= WARNING

This module is considered __internal__.

The Package Versioning Policy __does not apply__.

The contents of this module may change __in any way whatsoever__ and __without__
__any warning__ between minor versions of this package.

= Description

This module is the internal entry point into @discord-haskell-voice@. Any use of
this module (or other Internal modules) is discouraged. Please see "Discord.Voice"
for the public interface.
-}
module Discord.Internal.Voice where

import Codec.Audio.Opus.Encoder
import Conduit
import Control.Concurrent.Async ( race )
import Control.Concurrent
    ( ThreadId
    , myThreadId
    , threadDelay
    , killThread
    , forkIO
    , mkWeakThreadId
    , Chan
    , dupChan
    , newChan
    , readChan
    , writeChan
    -- We prefer UnliftIO.MVar functions for most MVar-related operations in
    -- DiscordHandler, but since the Voice monad doesn't have MonadUnliftIO
    -- (because it has an ExceptT transformer), we use the default
    -- Control.Concurrent.MVar functions there.
    , newEmptyMVar
    , modifyMVar_
    , readMVar
    )
import Control.Concurrent.BoundedChan qualified as Bounded
import Control.Exception.Safe ( bracket, throwTo, catch, throwIO )
import Lens.Micro
import Lens.Micro.Extras (view)
import Control.Monad.Reader ( ask, liftIO, runReaderT )
import Control.Monad.Except ( runExceptT, throwError )
import Control.Monad.Trans ( lift )
import Control.Monad ( when, void, forM_ )
import Data.Aeson
import Data.Aeson.Types ( parseMaybe )
import Data.ByteString qualified as B
import Data.Foldable ( traverse_ )
import Data.List ( partition )
import Data.Maybe ( fromJust )
import Data.Text qualified as T
import GHC.Weak ( deRefWeak, Weak )
import System.Exit ( ExitCode(..) )
import System.IO ( hClose, hGetContents, hWaitForInput, hIsOpen )
import System.IO.Error ( isEOFError )
import System.Process
import UnliftIO qualified as UnliftIO

import Discord ( DiscordHandler, sendCommand, readCache )
import Discord.Handle ( discordHandleGateway, discordHandleLog )
import Discord.Internal.Gateway.Cache ( Cache(..) )
import Discord.Internal.Gateway.EventLoop
    ( GatewayException(..)
    , GatewayHandle(..)
    )
import Discord.Internal.Types
    ( GuildId
    , ChannelId
    , UserId
    , User(..)
    , GatewaySendable(..)
    , UpdateStatusVoiceOpts(..)
    , EventInternalParse (..)
    )
import Discord.Internal.Types.VoiceCommon
import Discord.Internal.Types.VoiceWebsocket
    ( VoiceWebsocketSendable(Speaking)
    , SpeakingPayload(..)
    )
import Discord.Internal.Voice.CommonUtils
import Discord.Internal.Voice.WebsocketLoop

-- | @liftDiscord@ lifts a computation in DiscordHandler into a computation in
-- Voice. This is useful for performing DiscordHandler actions inside the
-- Voice monad.
--
-- Usage:
-- 
-- @
-- runVoice $ do
--     join (read "123456789012345") (read "67890123456789012")
--     liftDiscord $ void $ restCall $ R.CreateMessage (read "2938481828383") "Joined!"
--     liftIO $ threadDelay 5e6
--     playYouTube "Rate of Reaction of Sodium Hydroxide and Hydrochloric Acid"
--     liftDiscord $ void $ restCall $ R.CreateMessage (read "2938481828383") "Finished!"
-- void $ restCall $ R.CreateMessage (read "2938481828383") "Finished all voice actions!"
-- @
--
liftDiscord :: DiscordHandler a -> Voice a
liftDiscord = Voice . lift . lift

-- | Execute the voice actions stored in the Voice monad.
--
-- A single mutex and sending packet channel is used throughout all voice
-- connections within the actions, which enables multi-channel broadcasting.
-- The following demonstrates how a single playback is streamed to multiple
-- connections.
--
-- @
-- runVoice $ do
--     join (read "123456789012345") (read "67890123456789012")
--     join (read "098765432123456") (read "12345698765456709")
--     playYouTube "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
-- @
--
-- The return type of @runVoice@ represents result status of the voice computation.
-- It is isomorphic to @Maybe@, but the use of Either explicitly denotes that
-- the correct\/successful\/'Right' behaviour is (), and that the potentially-
-- existing value is of failure.
--
-- This function may propagate and throw an 'IOException' if 'createProcess' 
-- fails for e.g. ffmpeg or youtube-dl.
runVoice :: Voice () -> DiscordHandler (Either VoiceError ())
runVoice action = do
    voiceHandles <- UnliftIO.newMVar []
    mutEx <- UnliftIO.newMVar ()

    let initialState = DiscordBroadcastHandle voiceHandles mutEx

    result <- runExceptT $ flip runReaderT initialState $ unVoice $ action

    -- Wrap cleanup action in @finally@ to ensure we always close the
    -- threads even if an exception occurred.
    finalState <- UnliftIO.readMVar voiceHandles

    -- Unfortunately, the following updateStatusVoice doesn't always run
    -- when we have entered this @finally@ block through a SIGINT or other
    -- asynchronous exception. The reason is that sometimes, the
    -- discord-haskell websocket sendable thread is killed before this.
    -- There is no way to prevent it, so as a consequence, the bot may
    -- linger in the voice call for a few minutes after the bot program is
    -- killed.
    traverseOf_ (traverse . guildId) (\x -> updateStatusVoice x Nothing False False) finalState
    traverseOf_ (traverse . websocket . _1) (liftIO . killWkThread) finalState

    return result

-- | Join a specific voice channel, given the Guild and Channel ID of the voice
-- channel. Since the Channel ID is globally unique, there is theoretically no
-- need to specify the Guild ID, but it is provided until discord-haskell fully
-- caches the mappings internally.
--
-- This function returns a Voice action that, when executed, will leave the
-- joined voice channel. For example:
--
-- @
-- runVoice $ do
--   leave <- join (read "123456789012345") (read "67890123456789012")
--   playYouTube "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
--   leave
-- @
--
-- The above use is not meaningful in practice, since @runVoice@ will perform
-- the appropriate cleanup and leaving as necessary at the end of all actions.
-- However, it may be useful to interleave @leave@ with other Voice actions.
--
-- Since the @leave@ function will gracefully do nothing if the voice connection
-- is already severed, it is safe to escape this function from the Voice monad
-- and use it in a different context. That is, the following is allowed and
-- is encouraged if you are building a @\/leave@ command of any sort:
--
-- @
-- -- On \/play
-- runVoice $ do
--   leave <- join (read "123456789012345") (read "67890123456789012")
--   liftIO $ putMVar futureLeaveFunc leave
--   forever $
--     playYouTube "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
--
-- -- On \/leave, from a different thread
-- leave <- liftIO $ takeMVar futureLeaveFunc
-- runVoice leave
-- @
--
-- The above will join a voice channel, play a YouTube video, but immediately
-- quit and leave the channel when the @\/leave@ command is received, regardless
-- of the playback status.
join :: GuildId -> ChannelId -> Voice (Voice ())
join guildId channelId = do
    h <- liftDiscord ask
    -- Duplicate the event channel, so we can read without taking data from event handlers
    events <- liftIO $ dupChan $ gatewayHandleEvents $ discordHandleGateway h

    -- To join a voice channel, we first need to send Voice State Update (Opcode
    -- 4) to the gateway, which will then send us two responses, Dispatch Event
    -- (Voice State Update) and Dispatch Event (Voice Server Update).
    liftDiscord $ updateStatusVoice guildId (Just channelId) False False

    (liftIO . timeoutMs 5000) (waitForVoiceStatusServerUpdate events) >>= \case
        Nothing -> do
            -- did not respond in time: no permission? or discord offline?
            throwError VoiceNotAvailable
        Just (_, _, _, Nothing) -> do
            -- If endpoint is null, according to Docs, no servers are available.
            throwError NoServerAvailable
        Just (sessionId, token, guildId, Just endpoint) -> do
            -- create the sending and receiving channels for Websocket
            wsChans <- liftIO $ (,) <$> newChan <*> newChan
            -- thread id and handles for UDP. 100 packets will contain 2
            -- seconds worth of 20ms audio. Each packet (20ms) contains
            -- (48000 / 1000 * 20 =) 960 frames, for which each frame has
            -- 2 channels and 16 bits (2 bytes) in each channel. So, the total
            -- amount of memory required for each BoundedChan is 2*2*960*100=
            -- 384 kB (kilobytes).
            udpChans <- liftIO $ (,) <$> newChan <*> Bounded.newBoundedChan 100
            udpTidM <- liftIO newEmptyMVar
            -- ssrc to be filled in during initial handshake
            ssrcM <- liftIO newEmptyMVar

            uid <- userId . cacheCurrentUser <$> (liftDiscord readCache)
            let wsOpts = WebsocketLaunchOpts uid sessionId token guildId endpoint
                    wsChans udpTidM udpChans ssrcM

            -- fork a thread to start the websocket thread in the DiscordHandler
            -- monad using the current Reader state. Not much of a problem
            -- since many of the fields are mutable references.
            wsTid <- liftIO $ forkIO $ launchWebsocket wsOpts $ discordHandleLog h
            
            wsTidWeak <- liftIO $ mkWeakThreadId wsTid

            -- TODO: check if readMVar ever blocks if the UDP thread fails to
            -- launch. Handle somehow? Perhaps with exception throwTo?
            udpTid <- liftIO $ readMVar udpTidM
            ssrc <- liftIO $ readMVar ssrcM

            -- modify the current Voice monad state to add the newly created
            -- UDP and Websocket handles (a handle consists of thread id and
            -- send/receive channels).
            voiceState <- ask
            -- Add the new voice handles to the list of handles
            liftIO $ modifyMVar_ (voiceState ^. voiceHandles) $ \handles -> do
                let newHandle = DiscordVoiceHandle guildId channelId
                        (wsTidWeak, wsChans) (udpTid, udpChans) ssrc
                pure (newHandle : handles)

            -- Give back a function used for leaving this voice channel.
            pure $ do
                liftDiscord $ updateStatusVoice guildId Nothing False False
                liftIO $ killWkThread wsTidWeak
  where
    -- | Continuously take the top item in the gateway event channel until both
    -- Dispatch Event VOICE_STATE_UPDATE and Dispatch Event VOICE_SERVER_UPDATE
    -- are received.
    --
    -- The order is undefined in docs, so this function will block until both
    -- are received in any order.
    waitForVoiceStatusServerUpdate
        :: Chan (Either GatewayException EventInternalParse)
        -> IO (T.Text, T.Text, GuildId, Maybe T.Text)
    waitForVoiceStatusServerUpdate = loopForBothEvents Nothing Nothing
    
    loopForBothEvents
        :: Maybe T.Text
        -> Maybe (T.Text, GuildId, Maybe T.Text)
        -> Chan (Either GatewayException EventInternalParse)
        -> IO (T.Text, T.Text, GuildId, Maybe T.Text)
    loopForBothEvents (Just a) (Just (b, c, d)) events = pure (a, b, c, d)
    loopForBothEvents mb1 mb2 events = readChan events >>= \case
        -- Parse UnknownEvent, which are events not handled by discord-haskell.
        Right (InternalUnknownEvent "VOICE_STATE_UPDATE" obj) -> do
            -- Conveniently, we can just pass the result of parseMaybe
            -- back recursively.
            let sessionId = flip parseMaybe obj $ \o -> do
                    o .: "session_id"
            loopForBothEvents sessionId mb2 events
        Right (InternalUnknownEvent "VOICE_SERVER_UPDATE" obj) -> do
            let result = flip parseMaybe obj $ \o -> do
                    token <- o .: "token"
                    guildId <- o .: "guild_id"
                    endpoint <- o .: "endpoint"
                    pure (token, guildId, endpoint)
            loopForBothEvents mb1 result events
        _ -> loopForBothEvents mb1 mb2 events

-- | Helper function to update the speaking indicator for the bot. Setting the
-- microphone status to True is required for Discord to transmit the bot's
-- voice to other clients. It is done automatically in all of the @play*@
-- functions, so there should be no use for this function in practice.
--
-- Note: Soundshare and priority are const as False in the payload because I
-- don't see bots needing them. If and when required, add Bool signatures to
-- this function.
updateSpeakingStatus :: Bool -> Voice ()
updateSpeakingStatus micStatus = do
    h <- (^. voiceHandles) <$> ask
    handles <- UnliftIO.readMVar h
    flip (traverseOf_ traverse) handles $ \handle ->
        liftIO $ writeChan (handle ^. websocket . _2 . _2) $ Speaking $ SpeakingPayload
            { speakingPayloadMicrophone = micStatus
            , speakingPayloadSoundshare = False
            , speakingPayloadPriority   = False
            , speakingPayloadDelay      = 0
            , speakingPayloadSSRC       = handle ^. ssrc
            }

-- | Send a Gateway Websocket Update Voice State command (Opcode 4). Used to
-- indicate that the client voice status (deaf/mute) as well as the channel
-- they are active on.
-- This is not in the Voice monad because it has to be used after all voice
-- actions end, to quit the voice channels. It also has no benefit, since it
-- would cause extra transformer wrapping/unwrapping.
updateStatusVoice
    :: GuildId
    -- ^ Id of Guild
    -> Maybe ChannelId
    -- ^ Id of the voice channel client wants to join (Nothing if disconnecting)
    -> Bool
    -- ^ Whether the client muted
    -> Bool
    -- ^ Whether the client deafened
    -> DiscordHandler ()
updateStatusVoice a b c d = sendCommand $ UpdateStatusVoice $ UpdateStatusVoiceOpts a b c d

probeCodec :: String -> AudioResource -> IO AudioCodec
probeCodec = undefined

-- | todo
-- The audio resource pipeline can be complex. Even if the source is already
-- OPUS encoded (and Discord wants OPUS), if there is an FFmpeg filter desired
-- then discord-haskell-voice will need to launch a FFmpeg subprocess. If there
-- is also a ByteString conduit filter also desired, then the FFmpeg subprocess
-- will need to output PCM ByteString, which is then transcoded to OPUS within
-- Haskell. This complex behaviour is hard to keep track of, so this function
-- acts as an abstraction layer that basically returns what things are necessary
-- for a given audio resource.
getPipeline :: AudioResource -> AudioCodec -> AudioPipeline
getPipeline = undefined

-- TODO
-- | @play source@ plays some sound from the conduit @source@, provided in the
-- form of 16-bit Little Endian PCM. The use of Conduit allows you to perform
-- arbitrary lazy transformations of audio data, using all the advantages that
-- Conduit brings. As the base monad for the Conduit is @ResourceT DiscordHandler@,
-- you can access any DiscordHandler effects (through @lift@) or IO effects
-- (through @liftIO@) in the conduit as well.
--
-- For a more specific interface that is easier to use, see the 'playPCMFile',
-- 'playFile', and 'playYouTube' functions.
-- @
-- import Conduit ( sourceFile )
--
-- runVoice $ do
--   join gid cid
--   play $ sourceFile ".\/audio\/example.pcm"
-- @
play :: AudioResource -> AudioCodec -> Voice ()
play resource (ProbeCodec ffprobeExe) = do
    -- If we are told the audio should be probed for info, do so and rerun play.
    -- The return value of probeCodec will never be ProbeCodec again, so this
    -- will safely recurse only once.
    liftIO (probeCodec ffprobeExe resource) >>= play resource
play resource codec = do
    -- If we are given a codec of some sort, check if we need transcoding. Also
    -- check if we have any transformations that will need FFmpeg.
    h <- ask
    dh <- liftDiscord ask
    handles <- UnliftIO.readMVar $ h ^. voiceHandles

    updateSpeakingStatus True
    let pipeline = getPipeline resource codec

    -- If the pipeline before it gets to Haskell outputs in OPUS, we can send
    -- those directly to Discord. For PCM, there is likely a transformation we
    -- have to apply (or perhaps none, if it was a PCM bytestring with no FFmpeg).
    -- We then use the Haskell opus library to transcode and send natively.
    let finalProcessing = case pipeline ^. outputCodec of
            OPUSFinalOutput -> sinkHandles handles
            PCMFinalOutput -> case resource ^. transform of
                Just (HaskellTransformation tsC) -> tsC .| encodeOpusC .| sinkHandles handles
                _ -> encodeOpusC .| sinkHandles handles

    if not (pipeline ^. needsFFmpeg) then do
        -- The resource doesn't need to go through ffmpeg at all. It can be an
        -- OPUS or PCM filepath that can be read directly as a Conduit source,
        -- or some lazy bytestring containing either OPUS or PCM.
        liftDiscord $ runConduitRes $ case resource ^. stream of
            Left filepath -> sourceFile filepath .| finalProcessing
            Right lbs -> sourceLazy lbs .| finalProcessing

    else do
        -- We need the FFmpeg subprocess. Sources may be file input, stream URL,
        -- or bytestring stdin.
        let (iFlag, stdinHandle) = case resource ^. stream of
                Left filepath -> (filepath, Inherit)
                Right lbs -> ("pipe:0", CreatePipe)
        let outputFlags = case pipeline ^. outputCodec of
                OPUSFinalOutput -> ["-c:a", "libopus", "-b:a", "48K"]
                PCMFinalOutput -> ["-f", "s16le", "-ar", "48000", "-ac", "2"]
        let args = [ "-i", iFlag
                    , "-loglevel", "warning"
                    , "-xerror"
                    ] <> outputFlags <>
                    [ "pipe:1"
                    ]

    -- NOTE: We use CreatePipe for the stdout handle of ffmpeg, but a preexisting
    -- handle for stderr. This is because we want to retain the stderr output
    -- when ffmpeg has exited with an error code, and capture it before manually
    -- closing the handle. Otherwise, the stderr of ffmpeg may be lost. Using
    -- a preexisting handle for stdout is however, avoided, because createProcess_
    -- does not automatically close UseHandles when done, while conduit's
    -- sourceHandle will patiently wait and block forever for the handle to close.
    -- We may use createProcess (notice the lack of underscore) to automatically
    -- close the UseHandles passed into it, but then we 1. lose the error output
    -- for stderr, and 2. there have been frequent occasions of ffmpeg trying to
    -- write to the closed pipe, causing a "broken pipe" fatal error. We want to
    -- therefore make sure that even if that happens, the error is captured and
    -- stored. Perhaps this explanation makes no sense, but I have suffered too
    -- long on this problem (of calling a subprocess, streaming its output,
    -- storing its errors, and making sure they gracefully kill themselves upon
    -- the parent thread being killed) and I am hoping that this is something
    -- I don't have to touch again.
        (errorReadEnd, errorWriteEnd) <- liftIO $ createPipe
        (a, Just stdout, c, ph) <- liftIO $ createProcess_ "ffmpeg" (proc "ffmpeg" args)
            { std_out = CreatePipe
            , std_err = UseHandle errorWriteEnd
            }

    -- We maintain a forked thread that constantly monitors the stderr output,
    -- and if it sees an error, it kills the ffmpeg process so it doesn't block
    -- (sometimes ffmpeg outputs a fatal error but still tries to continue,
    -- especially during streams), and then rethrows the error as a
    -- SubprocessException to the parent (this) thread. The idea is for the
    -- @bracket@ to handle it, properly clean up any remnants, then rethrow it
    -- further up so that user code can handle it, or let it propagate to
    -- the "discord-haskell encountered an exception" handler. However in
    -- practice, I have not seen this exception appear in the logs even once,
    -- even when the preceding putStrLn executes.
        myTid <- liftIO myThreadId
        bracket (liftIO $ forkIO $ do
            thereIsAnError <- hWaitForInput errorReadEnd (-1) `catch` \e ->
                if isEOFError e then return False else throwIO e
            when thereIsAnError $ do
                exitCode <- terminateProcess ph >> waitForProcess ph
                case exitCode of
                    ExitSuccess -> do
                        putStrLn "ffmpeg exited successfully"
                        pure ()
                    ExitFailure i -> do
                        err <- hGetContents errorReadEnd
                        exitCode <- terminateProcess ph >> waitForProcess ph
                        putStrLn $ "ffmpeg exited with code " ++ show exitCode ++ ": " ++ err
                        throwTo myTid $ SubprocessException err
            ) (\tid -> do
                liftIO $ cleanupProcess (a, Just stdout, c, ph)
                liftIO $ killThread tid
            ) $ const $ liftDiscord $ runConduitRes $ sourceHandle stdout .| finalProcessing
        liftIO $ hClose errorReadEnd >> hClose errorWriteEnd

        updateSpeakingStatus False
    where
        sinkHandles
            :: [DiscordVoiceHandle]
            -> ConduitT B.ByteString Void (ResourceT DiscordHandler) ()
        sinkHandles handles = getZipSink $
            traverse_ (ZipSink . sinkChan . view (udp . _2 . _2)) handles

        sinkChan
            :: Bounded.BoundedChan B.ByteString
            -> ConduitT B.ByteString Void (ResourceT DiscordHandler) ()
        sinkChan chan = await >>= \case
            Nothing -> pure ()
            Just bs -> do
                liftIO $ Bounded.writeChan chan bs
                sinkChan chan

-- | @encodeOpusC@ is a conduit that splits the ByteString into chunks of
-- (frame size * no of channels * 16/8) bytes, and encodes each chunk into
-- OPUS format. ByteStrings are made of CChars (Int8)s, but the data is 16-bit
-- so this is why we multiply by two to get the right amount of bytes instead of
-- prematurely cutting off at the half-way point.
encodeOpusC :: ConduitT B.ByteString B.ByteString (ResourceT DiscordHandler) ()
encodeOpusC = chunksOfCE (48*20*2*2) .| do
    encoder <- liftIO $ opusEncoderCreate enCfg
    loop encoder
  where
    enCfg = mkEncoderConfig opusSR48k True app_audio
    -- 1275 is the max bytes an opus 20ms frame can have
    streamCfg = mkStreamConfig enCfg (48*20) 1276
    loop encoder = await >>= \case
        Nothing -> do
            -- Send at least 5 blank frames (20ms * 5 = 100 ms)
            let frame = B.pack $ concat $ replicate 1280 [0xF8, 0xFF, 0xFE]
            encoded <- liftIO $ opusEncode encoder streamCfg frame
            forM_ [0..4] $ \_ -> do
                yield encoded
        Just frame -> do
            -- encode the audio
            encoded <- liftIO $ opusEncode encoder streamCfg frame
            -- send it
            yield encoded
            loop encoder
