{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Discord.Internal.Voice
Description : Strictly for internal use only. See Discord.Voice for the public interface.
Copyright   : (c) 2021-2022 Yuto Takano
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
module Discord.Internal.Voice
    ( module Discord.Internal.Voice
    ) where

import Codec.Audio.Opus.Encoder
import Conduit
import Control.Concurrent
    ( forkIO
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
import Control.Concurrent.STM ( atomically, newEmptyTMVar, putTMVar, tryReadTMVar )
import Control.Concurrent.BoundedChan qualified as Bounded
import Control.Exception.Safe ( catch )
import Control.Monad.Reader ( ask, runReaderT )
import Control.Monad.Except ( runExceptT, throwError )
import Control.Monad ( when, void, forM_ )
import Data.Aeson
import Data.Aeson.Types ( parseMaybe )
import Data.ByteString qualified as B
import Data.Conduit.Process.Typed
import Data.Foldable ( traverse_ )
import Data.Function ( on )
import Data.List ( intercalate, groupBy )
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe ( isNothing )
import Data.Text qualified as T
import Lens.Micro
import Lens.Micro.Extras (view)
import System.IO ( hGetContents, hWaitForInput )
import System.IO.Error ( isEOFError )
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
import Discord.Internal.Voice.OggParser
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
    loopForBothEvents (Just a) (Just (b, c, d)) _ = pure (a, b, c, d)
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

-- | @getPipeline resource codec@ returns some information about the pipeline
-- required for the audio before it is sent to Discord. See the 'AudioPipeline'
-- datatype for the flags set.
--
-- The audio resource pipeline can be complex. Even if the source is already
-- OPUS encoded (and Discord wants OPUS), if there is an FFmpeg filter desired
-- then discord-haskell-voice will need to launch a FFmpeg subprocess. If there
-- is also a ByteString conduit filter also desired, then the FFmpeg subprocess
-- will need to output PCM ByteString, which is then transcoded to OPUS within
-- Haskell. This complex behaviour is hard to keep track of, so this function
-- acts as an abstraction layer that basically returns what things are necessary
-- for a given audio resource.
getPipeline :: AudioResource -> AudioCodec -> AudioPipeline
getPipeline (AudioResource (Left _path) _ Nothing) OPUSCodec = AudioPipeline
    { audioPipelineNeedsFFmpeg = True
    , audioPipelineOutputCodec = OPUSFinalOutput
    }
getPipeline (AudioResource (Right _bs) _ Nothing) OPUSCodec = AudioPipeline
    { audioPipelineNeedsFFmpeg = False
    , audioPipelineOutputCodec = OPUSFinalOutput
    }
getPipeline (AudioResource (Left _path) _ Nothing) PCMCodec = AudioPipeline
    { audioPipelineNeedsFFmpeg = True
    , audioPipelineOutputCodec = PCMFinalOutput
    }
getPipeline (AudioResource (Right _bs) _ Nothing) PCMCodec = AudioPipeline
    { audioPipelineNeedsFFmpeg = False
    , audioPipelineOutputCodec = PCMFinalOutput
    }
getPipeline (AudioResource _ _ Nothing) UnknownCodec = AudioPipeline
    { audioPipelineNeedsFFmpeg = True
    , audioPipelineOutputCodec = OPUSFinalOutput
    }
getPipeline (AudioResource _ _ (Just (FFmpegTransformation _))) _ = AudioPipeline
    { audioPipelineNeedsFFmpeg = True
    , audioPipelineOutputCodec = OPUSFinalOutput
    }
getPipeline (AudioResource (Left _path) _ (Just (HaskellTransformation _))) PCMCodec = AudioPipeline
    { audioPipelineNeedsFFmpeg = True
    , audioPipelineOutputCodec = PCMFinalOutput
    }
getPipeline (AudioResource (Right _bs) _ (Just (HaskellTransformation _))) PCMCodec = AudioPipeline
    { audioPipelineNeedsFFmpeg = False
    , audioPipelineOutputCodec = PCMFinalOutput
    }
getPipeline (AudioResource _ _ (Just (HaskellTransformation _))) _ = AudioPipeline
    { audioPipelineNeedsFFmpeg = True
    , audioPipelineOutputCodec = PCMFinalOutput
    }
getPipeline (AudioResource _ _ (Just (_ ::.: _))) _ = AudioPipeline
    { audioPipelineNeedsFFmpeg = True
    , audioPipelineOutputCodec = PCMFinalOutput
    }
getPipeline _ (ProbeCodec _) = error $
    "Impossible! getPipeline should only be called after codec is probed." -- TODO: encode in type

-- | @play resource codec@ plays the specified @resource@. The codec argument
-- should be 'OPUSCodec' or 'PCMCodec' if you know that the resource is already
-- in this format (it can save computational power to transcode). If you do not
-- know and want to unconditionally use ffmpeg to transcode, then specify the
-- codec as 'UnknownCodec'. If you do not know but can afford to wait a few
-- seconds at first to probe the codec (using @ffprobe@) so that we may be able
-- to skip transcoding if it's either PCM or OPUS, then use 'ProbeCodec'.
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
    handles <- UnliftIO.readMVar $ h ^. voiceHandles

    updateSpeakingStatus True
    let pipeline = getPipeline resource codec

    -- If the pipeline before it gets to Haskell outputs in OPUS, we can send
    -- those directly to Discord. For PCM, there is likely a transformation we
    -- have to apply (or perhaps none, if it was a PCM bytestring with no FFmpeg).
    -- We then use the Haskell opus library to transcode and send natively.
    let finalProcessing = case pipeline ^. outputCodec of
            OPUSFinalOutput -> transPipe liftIO unwrapOggPacketsC .| sinkHandles handles
            PCMFinalOutput -> case resource ^. transform of
                Just (HaskellTransformation tsC) -> tsC .| encodeOpusC .| sinkHandles handles
                Just (_ ::.: tsC) -> tsC .| encodeOpusC .| sinkHandles handles
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
        let (iFlag, inputStream) = case resource ^. stream of
                Left filepath -> (filepath, nullStream)
                Right lbs     -> ("pipe:0", byteStringInput lbs)
        -- Get all the filter argument pairs
        let filtArgs = groupFiltArgs $ concatMap filterToArg $ case resource ^. transform of
                Just (FFmpegTransformation fils) -> NonEmpty.toList fils
                Just (fils ::.: _)               -> NonEmpty.toList fils
                _ -> []
        let outputFlags = case pipeline ^. outputCodec of
                -- OPUS output from FFmpeg will be in an OGG container.
                OPUSFinalOutput -> ["-f", "opus", "-map", "0:a", "-c:a", "libopus", "-b:a", "128K", "-ar", "48000", "-ac", "2"]
                PCMFinalOutput -> ["-f", "s16le", "-c:a", "pcm_s16le", "-ar", "48000", "-ac", "2"]
        let args = [ "-i", iFlag
                    ] <> concatMap (\(k, v) -> [k, v]) filtArgs <> outputFlags <>
                    [ "pipe:1"
                    , "-loglevel", "warning"
                    , "-xerror"
                    ]

        let processConfig = proc "ffmpeg" args
                & setStdin inputStream
                & setStdout createSource
                & setStderr createPipe
        -- run the process, but send SIGTERM and terminate it when our inner
        -- function exits. Since we use the stdout conduit source, the inner
        -- func shouldn't exit prematurely. The only possible way we may exit
        -- prematurely is when we get a signal from stderr, in which case it's
        -- desired to stop the process rather than wait until safe termination
        -- (which may never happen).
        liftDiscord $ withProcessTerm processConfig $ \process -> do
            errorSignal <- liftIO $ atomically $ newEmptyTMVar
            void $ liftIO $ forkIO $
                -- wait indefinitely until end of handle to see if we catch any
                -- output in stderr.
                void $ hWaitForInput (getStderr process) (-1) `catch` \e -> do
                    when (not $ isEOFError e) $ do
                        -- when there is output in stderr, signal its contents
                        -- back to main thread
                        err <- hGetContents (getStderr process)
                        atomically $ putTMVar errorSignal err
                    return True
            runConduitRes
                $ ( do
                    -- check if we have received an error signal, if so terminate
                    errorsExist <- liftIO $ atomically $ tryReadTMVar errorSignal
                    when (isNothing errorsExist) $ getStdout process
                ) .| finalProcessing

        updateSpeakingStatus False
    where
        -- | @sinkHandles handles@ is a conduit that sinks the source to the
        -- individual channels within DiscordVoiceHandle.
        sinkHandles
            :: [DiscordVoiceHandle]
            -> ConduitT B.ByteString Void (ResourceT DiscordHandler) ()
        sinkHandles handles = getZipSink $
            traverse_ (ZipSink . sinkChan . view (udp . _2 . _2)) handles

        -- | @sinkChan chan@ is a conduit that sinks the source to a single
        -- bounded channel.
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

createYoutubeResource :: String -> Maybe AudioTransformation -> Voice (Maybe AudioResource)
createYoutubeResource query mbTransform = do
    let processConfig = proc "yt-dlp"
            [ "-j"
            , "--default-search", "ytsearch"
            , "--format", "bestaudio/best"
            , query
            ]
    (_exitCode, stdout, _stderr) <- liftIO $ readProcess processConfig

    pure $ do
        -- Chain the Maybe monad
        result <- decode stdout
        url <- flip parseMaybe result $ \obj -> obj .: "url"
        pure $ AudioResource
            { audioResourceStream = Left url
            , audioResourceYouTubeDLInfo = Just result
            , audioResourceTransform = mbTransform
            }

-- |
-- >>> groupFiltArgs $ map filterToArg $ [Reverb 10, Reverb 20]
-- [("-af","aecho=1.0:0.7:10:0.5;aecho=1.0:0.7:20:0.5")]
filterToArg :: FFmpegFilter -> [(String, String)]
filterToArg (Reverb _i) =
    [ ("-guess_layout_max", "0")
    , ("-i", "static/st-patricks-church-patrington.wav")
    , ("-filter_complex", "[0] [1] afir=dry=10:wet=10 [reverb]; [0] [reverb] amix=inputs=2:weights=10 2")
    ]

groupFiltArgs :: [(String, String)] -> [(String, String)]
groupFiltArgs argPairs = groupBy ((==) `on` fst) argPairs
    & map (\args -> (fst (head args), intercalate ";" (map snd args)))
