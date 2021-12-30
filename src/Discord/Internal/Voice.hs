{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Discord.Internal.Voice
Description : Recommended for internal use only. See Discord.Voice for the public interface.
Copyright   : (c) Yuto Takano (2021)
License     : MIT
Maintainer  : moa17stock@email.com

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
    , MVar
    , newEmptyMVar
    , newMVar
    , readMVar
    , putMVar
    , withMVar
    , tryPutMVar
    , modifyMVar_
    )
import Control.Concurrent.BoundedChan qualified as Bounded
import Control.Exception.Safe ( finally, throwTo, catch, throwIO )
import Control.Lens
import Control.Monad.Reader ( ask, liftIO, runReaderT )
import Control.Monad.Except ( runExceptT, throwError )
import Control.Monad.Trans ( lift )
import Control.Monad ( when, void )
import Data.Aeson
import Data.Aeson.Types ( parseMaybe )
import Data.ByteString qualified as B
import Data.Foldable ( traverse_ )
import Data.List ( partition )
import Data.Maybe ( fromJust )
import Data.Text qualified as T
import GHC.Weak ( deRefWeak, Weak )
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
    , Event(..)
    )
import Discord.Internal.Types.VoiceCommon
import Discord.Internal.Types.VoiceWebsocket
    ( VoiceWebsocketSendable(Speaking)
    , SpeakingPayload(..)
    )
import Discord.Internal.Voice.CommonUtils
import Discord.Internal.Voice.WebsocketLoop

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

-- | @liftDiscord@ lifts a computation in DiscordHandler into a computation in
-- Voice. This is useful for performing DiscordHandler/IO actions inside the
-- Voice monad.
liftDiscord :: DiscordHandler a -> Voice a
liftDiscord = lift . lift

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
-- the correct/successful/"Right" behaviour is (), and that the potentially-
-- existing value is of failure.
runVoice :: Voice () -> DiscordHandler (Either VoiceError ())
runVoice action = do
    voiceHandles <- liftIO $ newMVar []
    mutEx <- liftIO $ newMVar ()

    let initialState = DiscordBroadcastHandle voiceHandles mutEx

    result <- finally (runExceptT $ flip runReaderT initialState $ action) $ do
        -- Wrap cleanup action in @finally@ to ensure we always close the
        -- threads even if an exception occurred.
        finalState <- liftIO $ readMVar voiceHandles

        -- Unfortunately, the following updateStatusVoice doesn't always run
        -- when we have entered this @finally@ block through a SIGINT or other
        -- asynchronous exception. The reason is that sometimes, the
        -- discord-haskell websocket sendable thread is killed before this.
        -- There is no way to prevent it, so as a consequence, the bot may
        -- linger in the voice call for a few minutes after the bot program is
        -- killed.
        mapMOf_ (traverse . guildId) (\x -> updateStatusVoice x Nothing False False) finalState
        mapMOf_ (traverse . websocket . _1) (liftIO . killWkThread) finalState

    pure result

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
    h <- lift $ lift $ ask
    -- Duplicate the event channel, so we can read without taking data from event handlers
    events <- liftIO $ dupChan $ gatewayHandleEvents $ discordHandleGateway h

    -- To join a voice channel, we first need to send Voice State Update (Opcode
    -- 4) to the gateway, which will then send us two responses, Dispatch Event
    -- (Voice State Update) and Dispatch Event (Voice Server Update).
    lift $ lift $ updateStatusVoice guildId (Just channelId) False False

    (liftIO . doOrTimeout 5000) (waitForVoiceStatusServerUpdate events) >>= \case
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
            ssrcM <- liftIO $ newEmptyMVar

            uid <- userId . cacheCurrentUser <$> (lift $ lift $ readCache)
            let wsOpts = WebsocketLaunchOpts uid sessionId token guildId endpoint
                    events wsChans udpTidM udpChans ssrcM

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
                lift $ lift $ updateStatusVoice guildId Nothing False False
                liftIO $ killWkThread wsTidWeak
  where
    -- | Continuously take the top item in the gateway event channel until both
    -- Dispatch Event VOICE_STATE_UPDATE and Dispatch Event VOICE_SERVER_UPDATE
    -- are received.
    --
    -- The order is undefined in docs, so this function will block until both
    -- are received in any order.
    waitForVoiceStatusServerUpdate
        :: Chan (Either GatewayException Event)
        -> IO (T.Text, T.Text, GuildId, Maybe T.Text)
    waitForVoiceStatusServerUpdate = loopForBothEvents Nothing Nothing
    
    loopForBothEvents
        :: Maybe T.Text
        -> Maybe (T.Text, GuildId, Maybe T.Text)
        -> Chan (Either GatewayException Event)
        -> IO (T.Text, T.Text, GuildId, Maybe T.Text)
    loopForBothEvents (Just a) (Just (b, c, d)) events = pure (a, b, c, d)
    loopForBothEvents mb1 mb2 events = readChan events >>= \case
        -- Parse UnknownEvent, which are events not handled by discord-haskell.
        Right (UnknownEvent "VOICE_STATE_UPDATE" obj) -> do
            -- Conveniently, we can just pass the result of parseMaybe
            -- back recursively.
            let sessionId = flip parseMaybe obj $ \o -> do
                    o .: "session_id"
            loopForBothEvents sessionId mb2 events
        Right (UnknownEvent "VOICE_SERVER_UPDATE" obj) -> do
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
-- Soundshare and priority are const as False, don't see bots needing them.
-- If and when required, add Bool signatures to this function.
updateSpeakingStatus :: Bool -> Voice ()
updateSpeakingStatus micStatus = do
    h <- (^. voiceHandles) <$> ask
    handles <- liftIO $ readMVar h
    flip (mapMOf_ traverse) handles $ \handle ->
        liftIO $ writeChan (handle ^. websocket . _2 . _2) $ Speaking $ SpeakingPayload
            { speakingPayloadMicrophone = micStatus
            , speakingPayloadSoundshare = False
            , speakingPayloadPriority   = False
            , speakingPayloadDelay      = 0
            , speakingPayloadSSRC       = handle ^. ssrc
            }

-- | @play source@ plays some sound from the conduit @source@, provided in the form of
-- 16-bit Little Endian PCM. The use of Conduit allows you to perform arbitrary
-- lazy transformations of audio data, using all the advantages that Conduit
-- brings. As the base monad for the Conduit is @ResourceT DiscordHandler@, you
-- can access any DiscordHandler effects (through @lift@) or IO effects (through
-- @liftIO@) in the conduit as well.
--
-- For a more specific interface that is easier to use, see the 'playPCMFile',
-- 'playFile', and 'playYoutube' functions.
--
-- @
-- import Conduit ( sourceFile )
--
-- runVoice $ do
--   join gid cid
--   play $ sourceFile "./examples/example.pcm"
-- @
play :: ConduitT () B.ByteString (ResourceT DiscordHandler) () -> Voice ()
play source = do
    h <- ask
    dh <- lift $ lift $ ask
    handles <- liftIO $ readMVar $ h ^. voiceHandles

    updateSpeakingStatus True
    lift $ lift $ UnliftIO.withMVar (h ^. mutEx) $ \_ -> do
        runConduitRes $ source .| encodeOpusC .| sinkHandles handles
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
    enCfg = _EncoderConfig # (opusSR48k, True, app_audio)
    -- 1275 is the max bytes an opus 20ms frame can have
    streamCfg = _StreamConfig # (enCfg, 48*20, 1276)
    loop encoder = await >>= \case
        Nothing -> do
            -- Send at least 5 blank frames (10 just in case)
            yieldMany $ replicate 10 "\248\255\254"
        Just clip -> do
            -- encode the audio
            encoded <- liftIO $ opusEncode encoder streamCfg clip
            -- send it
            yield encoded
            loop encoder

-- | Play some sound on the file system, provided in the form of 16-bit Little
-- Endian PCM. @playPCMFile@ is a handy alias for @'play' . 'sourceFile'@.
--
-- For a variant of this function that allows arbitrary transformations of the
-- audio data through a conduit component, see 'playPCMFile\''.
--
-- To play any other format, it will need to be transcoded using FFmpeg. See
-- 'playFile' for such usage.
playPCMFile
    :: FilePath
    -- ^ The path to the PCM file to play
    -> Voice ()
playPCMFile = play . sourceFile

-- | Play some sound on the file system, provided in the form of 16-bit Little
-- Endian PCM. Audio data will be passed through the @processor@ conduit
-- component, allowing arbitrary transformations to audio data before playback.
--
-- For a variant of this function with no processing, see 'playPCMFile'.
--
-- To play any other format, it will need to be transcoded using FFmpeg. See
-- 'playFile' for such usage.
playPCMFile'
    :: FilePath
    -- ^ The path to the PCM file to play
    -> ConduitT B.ByteString B.ByteString (ResourceT DiscordHandler) ()
    -- ^ Any processing that needs to be done on the audio data
    -> Voice ()
playPCMFile' fp processor = play $ sourceFile fp .| processor

-- | Play some sound on the file system, provided in any format supported by
-- FFmpeg. This function expects "@ffmpeg@" to be available in the system PATH.
--
-- For a variant that allows you to specify the executable and/or any arguments,
-- see 'playFileWith'.
--
-- For a variant of this function that allows arbitrary transformations of the
-- audio data through a conduit component, see 'playFile\''.
--
-- If the file is already known to be in 16-bit little endian PCM, using
-- @playPCMFile@ is much more efficient as it does not go through FFmpeg.
playFile
    :: FilePath
    -- ^ The path to the audio file to play
    -> Voice ()
playFile fp = playFile' fp (awaitForever yield)

-- | Play some sound on the file system, provided in any format supported by
-- FFmpeg. This function expects "@ffmpeg@" to be available in the system PATH.
-- Audio data will be passed through the @processor@ conduit component, allowing
-- arbitrary transformations to audio data before playback.
--
-- For a variant that allows you to specify the executable and/or any arguments,
-- see 'playFileWith\''.
--
-- For a variant of this function with no processing, see 'playFile'.
--
-- If the file is already known to be in 16-bit little endian PCM, using
-- 'playPCMFile\'' is much more efficient as it does not go through FFmpeg.
playFile'
    :: FilePath
    -- ^ The path to the audio file to play
    -> ConduitT B.ByteString B.ByteString (ResourceT DiscordHandler) ()
    -- ^ Any processing that needs to be done on the audio data
    -> Voice ()
playFile' fp = playFileWith' "ffmpeg" defaultFFmpegArgs fp

-- | @defaultFFmpegArgs@ is a generator function for the default FFmpeg
-- arguments used when streaming audio into 16-bit little endian PCM on stdout.
--
-- This function takes in the input file path as an argument, because FFmpeg
-- arguments are position sensitive in relation to the placement of @-i@.
--
-- @
-- -i FILE -f s16le -ar 48000 -ac 2 -loglevel warning pipe:1
-- @
defaultFFmpegArgs :: FilePath -> [String]
defaultFFmpegArgs fp =
    [ "-i", fp
    , "-f", "s16le"
    , "-ar", "48000"
    , "-ac", "2"
    , "-loglevel", "warning"
    , "pipe:1"
    ]

-- | Play some sound on the file system, provided in any format supported by
-- FFmpeg. This function allows you to specify the ffmpeg executable and a
-- generator function to create arguments to it (see @defaultFFmpegArgs@ for
-- the default)
-- 
-- For a variant of this function that uses the "@ffmpeg@" executable in your
-- PATH automatically, see 'playFile'.
--
-- For a variant of this function that allows arbitrary transformations of the
-- audio data through a conduit component, see 'playFileWith\''.
--
-- If the file is known to be in 16-bit little endian PCM, using 'playPCMFile'
-- is more efficient as it does not go through FFmpeg.
playFileWith
    :: String
    -- ^ The name of the FFmpeg executable
    -> (String -> [String])
    -- ^ FFmpeg argument generator function, given the filepath
    -> FilePath
    -> Voice ()
playFileWith exe args fp = playFileWith' exe args fp (awaitForever yield)

-- | Play some sound on the file system, provided in any format supported by
-- FFmpeg. This function allows you to specify the ffmpeg executable and a
-- generator function to create arguments to it (see @defaultFFmpegArgs@ for
-- the default). Audio data will be passed through the @processor@ conduit
-- component, allowing arbitrary transformations to audio data before playback.
-- 
-- For a variant of this function that uses the "@ffmpeg@" executable in your
-- PATH automatically, see 'playFile\''.
--
-- For a variant of this function with no processing, see 'playFileWith'.
--
-- If the file is known to be in 16-bit little endian PCM, using 'playPCMFile\''
-- is more efficient as it does not go through FFmpeg.
playFileWith'
    :: String
    -- ^ The name of the FFmpeg executable
    -> (String -> [String])
    -- ^ FFmpeg argument generator function, given the filepath
    -> String
    -- ^ The path to the audio file to play
    -> ConduitT B.ByteString B.ByteString (ResourceT DiscordHandler) ()
    -- ^ Any processing that needs to be done on the audio data
    -> Voice ()
playFileWith' exe argsGen path processor = do
    let args = argsGen path
    (readEnd, writeEnd) <- liftIO $ createPipe
    (a, b, Just stderr, ph) <- liftIO $ createProcess_ "the ffmpeg process" (proc exe args)
        { std_out = UseHandle writeEnd
        , std_err = CreatePipe
        }
    -- When the FFmpeg subprocess prints something to stderr, it's usually fatal,
    -- but it doesn't exit the process. The following forked process will
    -- continuously read from stderr and if anything is found, will throw an
    -- exception back into this thread.
    myTid <- liftIO myThreadId
    errorListenerTid <- liftIO $ forkIO $ do
        thereIsAnError <- hWaitForInput stderr (-1) `catch` \e ->
            if isEOFError e then return True else throwIO e
        -- thereIsAnError can be flagged True if the process was killed and
        -- I think EOF is written? In which case, the handle is already closed
        -- so we must make sure not to perform hGetContents on the closed handle.
        stillOpen <- hIsOpen stderr
        when (thereIsAnError && stillOpen) $ do
            err <- hGetContents stderr
            exitCode <- terminateProcess ph >> waitForProcess ph
            putStrLn $ "ffmpeg exited with code " ++ show exitCode
            throwTo myTid $ SubprocessException err
    finally (play $ sourceHandle readEnd .| processor) $ do
        liftIO $ cleanupProcess (a, b, Just stderr, ph)
        liftIO $ hClose readEnd >> hClose writeEnd
        liftIO $ killThread errorListenerTid

-- | Play any video or search query from YouTube, automatically transcoded
-- through FFmpeg. This function expects "@ffmpeg@" and "@youtube-dl@" to be
-- available in the system PATH.
--
-- For a variant that allows you to specify the executable and/or any arguments,
-- see 'playYouTubeWith'.
--
-- For a variant of this function that allows arbitrary transformations of the
-- audio data through a conduit component, see 'playYouTube\''.
playYouTube
    :: String
    -- ^ Search query (or video URL)
    -> Voice ()
playYouTube query = playYouTube' query (awaitForever yield)

-- | Play any video or search query from YouTube, automatically transcoded
-- through FFmpeg. This function expects "@ffmpeg@" and "@youtube-dl@" to be
-- available in the system PATH.Audio data will be passed through the
-- @processor@ conduit component, allowing arbitrary transformations to audio
-- data before playback.
--
-- For a variant that allows you to specify the executable and/or any arguments,
-- see 'playYouTubeWith\''.
--
-- For a variant of this function with no processing, see 'playYouTube'.
playYouTube'
    :: String
    -- ^ Search query (or video URL)
    -> ConduitT B.ByteString B.ByteString (ResourceT DiscordHandler) ()
    -- ^ Any processing that needs to be done on the audio data
    -> Voice ()
playYouTube' query processor =
  let
    customArgGen url = 
        [ "-reconnect", "1"
        , "-reconnect_streamed", "1"
        , "-reconnect_delay_max", "2"
        ] <> defaultFFmpegArgs url
  in
    playYouTubeWith' "ffmpeg" customArgGen "youtube-dl" query processor

-- | Play any video or search query from YouTube, automatically transcoded
-- through FFmpeg. This function allows you to specify the ffmpeg executable,
-- a generator function to create arguments to it (see @defaultFFmpegArgs@ for
-- the default), as well as the youtube-dl executable.
--
-- For a variant of this function that uses the "@ffmpeg@" executable and 
-- "@youtube-dl@" executable in your PATH automatically, see 'playYouTube'.
--
-- For a variant of this function that allows arbitrary transformations of the
-- audio data through a conduit component, see 'playYouTubeWith\''.
playYouTubeWith
    :: String
    -- ^ The name of the FFmpeg executable
    -> (String -> [String])
    -- ^ FFmpeg argument generator function, given the URL
    -> String
    -- ^ The name of the youtube-dl executable
    -> String
    -- ^ The search query (or video URL)
    -> Voice ()
playYouTubeWith fexe fargsGen yexe query = playYouTubeWith' fexe fargsGen yexe query (awaitForever yield)

-- | Play any video or search query from YouTube, automatically transcoded
-- through FFmpeg. This function allows you to specify the ffmpeg executable,
-- a generator function to create arguments to it (see @defaultFFmpegArgs@ for
-- the default), as well as the youtube-dl executable. Audio data will be passed
-- through the @processor@ conduit component, allowing arbitrary transformations
-- to audio data before playback.
--
-- For a variant of this function that uses the "@ffmpeg@" executable and 
-- "@youtube-dl@" executable in your PATH automatically, see 'playYouTube\''.
--
-- For a variant of this function with no processing, see 'playYouTubeWith'.
playYouTubeWith'
    :: String
    -- ^ The name of the FFmpeg executable
    -> (String -> [String])
    -- ^ The arguments to pass to FFmpeg
    -> String
    -- ^ The name of the youtube-dl executable
    -> String
    -- ^ The search query (or video URL)
    -> ConduitT B.ByteString B.ByteString (ResourceT DiscordHandler) ()
    -- ^ Any processing that needs to be done on the audio data
    -> Voice ()
playYouTubeWith' fexe fargsGen yexe query processor = do
    extractedInfo <- liftIO $ withCreateProcess (proc yexe
        [ "-j"
        , "--default-search", "ytsearch"
        , "--format", "bestaudio/best"
        , query
        ]) { std_out = CreatePipe } $ \stdin (Just stdout) stderr ph ->
            B.hGetContents stdout

    let perhapsUrl = do
            result <- decodeStrict extractedInfo
            flip parseMaybe result $ \obj -> obj .: "url"
    case perhapsUrl of
        -- no matching url found
        Nothing -> pure ()
        Just url -> playFileWith' fexe fargsGen url processor
