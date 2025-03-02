{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Discord.Internal.Voice
Description : Strictly for internal use only. See Discord.Voice for the public interface.
Copyright   : (c) 2021-2022 Yuto Takano
              (c) 2025-PRESENT discord-haskell-voice Contributors
License     : MIT
Maintainer  : Yuto Takano <moa17stock@gmail.com>

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
import Control.Exception.Safe ( catch, throwIO )
import Control.Monad.Reader ( ask, runReaderT )
import Control.Monad.Except ( runExceptT )
import Control.Monad ( when, void, forM_ )
import Control.Exception ( BlockedIndefinitelyOnMVar(..) )
import Data.Aeson
import Data.Aeson.Types ( parseMaybe )
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
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

-- | @liftDiscord@ lifts a computation in 'DiscordHandler' into a computation in
-- 'Voice'. This is useful for performing DiscordHandler actions inside the
-- Voice monad. To perform IO actions inside the Voice monad, use 'liftIO'.
--
-- ## Examples
--
-- @
-- func :: DiscordHandler ()
-- func = do
--     let textChannel = read "2938481828383"
--     void $ restCall $ R.CreateMessage textChannel "Joining!"
--     runVoice $ do
--         join (read "123456789012345") (read "67890123456789012")
--
--         liftDiscord $ void $ restCall $ R.CreateMessage textChannel "Joined!"
--         liftIO $ threadDelay 5e6
--
--         res <- createYoutubeResource "Sodium Hydroxide" Nothing
--         play res UnknownCodec
--
--         liftDiscord $ void $ restCall $ R.CreateMessage textChannel "Finished!"
-- @
--
liftDiscord :: DiscordHandler a -> Voice a
liftDiscord = Voice . lift

-- | Executes the voice actions stored in the form of the Voice monad.
--
-- ## Basic Usage
--
-- The following demonstrates joining a voice call and instantly leaving it.
--
-- @
-- runVoice $ join (read "<guildID>") (read "<vcID>")
-- @
--
-- ## Control Flow
--
-- The voice monad cleans up after execution and thus you cannot linger in the
-- voice chat unless you explicitly prevent exiting from the computation. The
-- following demonstrates joining a voice call and never leaving it.
--
-- @
-- runVoice $ do
--     join (read "<guildID>") (read "<vcID>")
--     forever $ pure ()
-- @
--
-- This might seem unintuitive at first. You might ask, "How would I return
-- control flow to my program!? I want to control audio or do something else, not
-- be stuck in the 'runVoice' function!". Our approach makes it impossible for
-- your bot to be in a state that you forgot about, and makes it easy to reason
-- about your voice call. So in fact, the solution to do other things during
-- voice calls is not to return control flow, but to use multiple threads and
-- communicate. Threads are cheap in Haskell, and Discord-Haskell already spawns
-- a new thread for each event. You can let one thread forever stay in
-- 'runVoice', while other threads communicate with it using 'MVar', for example.
-- To leave a voice call from another thread, use the return value of 'join',
-- which you might choose to communicate using 'MVar' too.
--
-- The following demonstrates joining a voice call and never leaving it, but
-- making the leave action available in an STM Map container, which can be
-- accessed and invoked from another event handler thread.
--
-- @
-- mapping <- StmContainers.Map.newIO
-- let guildId = (read "123456789012345")
-- runVoice $ do
--     leave <- join guildId (read "67890123456789012")
--     liftDiscord $ atomically $ M.insert leave (show guildId) mapping
--     forever $ pure ()
-- @
--
-- See the BasicMusicBot example program provided together with this library
-- (or on the GitHub repository for this library) for a music bot that does
-- exactly the approach described above.
--
-- ## Broadcasting
--
-- Within a single use of 'runVoice', the same packets are sent to all
-- connections that have been joined. This enables multi-channel broadcasting.
-- The following demonstrates how a single playback is streamed to multiple
-- connections.
--
-- @
-- runVoice $ do
--     join (read "123456789012345") (read "67890123456789012")
--     join (read "098765432123456") (read "12345698765456709")
--     res <- createYoutubeResource "https://www.youtube.com/watch?v=dQw4w9WgXcQ" Nothing
--     play res UnknownCodec
-- @
--
-- ## Possible Exceptions
--
-- This function may propagate and throw an 'IOException' if 'createProcess'
-- fails for e.g. ffmpeg or yt-dlp, or if any file- or network-related
-- errors occur.
runVoice :: Voice () -> DiscordHandler ()
runVoice action = do
    voiceHandles <- UnliftIO.newMVar []
    mutEx <- UnliftIO.newMVar ()

    let initialState = DiscordBroadcastHandle voiceHandles mutEx

    result <- flip runReaderT initialState $ unVoice $ action

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
-- need to specify the Guild ID, but it needs to be provided manually until
-- discord-haskell fully caches the mappings internally and is able to look up
-- the Guild for any Channel ID.
--
-- ## Basic Usage
--
-- @
-- runVoice $ do
--    join (read "123456789012345") (read "67890123456789012")
-- @
--
-- Note that the above on its own is not useful in practice, since the runVoice
-- cleanup will make the bot leave the voice call immediately after joining.
--
-- ## Leaving the channel
--
-- This function returns a Voice action that, when executed, will leave the
-- joined voice channel. For example:
--
-- @
-- runVoice $ do
--     leave <- join (read "123456789012345") (read "67890123456789012")
--     let res = createFileResource "myfile.mp3" Nothing
--     play res UnknownCodec
--     leave
-- @
--
-- Calling leave at the end of runVoice is not needed in practice, since
-- @runVoice@ will perform the appropriate cleanup and leaving as necessary at
-- the end of all actions. However, it may be useful to interleave @leave@ with
-- other Voice actions, or escape it from the monad using MVars or STMs to be
-- called from other threads.
--
-- The @leave@ function is idempotent and will do nothing if the voice
-- connection is already severed, meaning it is safe to call this from other
-- contexts. The following demonstrates joining a voice channel and playing a
-- file on repeat forever, until a @\/leave@ command is received at which point
-- the connection is severed and the playback thread is killed gracefully.
--
-- @
-- -- On \/play
-- runVoice $ do
--   leave <- join (read "123456789012345") (read "67890123456789012")
--   liftIO $ putMVar futureLeaveFunc leave
--   forever $
--     play (createFileResource "myfile.ogg" Nothing) OpusCodec
--
-- -- On \/leave, from a different thread
-- leave <- liftIO $ takeMVar futureLeaveFunc
-- runVoice leave
-- @
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
            liftIO $ throwIO VoiceNotAvailable
        Just (_, _, _, Nothing) -> do
            -- If endpoint is null, according to Docs, no servers are available.
            liftIO $ throwIO NoServerAvailable
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

-- | @updateSpeakingStatus@ updates the speaking indicator for the bot. Setting
-- the indicator to True is required for Discord to transmit the bot's voice to
-- other clients. However, we call this automatically as part of the 'play'
-- function, so there should be no use for this function in practice.
--
-- === __Dev Note:__
--
-- Note: Soundshare and priority are const as False in the payload because I
-- don't see bots needing them. If there are users who require this, this
-- function will gain additional arguments to control those fields.
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

-- | @updateStatusVoice@ sends a Gateway uplink Opcode 4 Update Voice State.
-- This is used to indicate the client voice status (deaf/mute) as well as the
-- channel they are active on (or Nothing if they are leaving).
--
-- This function is not in the Voice monad because it's purely an operation on
-- the regular websocket. It even has to be used after all voice actions end to
-- move the bot out of voice channels, so there's no benefit to the extra
-- transformer wrapping.
updateStatusVoice
    :: GuildId
    -- ^ Guild ID
    -> Maybe ChannelId
    -- ^ Voice channel ID that we want to join (Nothing if disconnecting)
    -> Bool
    -- ^ Whether we want to be muted
    -> Bool
    -- ^ Whether we want to be deafened
    -> DiscordHandler ()
updateStatusVoice a b c d = sendCommand $ UpdateStatusVoice $ UpdateStatusVoiceOpts a b c d

-- | @probeCodec resource@ tries to use @ffprobe@ to probe the codec of the
-- audio resource for files/URLs. If the probed format was Opus or 16-bit PCM,
-- then the function will return the appropriate 'AudioCodec', so that it can be
-- used to tell we won't need FFmpeg is needed as a transcoder. For any other
-- codec or if the resource is raw bytes, the function will return 'UnknownCodec'.
probeCodec :: String -> AudioResource -> IO AudioCodec
probeCodec ffprobeBinary resource = do
    case resource ^. stream of
        Right lbs -> pure UnknownCodec
        Left source -> do
            let processConfig = proc ffprobeBinary
                    [ source
                    , "-print_format", "json"
                    , "-v", "quiet"
                    , "-show_entries", "stream=codec_name"
                    , "-select_streams", "0"
                    ]
            (_exitCode, stdout, _stderr) <- liftIO $ readProcess processConfig

            let codec = do
                    -- Chain the Maybe monad
                    result <- decode stdout
                    flip parseMaybe result $ \obj -> do
                        streams <- obj .: "streams"
                        head streams .: "codec_name"

            case codec :: Maybe String of
                Just "opus" -> pure OPUSCodec
                Just "pcm_s16le" -> pure PCMCodec
                _ -> pure UnknownCodec


-- | @getPipeline resource codec@ returns some information about the pipeline
-- required for the audio before it is sent to Discord. See the 'AudioPipeline'
-- datatype for the flags set.
--
-- The audio resource pipeline can be complex. Even if the source is already
-- Opus encoded (and Discord wants Opus), if there is an FFmpeg filter desired
-- then discord-haskell-voice will need to launch a FFmpeg subprocess. If there
-- is also a ByteString conduit filter also desired, then the FFmpeg subprocess
-- will need to output PCM ByteString, which is then transcoded to Opus within
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
getPipeline (AudioResource _ _ (Just (_ :.->: _))) _ = AudioPipeline
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
-- to skip transcoding if it's either PCM or OPUS, then use 'ProbeCodec' and
-- specify the name/location of the @ffprobe@ binary.
--
-- ## Basic Usage
--
-- The following unconditionally calls out to ffmpeg to transcode to Opus:
--
-- @
-- res <- createYoutubeResource "i love trains" Nothing
-- play res UnknownCodec
-- @
--
-- The following checks if the resource is actually Opus (which actually a lot
-- of YouTube audio streams are), otherwise calls out to ffmpeg to transcode:
--
-- @
-- res <- createYoutubeResource "i love trains" Nothing
-- play res (ProbeCodec "ffprobe")
-- @
--
-- The following calls out to ffmpeg to transcode to 16-bit PCM (despite being
-- Opus already), because it needs to use the provided conduit to process each
-- PCM sample. Afterwards, the bytestream is encoded to Opus from within Haskell
-- using @libopus@ bindings
--
-- @
-- let res = createFileResource "myfile.ogg" $ HaskellTransformation myConduit
-- play res OpusCodec
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
                Just (_ :.->: tsC) -> tsC .| encodeOpusC .| sinkHandles handles
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
        -- Get all the arguments for FFmpeg using the generator function, either
        -- provided or a default one that only consists of -i.
        let inputArgs = case resource ^. transform of
                Just (FFmpegTransformation argsFunc) -> argsFunc iFlag
                Just (argsFunc :.->: _) -> argsFunc iFlag
                _ -> defaultFfmpegArgs iFlag
        let outputFlags = case pipeline ^. outputCodec of
                -- OPUS output from FFmpeg will be in an OGG container.
                OPUSFinalOutput -> ["-f", "opus", "-c:a", "libopus", "-b:a", "128K", "-ar", "48000", "-ac", "2"]
                PCMFinalOutput -> ["-f", "s16le", "-c:a", "pcm_s16le", "-ar", "48000", "-ac", "2"]
        let args = inputArgs <> outputFlags <>
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
                -- Try to write to the channel. It can fail if it's full, in
                -- which case we just want to wait and keep writing. But if the
                -- consuming thread has been killed (by e.g. leaving VC), then
                -- writeChan'll throw an async MVar deadlock exception, which
                -- we'll check for to avoid looping any more.
                tryC (liftIO $ Bounded.writeChan chan bs) >>= \case
                    Left BlockedIndefinitelyOnMVar -> pure ()
                    _ -> sinkChan chan

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

    -- 48kHz means that 1s has 48,000 samples (or 96,000 for stereo). Therefore,
    -- 1ms has 48 samples. An opus frame is 20ms (by default), so it has
    -- 48 * 20 samples (or 48 * 20 * 2 for stereo).
    -- Each sample has 2 bytes of precision (16 bits), so the size of a frame
    -- is 48 * 20 * 2 (or double for stereo). libopus, per its docs, wants a
    -- per-channel frame size in the unit of "number of samples", so we use
    -- 48 * 20 as the input.
    --
    -- For the output buffer size, 1275 is the max bytes an opus 20ms frame can
    -- have, + the largest possible Opus packet header size is 7 bytes.
    streamCfg = mkStreamConfig enCfg (48 * 20) (1275 + 7)
    loop encoder = await >>= \case
        Nothing -> do
            -- Send at least 5 blank frames before stopping.
            -- Per Discord docs: "When there's a break in the sent data, the
            -- packet transmission shouldn't simply stop. Instead, send five
            -- frames of silence (0xF8, 0xFF, 0xFE) before stopping to avoid
            -- unintended Opus interpolation with subsequent transmissions."
            let encoded = B.pack [0xF8, 0xFF, 0xFE]
            forM_ [0..4] $ \_ -> do
                yield encoded
        Just frame -> do
            -- encode the audio
            encoded <- liftIO $ opusEncode encoder streamCfg frame
            -- send it
            yield encoded
            loop encoder

-- | @createYoutubeResource query mbTransform@ creates an audio resource from a
-- YouTube query. The query is passed to @yt-dlp@, which returns the best audio
-- stream available. If you specify a URL as the query, naturally it will
-- choose the search result for that, which is (likely) guaranteed to be the
-- video itself. The optional 'AudioTransformation' is applied to the audio
-- stream before it is sent to Discord.
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

-- | @createFileResource path mbTransform@ creates an audio resource from a file
-- path. The optional 'AudioTransformation' is applied to the audio stream before
-- it is sent to Discord.
createFileResource :: String -> Maybe AudioTransformation -> AudioResource
createFileResource path mbTransform = AudioResource
    { audioResourceStream = Left path
    , audioResourceYouTubeDLInfo = Nothing
    , audioResourceTransform = mbTransform
    }

-- | @createPCMResource bs mbTransform@ creates an audio resource from a
-- lazy ByteString. The optional 'AudioTransformation' is applied to the audio
-- stream before it is sent to Discord.
createPCMResource :: BL.ByteString -> Maybe AudioTransformation -> AudioResource
createPCMResource bs mbTransform = AudioResource
    { audioResourceStream = Right bs
    , audioResourceYouTubeDLInfo = Nothing
    , audioResourceTransform = mbTransform
    }

-- | @defaultFfmpegArgs file@ is a generator function that returns the default
-- arguments for FFmpeg (excluding error handling and output file specification).
-- Specifically, it is only §-i file@.
-- You may write a replacement argument generator function if you need to
-- specify custom complex_filters or other FFmpeg arguments.
defaultFfmpegArgs :: FilePath -> [String]
defaultFfmpegArgs file = [ "-i", file ]
