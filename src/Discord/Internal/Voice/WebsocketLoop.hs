{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : Discord.Internal.Voice.WebsocketLoop
Description : Strictly for internal use only. See Discord.Voice for the public interface.
Copyright   : (c) 2021-2022 Yuto Takano
              (c) 2025-PRESENT discord-haskell-voice Contributors
License     : MIT
Maintainer  : Yuto Takano <moa17stock@gmail.com>

= WARNING

This module is considered __internal__.

The Package Versioning Policy __does not apply__.

The contents of this module may change __in any way whatsoever__ and __without__
__any warning__ between minor versions of this package, unless the identifier is
re-exported from a non-internal module.

= Description

This module provides 'launchWebsocket', a function used to start a websocket
connection to the Discord voice gateway (i.e. the voice control-plane), and
perform necessary setup including launching a heartbeat thread, selecting
encryption modes, and initiating IP Discovery. The function will also set up
the UDP socket (i.e. the voice data-plane) for voice data transmission by
calling 'launchUDP'.
-}
module Discord.Internal.Voice.WebsocketLoop
    ( launchWebsocket
    ) where

import Control.Concurrent.Async ( race )
import Control.Concurrent
    ( Chan
    , newChan
    , writeChan
    , readChan
    , threadDelay
    , forkIO
    , killThread
    , MVar
    , putMVar
    , newEmptyMVar
    , ThreadId
    , myThreadId
    , mkWeakThreadId
    , modifyMVar_
    , newMVar
    , readMVar
    )
import Control.Exception.Safe ( try, SomeException, finally )
import Lens.Micro
import Control.Monad ( forever, guard )
import Control.Monad.Except ( runExceptT, ExceptT (ExceptT) )
import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( encode, eitherDecode )
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX
import Data.Time
import Data.Word ( Word16 )
import Network.WebSockets
    ( ConnectionException(..)
    , Connection
    , receiveData
    , sendTextData
    )
import Wuss ( runSecureClient )

import Discord.Internal.Types.VoiceCommon
import Discord.Internal.Types.VoiceWebsocket
import Discord.Internal.Types.VoiceUDP
import Discord.Internal.Voice.CommonUtils
import Discord.Internal.Voice.UDPLoop

-- | States of the Websocket thread state machine.
data WSState
    = WSStart
    | WSClosed
    | WSResume
    deriving stock Show

-- | A custom logging function that writes the date/time and the thread ID.
(✍) :: Chan T.Text -> T.Text -> IO ()
logChan ✍ log = do
    t <- formatTime defaultTimeLocale "%F %T %q" <$> getCurrentTime
    tid <- myThreadId
    writeChan logChan $ (T.pack t) <> " " <> (tshow tid) <> " " <> log

-- | A variant of (✍) that prepends the wsError text.
(✍!) :: Chan T.Text -> T.Text -> IO ()
logChan ✍! log = logChan ✍ ("!!! Voice Websocket Error - " <> log)

-- | @connect@ is an alias for running a websocket connection using the Discord
-- endpoint URL (which contains the port as well). It makes sure to connect to
-- the correct voice gateway version (v4) as well, as the default version of 1
-- is severely out of date (the opcode behaviours are not according to docs).
connect :: T.Text -> (Connection -> IO a) -> IO a
connect endpoint = runSecureClient url port "/?v=4"
  where
    url = (T.unpack . T.takeWhile (/= ':')) endpoint
    port = (read . T.unpack . T.takeWhileEnd (/= ':')) endpoint

-- | Attempt to connect (and reconnect on disconnects) to the voice websocket.
-- Also launches the UDP thread after the initialisation.
launchWebsocket :: WebsocketLaunchOpts -> Chan T.Text -> IO ()
launchWebsocket opts log = do
    -- Keep an MVar (only for use in this function), to store the UDP launch
    -- options across Resume events.
    udpOpts <- newMVar undefined
    websocketFsm WSStart 0 udpOpts
  where
    websocketFsm :: WSState -> Int -> MVar UDPLaunchOpts -> IO ()
    -- Websocket closed legitimately. The UDP thread and this thread
    -- will be closed by the cleanup in 'runVoice'.
    websocketFsm WSClosed _retries _udpInfo = pure ()

    -- First time. Let's open a Websocket connection to the Voice gateway, do
    -- the initial Websocket handshake routine, then ask to open the UDP data
    -- plane connection. When creating the UDP thread, we will fill in the MVars
    -- in @opts@ to report back to 'runVoice', so it can be killed from the main
    -- thread if necessary (e.g. on leaving the call).
    websocketFsm WSStart _retries udpInfo = do
        next <- try $ connect (opts ^. endpoint) $ \conn -> do
            (libSends, sendTid) <- flip (setupSendLoop conn) log $ opts ^. wsHandle . _2

            result <- flip finally (killThread sendTid) $ runExceptT $ do
                helloPacket <- ExceptT $
                    over _Left ((<> "Failed to get Opcode 8 Hello: ") . tshow) <$>
                    getPayload conn

                interval <- ExceptT $ pure $
                    maybeToRight ("First packet not Opcode 8 Hello: " <> tshow helloPacket) $
                        helloPacket ^? _Hello

                -- Create a thread to add heartbeating packets to the
                -- libSends Chan.
                heartGenTid <- liftIO $ forkIO $ heartbeatLoop libSends interval log

                flip finally (liftIO $ killThread heartGenTid) $ do
                    -- Perform the Identify/Ready handshake
                    readyPacket <- ExceptT $
                        over _Left ((<> "Failed to get Opcode 2 Ready: ") . tshow) <$>
                        performIdentification conn opts

                    p <- ExceptT $ pure $
                        maybeToRight ("First packet after Identify not " <> "Opcode 2 Ready " <> tshow readyPacket) $
                            readyPacket ^? _Ready

                    secretKey <- liftIO $ newEmptyMVar
                    let udpLaunchOpts = UDPLaunchOpts
                            { uDPLaunchOptsSsrc      = readyPayloadSSRC p
                            , uDPLaunchOptsIp        = readyPayloadIP p
                            , uDPLaunchOptsPort      = readyPayloadPort p
                            , uDPLaunchOptsMode      = "xsalsa20_poly1305"
                            , uDPLaunchOptsUdpHandle = opts ^. udpHandle
                            , uDPLaunchOptsSecretKey = secretKey
                            -- TODO: support all encryption modes
                            }
                    -- We should be putting SSRC into the MVar to report back to
                    -- the websocket (TODO: why was this again), but we hold it off
                    -- until the ssrcCheck guard a few lines below.
                    liftIO $ modifyMVar_ udpInfo (pure . const udpLaunchOpts)

                    -- Launch the UDP thread, automatically perform 
                    -- IP discovery, which will write the result
                    -- to the receiving Chan. We will pass not the MVar but
                    -- the raw options, since there's no writing to be done.

                    forkedId <- liftIO $ forkIO $ launchUdp udpLaunchOpts log
                    flip finally (liftIO $ killThread forkedId) $ do
                        udpTidWeak <- liftIO $ mkWeakThreadId forkedId
                        liftIO $ putMVar (opts ^. udpTid) udpTidWeak

                        ipDiscovery <- liftIO $ readChan $ opts ^. udpHandle . _1
                        (ssrcCheck, ip, port) <- ExceptT $ pure $
                            maybeToRight ("First UDP Packet not IP Discovery " <> tshow ipDiscovery) $
                                ipDiscovery ^? _IPDiscovery

                        guard (ssrcCheck == udpLaunchOpts ^. ssrc)
                        liftIO $ putMVar (opts ^. ssrc) ssrcCheck

                        -- TODO: currently, we await the Opcode 4 SD right after
                        -- Select Protocol, blocking the start of heartbeats until
                        -- eventStream. This means there's a delay, so TODO to check
                        -- if this delay causes any problems. If it does, keep the
                        -- sending here, but receive the SD event in eventStream.
                        sessionDescPacket <- ExceptT $
                            over _Left ((<> "Failed to get Opcode 4 SD: ") . tshow) <$>
                                sendSelectProtocol conn ip port (udpLaunchOpts ^. mode)

                        (modeCheck, key) <- ExceptT $ pure $
                            maybeToRight ("First packet after Select Protocol " <>
                                "not Opcode 4 Session Description " <>
                                tshow readyPacket) $
                                    sessionDescPacket ^? _SessionDescription

                        guard (modeCheck == udpLaunchOpts ^. mode)

                        liftIO $ putMVar secretKey key

                        -- Move to eternal websocket event loop, mainly for the
                        -- heartbeats, but also for any user-generated packets.
                        liftIO $ eventStream conn opts interval udpLaunchOpts libSends log

            case result of
                Left reason -> log ✍! reason >> pure WSClosed
                Right state -> pure state

        -- Connection is now closed.
        case next :: Either SomeException WSState of
            Left e -> do
                (✍!) log $ "connection terminated due to a synchronous exception: " <>
                    (tshow e)
                writeChan (opts ^. wsHandle . _1) $ Left $
                    VoiceWebsocketCouldNotConnect
                        "connection terminated due to a synchronous exception"
                websocketFsm WSClosed 0 udpInfo
            Right n -> websocketFsm n 0 udpInfo

    websocketFsm WSResume retries udpInfo = do
        next <- try $ connect (opts ^. endpoint) $ \conn -> do
            (libSends, sendTid) <- flip (setupSendLoop conn) log $ opts ^. wsHandle . _2
            helloPacket <- getPayload conn
            case helloPacket of
                Left e -> do
                    (✍!) log $ "Failed to get Opcode 8 Hello: " <> (tshow e)
                    pure WSClosed
                Right (Hello interval) -> do
                    -- Create a thread to add heartbeating packets to the
                    -- libSends Chan.
                    heartGenTid <- forkIO $ heartbeatLoop libSends interval log
                    -- Perform the Resume/Resumed handshake
                    resumedPacket <- performResumption conn opts
                    case resumedPacket of
                        Left e -> do
                            (✍!) log $ "Failed to get Opcode 9 Resumed: " <> (tshow e)
                            pure WSClosed
                        Right (Discord.Internal.Types.VoiceWebsocket.Resumed) -> do
                            -- use the previous UDP launch options since it's not resent
                            udpLaunchOpts <- readMVar udpInfo

                            -- Pass not the MVar but the raw options, since
                            -- there's no writing to be done.
                            finally (eventStream conn opts interval udpLaunchOpts libSends log) $
                                (killThread heartGenTid >> killThread sendTid)
                        Right p -> do
                            (✍!) log $ "First packet after Resume not " <>
                                "Opcode 9 Resumed: " <> (tshow p)
                            pure WSClosed
                Right p -> do
                    (✍!) log $ "First packet not Opcode 8 Hello: " <> (tshow p)
                    pure WSClosed

        case next :: Either SomeException WSState of
            Left e -> do
                (✍!) log $ "could not resume due to a synchronous exception: " <>
                    (tshow e) <> ", retrying after 5 seconds"
                threadDelay $ 5 * (10^(6 :: Int))
                websocketFsm WSResume (retries + 1) udpInfo
            Right n -> websocketFsm n retries udpInfo

-- | @setupSendLoop@ takes a 'Chan' of user-generated packets to send in the
-- websocket (such as for 'updateSpeakingStatus'). It forks a thread that runs
-- 'sendableLoop' using the provided Chan, together with a new internal-use-only
-- Chan for e.g. heartbeat packets.
setupSendLoop
    :: Connection
    -- ^ The websocket connection
    -> VoiceWebsocketSendChan
    -- ^ User generated packets to send in the Websocket
    -> Chan T.Text
    -- ^ Logging channel
    -> IO (VoiceWebsocketSendChan, ThreadId)
    -- ^ Chan to send internal-only packets in the Websocket, and the thread
    -- ID of the eternal sending thread (useful for killing it).
setupSendLoop conn userSends log = do
    -- The following Chan will be used for accumulating library-generated
    -- WebSocket messages that we need to send to Discord, mostly for heartbeats.
    libSends <- newChan
    -- Start said eternal sending fork, which will eternally send from library-
    -- generated and user-generated packets.
    sendLoopId <- forkIO $ sendableLoop conn libSends userSends log

    pure (libSends, sendLoopId)

-- | @performIdentification@ sends the voice gateway uplink Opcode 0 Identify
-- packet to Discord, and awaits the voice gateway downlink Opcode 2 Ready,
-- which has the UDP connection info in its payload.
performIdentification
    :: Connection
    -- ^ The websocket connection
    -> WebsocketLaunchOpts
    -- ^ The options used to launch the websocket
    -> IO (Either ConnectionException VoiceWebsocketReceivable)
performIdentification conn opts = do
    -- Send opcode 0 Identify
    sendTextData conn $ encode $ Identify $ IdentifyPayload
        { identifyPayloadServerId = (opts ^. guildId)
        , identifyPayloadUserId = (opts ^. botUserId)
        , identifyPayloadSessionId = (opts ^. sessionId)
        , identifyPayloadToken = (opts ^. token)
        }

    getPayload conn

-- | @performResumption@ sends the voice gateway uplink Opcode 7 Resume packet
-- to Discord, and awaits the voice gateway downlink Opcode 9 Resumed payload.
performResumption
    :: Connection
    -- ^ The websocket connection
    -> WebsocketLaunchOpts
    -- ^ The options used to launch the websocket
    -> IO (Either ConnectionException VoiceWebsocketReceivable)
performResumption conn opts = do
    -- Send opcode 7 Resume
    sendTextData conn $ encode $
        Resume (opts ^. guildId) (opts ^. sessionId) (opts ^. token)

    getPayload conn

-- | @sendSelectProtocol@ sends the voice gateway uplink Opcode 1 Select
-- Protocol to Discord., and waits until we get voice gatway downlink Opcode 4
-- Session Description. We ignore any irrelevant packets during this wait,
-- including downlink Opcode 11 Client Connect, downlink Opcode 18 Client Flags,
-- downlink 20 Client Platform, all of which are sent to us if there is already
-- another user in the voice chat.
sendSelectProtocol
    :: Connection
    -- ^ The websocket connection
    -> T.Text
    -- ^ Our local UDP thread IP as found by IP Discovery
    -> Integer
    -- ^ Our local UDP thread port as found by IP Discovery
    -> T.Text
    -- ^ Selected encryption mode
    -> IO (Either ConnectionException VoiceWebsocketReceivable)
sendSelectProtocol conn ip port mode = do
    sendTextData conn $ encode $ SelectProtocol $ 
        SelectProtocolPayload "udp" ip port mode

    -- Skip payloads until we get the Opcode 4 Session Description.
    waitUntilSessionDescription
  where
    waitUntilSessionDescription :: IO (Either ConnectionException VoiceWebsocketReceivable)
    waitUntilSessionDescription = do
        payload <- getPayload conn
        case payload of
            Left e -> pure $ Left e
            Right s@(SessionDescription _ _) -> pure $ Right s
            Right _ -> waitUntilSessionDescription

-- | @getPayload@ gets one packet from the Websocket 'Connection' using
-- 'receiveData', parsing it into a 'VoiceWebsocketReceivable' using Aeson. If
-- the packet could not be parsed, it will return @Right (ParseError info)@.
getPayload
    :: Connection
    -> IO (Either ConnectionException VoiceWebsocketReceivable)
getPayload conn = try $ do
    msg' <- receiveData conn
    case eitherDecode msg' of
        Right msg -> pure msg
        Left err  -> pure $ ParseError $ T.pack err
            <> " while decoding " <> TE.decodeUtf8 (BL.toStrict msg')

-- | @sendableLoop@ eternally send data from 'libSends' and 'usrSends' channels,
-- whenever either of them have data to send.
sendableLoop
    :: Connection
    -- ^ The websocket connection
    -> VoiceWebsocketSendChan
    -- ^ Internal-use sendable channel, e.g. for heartbeat packets
    -> VoiceWebsocketSendChan
    -- ^ User-generated sendable packets channel
    -> Chan T.Text
    -- ^ Logs
    -> IO ()
sendableLoop conn libSends usrSends log = do
    -- Wait-time taken from discord-haskell/Internal.Gateway.EventLoop
    threadDelay $ round ((10^(6 :: Int)) * (62 / 120) :: Double)
    -- Get whichever possible, and send it
    payload <- either id id <$> race (readChan libSends) (readChan usrSends)
    -- log ✍ ("(send) " <> tshow payload) -- TODO: debug, remove.
    sendTextData conn $ encode payload
    sendableLoop conn libSends usrSends log

-- | @heartbeatLoop@ eternally generates Heartbeat packets every interval, and
-- puts it into the sendable channel to be sent by 'sendableLoop'.
heartbeatLoop
    :: VoiceWebsocketSendChan
    -- ^ The internal-use sendable channel for websocket packets
    -> Int
    -- ^ milliseconds
    -> Chan T.Text
    -- ^ Logs
    -> IO ()
heartbeatLoop libSends interval _log = do
    threadDelay $ 1 * 10^(6 :: Int)
    forever $ do
        time <- round <$> getPOSIXTime
        writeChan libSends $ Heartbeat $ time
        threadDelay $ interval * 1000

-- | @eventStream@ is the main event loop for the Websocket, after all initial
-- handshake stages (Hello and identification/resumption). It will continuously
-- read the top packet in the Websocket receives, and handle closures, and
-- packet responses (like heartbeat responses).
eventStream
    :: Connection
    -- ^ The websocket connection
    -> WebsocketLaunchOpts
    -- ^ Options used to launch the websokcet connection
    -> Int
    -- ^ Interval at which we expect to receive a heartbeat response. We tolerate
    -- up to double of the interval, but if no heartbeat response is received in
    -- this time, we consider the connection to be lost.
    -> UDPLaunchOpts
    -- ^ Options used to launch the UDP data plane
    -> VoiceWebsocketSendChan
    -- ^ The internal-use sendable channel for websocket packets
    -> Chan T.Text
    -- ^ Logs
    -> IO WSState
eventStream conn opts interval udpLaunchOpts libSends log = do
    -- there has to be at least one packet every @interval@ milliseconds (which
    -- is the heartbeat response), so if we don't get that, it's a sign of
    -- the connection gone, we should reconnect. For a quick heuristic accounting
    -- for any network delays, allow for a tolerance of double the time.
    payload <- timeoutMs (interval * 2) $ getPayload conn
    -- log ✍ ("(recv) " <> tshow payload) -- TODO: debug, remove.
    case payload of
        Nothing -> do
            log ✍! "connection timed out, trying to reconnect again."
            pure WSResume
        -- Network-WebSockets, type ConnectionException
        Just (Left (CloseRequest code str)) -> do
            -- Whether we resume or gracefully close depends on the close code,
            -- so offload the decision to the close code handler.
            handleClose code str
        Just (Left _) -> do
            log ✍! "connection exception in eventStream, trying to reconnect."
            pure WSResume
        Just (Right (HeartbeatAck _)) ->
            eventStream conn opts interval udpLaunchOpts libSends log
        Just (Right receivable) -> do
            writeChan (opts ^. wsHandle . _1) (Right receivable)
            eventStream conn opts interval udpLaunchOpts libSends log

  where
    -- | Handle Websocket Close codes by logging appropriate messages and
    -- closing the connection.
    handleClose :: Word16 -> BL.ByteString -> IO WSState
    handleClose 1000 _str = log ✍! "websocket closed normally."
        >> pure WSClosed
    handleClose 4001 _str = log ✍! "websocket closed due to unknown opcode"
        >> pure WSClosed
    handleClose 4014 _str = log ✍! ("vc deleted, main gateway closed, or bot " <>
        "forcefully disconnected... Restarting voice.")
        >> pure WSStart
    handleClose 4015 _str = log ✍! "server crashed on Discord side, resuming"
        >> pure WSResume
    handleClose code str = (✍!) log ("connection closed with code: [" <>
        tshow code <> "] " <> (TE.decodeUtf8 $ BL.toStrict str))
        >> pure WSClosed
