{-# LANGUAGE ImportQualifiedPost #-}
module Discord.Internal.Voice.WebsocketLoop where

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
    , tryReadMVar
    , newEmptyMVar
    , tryTakeMVar
    , ThreadId
    , myThreadId, modifyMVar_, newMVar, readMVar
    )
import Control.Exception.Safe ( try, SomeException, finally, handle )
import Control.Lens
import Control.Monad ( forever )
import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( encode, eitherDecode )
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Word ( Word16 )
import Network.WebSockets
    ( ConnectionException(..)
    , Connection
    , sendClose
    , receiveData
    , sendTextData
    )
import Wuss ( runSecureClient )
import Discord.Internal.Gateway ( GatewayException )
import Discord
import Discord.Internal.Types ( GuildId, UserId, User(..), Event(..) )
import Discord.Internal.Types.VoiceCommon
import Discord.Internal.Types.VoiceWebsocket


data WSState
    = WSStart
    | WSClosed
    | WSResume
    deriving Show

-- | A custom logging function that writes the date/time and the thread ID.
(✍) :: Chan T.Text -> T.Text -> IO ()
logChan ✍ log = do
    t <- getCurrentTime
    tid <- myThreadId
    writeChan logChan $ (T.pack $ show t) <> " " <> (T.pack $ show tid) <> " " <> log

-- | A variant of (✍) that prepends the wsError text.
(✍!) :: Chan T.Text -> T.Text -> IO ()
logChan ✍! log = logChan ✍ (wsError log)

wsError :: T.Text -> T.Text
wsError t = "Voice Websocket error - " <> t

connect :: T.Text -> (Connection -> IO a) -> IO a
connect endpoint = runSecureClient url port "/"
  where
    url = (T.unpack . T.takeWhile (/= ':')) endpoint
    port = (read . T.unpack . T.takeWhileEnd (/= ':')) endpoint

-- | Attempt to connect (and reconnect on disconnects) to the voice websocket.
-- Also launches the UDP thread after the initialisation.
launchWebsocket :: WebsocketLaunchOpts -> Chan T.Text -> DiscordHandler ()
launchWebsocket opts log = do
    uid <- getCacheUserId
    udpOpts <- liftIO $ newMVar undefined
    liftIO $ websocketFsm WSStart 0 udpOpts uid
  where
    -- | Get the user ID of the bot from the cache.
    getCacheUserId :: DiscordHandler UserId
    getCacheUserId = userId . cacheCurrentUser <$> readCache

    websocketFsm :: WSState -> Int -> MVar ReadyPayload -> UserId -> IO ()
    -- Websocket closed legitimately. The UDP thread and this thread
    -- will be closed by the cleanup in runVoice.
    websocketFsm WSClosed retries udpInfo uid = pure ()

    -- First time. Let's open a Websocket connection to the Voice
    -- Gateway, do the initial Websocket handshake routine, then
    -- ask to open the UDP connection.
    -- When creating the UDP thread, we will fill in the MVars in
    -- @opts@ to report back to runVoice, so it can be killed in the
    -- future.
    websocketFsm WSStart retries udpInfo uid = do
        next <- try $ connect (opts ^. endpoint) $ \conn -> do
            -- Send opcode 0 Identify
            sendTextData conn $ encode $ Identify $ IdentifyPayload
                { identifyPayloadServerId = (opts ^. guildId)
                , identifyPayloadUserId = uid
                , identifyPayloadSessionId = (opts ^. sessionId)
                , identifyPayloadToken = (opts ^. token)
                }
            -- Attempt to get opcode 2 Ready and Opcode 8 Hello in an
            -- undefined order.
            result <- doOrTimeout 10 $ waitForHelloReady conn Nothing Nothing
            case result of
                Nothing -> do
                    (✍!) log $ "did not receive a valid Opcode 2 and 8 "
                        <> "after connection within 10 seconds"
                    pure WSClosed
                Just (Left e) -> do
                    (✍!) log $ "network error occurred while waiting for " <>
                        "Opcode 2 and 8: " <> (T.pack $ show e)
                    pure WSClosed
                Just (Right (interval, payload)) -> do
                    -- Websocket responded with a valid Opcode 2 Ready, which
                    -- contains the UDP connection info that we write to the
                    -- MVar, so we can keep the info if we loop and resume.
                    modifyMVar_ udpInfo (pure . const payload)
                    startEternalStream (WebsocketConn conn opts payload) interval log

        -- Connection is now closed.
        case next :: Either SomeException WSState of
            Left e -> do
                (✍!) log $ "could not connect due to an exception: " <>
                    (T.pack $ show e)
                writeChan (opts ^. wsHandle . _1) $ Left $
                    VoiceWebsocketCouldNotConnect
                        "could not connect due to an exception"
                websocketFsm WSClosed 0 udpInfo uid
            Right n -> websocketFsm n 0 udpInfo uid

    websocketFsm WSResume retries udpInfo uid = do
        next <- try $ connect (opts ^. endpoint) $ \conn -> do
            -- Send opcode 7 Resume
            sendTextData conn $ encode $
                Resume (opts ^. guildId) (opts ^. sessionId) (opts ^. token)
            -- Attempt to get opcode 9 Resumed and Opcode 8 Hello in an
            -- undefined order
            result <- doOrTimeout 10 $ waitForHelloResumed conn Nothing False
            case result of
                Nothing -> do
                    (✍!) log $ "did not receive a valid Opcode 9 and 8 " <>
                        "after reconnection within 10 seconds, restarting " <>
                        "connection"
                    pure WSStart
                Just (Left e) -> do
                    (✍!) log $ "network error occurred while waiting for " <>
                        "Opcode 9 and 8: " <> (T.pack $ show e)
                    pure WSStart
                Just (Right interval) -> do
                    -- use the previous UDP launch options since it's not resent
                    readyPayload <- readMVar udpInfo
                    startEternalStream (WebsocketConn conn opts readyPayload) interval log

        -- Connection is now closed.
        case next :: Either SomeException WSState of
            Left _ -> do
                (✍!) log $ "could not resume, retrying after 10 seconds"
                threadDelay $ 10 * (10^(6 :: Int))
                websocketFsm WSResume (retries + 1) udpInfo uid
            Right n -> websocketFsm n 1 udpInfo uid

    
    -- | Perform an IO action for a maximum of @sec@ seconds.
    doOrTimeout :: Int -> IO a -> IO (Maybe a)
    doOrTimeout sec longAction = (^? _Right) <$> race waitSecs longAction
      where
        waitSecs :: IO (Maybe b)
        waitSecs = threadDelay (sec * 10^(6 :: Int)) >> pure Nothing

    -- | Wait for both Opcode 2 Ready and Opcode 8 Hello, and return both
    -- responses in a Maybe (so that the type signature matches wait10seconds)
    waitForHelloReady
        :: Connection
        -> Maybe Int
        -> Maybe ReadyPayload
        -> IO (Either ConnectionException (Int, ReadyPayload))
    waitForHelloReady conn (Just x) (Just y) = pure $ Right (x, y)
    waitForHelloReady conn mb1 mb2 = do
        msg <- getPayload conn log
        print msg -- TODO: debug, remove.
        case msg of
            Right (Discord.Internal.Types.VoiceWebsocket.Ready payload) ->
                waitForHelloReady conn mb1 (Just payload)
            Right (Discord.Internal.Types.VoiceWebsocket.Hello interval) ->
                waitForHelloReady conn (Just interval) mb2
            Right _ ->
                waitForHelloReady conn mb1 mb2
            Left e ->
                pure $ Left e

    -- | Wait for both Opcode 9 Resumed and Opcode 8 Hello, and return the
    -- Hello interval. There is no body in Resumed.
    waitForHelloResumed
        :: Connection
        -> Maybe Int
        -> Bool
        -> IO (Either ConnectionException Int)
    waitForHelloResumed conn (Just x) True = pure $ Right x
    waitForHelloResumed conn mb1 bool = do
        msg <- getPayload conn log
        case msg of
            Right (Discord.Internal.Types.VoiceWebsocket.Hello interval) ->
                waitForHelloResumed conn (Just interval) bool
            Right Discord.Internal.Types.VoiceWebsocket.Resumed ->
                waitForHelloResumed conn mb1 True
            Right _ ->
                waitForHelloResumed conn mb1 bool
            Left e ->
                pure $ Left e

    -- userId :: IO UserId
    -- userId = (lift . lift) getCacheUserId
    -- log <- discordHandleLog <$> (lift . lift) ask

getPayload
    :: Connection
    -> Chan T.Text
    -> IO (Either ConnectionException VoiceWebsocketReceivable)
getPayload conn log = try $ do
    msg' <- receiveData conn
    case eitherDecode msg' of
        Right msg -> pure msg
        Left err  -> do
            (✍) log $ "Voice Websocket parse error - " <> T.pack err
                <> " while decoding " <> TE.decodeUtf8 (BL.toStrict msg')
            pure $ ParseError $ T.pack err

getPayloadTimeout
    :: Connection
    -> Int
    -> Chan T.Text
    -> IO (Either ConnectionException VoiceWebsocketReceivable)
getPayloadTimeout conn interval log = do
  res <- race (threadDelay ((interval * 1000 * 3) `div` 2))
              (getPayload conn log)
  case res of
    Left () -> pure (Right Reconnect)
    Right other -> pure other

                        -- (opts ^. gatewayEvents) interval (opts ^. wsHandle . _2) log

-- | Fork and start the sendable and heartbeat loops, then once we've established
-- and stabilised the Websocket connection, continue into the UDP creation and
-- handshaking procedure.
startEternalStream
    :: WebsocketConn
    -- ^ Websocket connection, its launch options, and UDP launch options
    -> Int
    -- ^ Heartbeat interval specified by initially received Ready or Resume
    -> Chan T.Text
    -- ^ Log channel
    -> IO WSState
startEternalStream wsconn interval log = undefined
    -- sysSends <- newChan -- Chan for Heartbeat
    -- sendLoopId <- forkIO $ sendableLoop wsconn sysSends sends
    -- heartLoopId <- forkIO $ heartbeatLoop wsconn sysSends interval log
    -- gatewayReconnected <- newEmptyMVar
    -- gatewayCheckerId <- forkIO $ gatewayCheckerLoop gatewayEvents gatewayReconnected log

    -- finally (eventStream wsconn gatewayReconnected interval sysSends log) $
    --     (killThread heartLoopId >> killThread sendLoopId >> killThread gatewayCheckerId)

-- | Eternally stay on lookout for the connection. Writes to receivables channel.
eventStream
    :: WebsocketConn
    -> MVar ()
    -- ^ flag with () if gateway has reconnected
    -> Int
    -> Chan VoiceWebsocketSendable
    -> Chan T.Text
    -> IO WSState
eventStream wsconn gatewayReconnected interval sysSends log = do
    sem <- tryReadMVar gatewayReconnected
    case sem of
        Just () -> do
            log ✍! "gateway reconnected, doing same for voice."
            sendClose (wsconn ^. connection) $ T.pack "Hey Discord, we're reconnecting in a bit."
            pure WSResume
        Nothing -> do
            eitherPayload <- getPayloadTimeout (wsconn ^. connection) interval log
            putStrLn $ "<-- " <> show eitherPayload
            case eitherPayload of
                -- Network-WebSockets, type ConnectionException
                Left (CloseRequest code str) -> do
                    handleClose code str
                Left _ -> do
                    log ✍! "connection exception in eventStream."
                    pure WSResume
                Right Reconnect -> do
                    log ✍! "connection timed out, trying to reconnect again."
                    pure WSResume
                Right (HeartbeatAckR _) ->
                    -- discord docs says HeartbeatAck is sent (opcode 6) after every
                    -- Heartbeat (3) that I send. However, this doesn't seem to be
                    -- the case, as discord responds with another Heartbeat (3) to
                    -- my own, and Ack is never sent back.
                    -- I am required to send back an Ack from MY side in response to
                    -- the Heartbeat that they send which is in response to the
                    -- heartbeat that I send. wtf?
                    eventStream wsconn gatewayReconnected interval sysSends log
                Right (HeartbeatR a) -> do
                    writeChan sysSends $ HeartbeatAck a
                    eventStream wsconn gatewayReconnected interval sysSends log
                Right receivable -> do
                    writeChan (wsconn ^. launchOpts . wsHandle . _1) (Right receivable)
                    eventStream wsconn gatewayReconnected interval sysSends log

  where
    -- | Handle Websocket Close codes by logging appropriate messages and
    -- closing the connection.
    handleClose :: Word16 -> BL.ByteString -> IO WSState
    handleClose code str = do
        let reason = TE.decodeUtf8 $ BL.toStrict str
        case code of
            -- from discord.py voice_client.py#L421
            1000 -> do
                -- Normal close
                log ✍! "websocket closed normally."
                pure WSClosed
            4001 -> do
                -- Unknown opcode
                log ✍! "websocket closed due to unknown opcode"
                pure WSClosed
            4014 -> do
                -- VC deleted, main gateway closed, or bot kicked. Do not resume.
                -- Instead, restart from zero.
                log ✍! "vc deleted or bot forcefully disconnected... Restarting gateway"
                pure WSStart
            4015 -> do
                -- "The server crashed. Our bad! Try resuming."
                pure WSResume
            x    -> do
                (✍!) log $ "connection closed with code: [" <> T.pack (show code) <>
                        "] " <> reason
                pure WSClosed


-- | Eternally send data from sysSends and usrSends channels
sendableLoop
    :: WebsocketConn
    -> Chan VoiceWebsocketSendable
    -> Chan VoiceWebsocketSendable
    -> IO ()
sendableLoop wsconn sysSends usrSends = do
    -- Wait-time taken from discord-haskell/Internal.Gateway.EventLoop
    threadDelay $ round ((10^(6 :: Int)) * (62 / 120) :: Double)
    -- Get whichever possible, and send it
    payload <- either id id <$> race (readChan sysSends) (readChan usrSends)
    print $ "--> " <> show payload
    sendTextData (wsconn ^. connection) $ encode payload
    sendableLoop wsconn sysSends usrSends

-- | Eternally send heartbeats through the sysSends channel
heartbeatLoop
    :: WebsocketConn
    -> Chan VoiceWebsocketSendable
    -> Int
    -- ^ milliseconds
    -> Chan T.Text
    -> IO ()
heartbeatLoop wsconn sysSends interval log = do
    threadDelay $ 1 * 10^(6 :: Int)
    forever $ do
        time <- round <$> getPOSIXTime
        writeChan sysSends $ Heartbeat $ time
        threadDelay $ interval * 1000

gatewayCheckerLoop
    :: Chan (Either GatewayException Event)
    -- ^ Gateway events
    -> MVar ()
    -- ^ Binary empty semaphore, set to () when gateway has reconnected
    -> Chan T.Text
    -- ^ log
    -> IO ()
gatewayCheckerLoop gatewayEvents sem log = do
    top <- readChan gatewayEvents
    print top
    case top of
        Right (Discord.Internal.Types.Ready _ _ _ _ _) -> do
            log ✍ "gateway ready detected, putting () in sem"
            putMVar sem ()
            gatewayCheckerLoop gatewayEvents sem log
        _ -> gatewayCheckerLoop gatewayEvents sem log
