module Discord.Internal.Voice.WebsocketLoop where

import           Control.Concurrent.Async   ( race
                                            )
import           Control.Concurrent         ( Chan
                                            , newChan
                                            , writeChan
                                            , readChan
                                            , threadDelay
                                            , forkIO
                                            , killThread
                                            )
import           Control.Exception.Safe     ( try
                                            , SomeException
                                            , finally
                                            , handle
                                            )
import           Control.Monad              ( forever
                                            )
import           Data.Aeson                 ( encode
                                            , eitherDecode
                                            )
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock.POSIX
import           Data.Word                  ( Word16
                                            )
import           Wuss                       ( runSecureClient )
import           Network.WebSockets         ( ConnectionException(..)
                                            , Connection
                                            , receiveData
                                            , sendTextData
                                            )

import           Discord.Internal.Types     ( GuildId
                                            , UserId
                                            )
import           Discord.Internal.Types.VoiceWebsocket

data VoiceWebsocketException
    = VoiceWebsocketCouldNotConnect T.Text
    | VoiceWebsocketEventParseError T.Text
    | VoiceWebsocketUnexpected VoiceWebsocketReceivable T.Text
    | VoiceWebsocketConnection ConnectionException T.Text
    deriving (Show)

type DiscordVoiceHandleWebsocket
  = ( Chan (Either VoiceWebsocketException VoiceWebsocketReceivable)
    , Chan VoiceWebsocketSendable
    )

-- | session_id, token, guild_id, endpoint
data WebsocketConnInfo = WSConnInfo
    { wsInfoSessionId  :: T.Text
    , wsInfoToken      :: T.Text
    , wsInfoGuildId    :: GuildId
    , wsInfoEndpoint   :: T.Text
    }

data WebsocketConn = WSConn
    { wsDataConnection   :: Connection
    , wsDataConnInfo     :: WebsocketConnInfo
    , wsDataReceivesChan :: Chan (Either VoiceWebsocketException VoiceWebsocketReceivable)
    }

data ConnLoopState
    = ConnStart
    | ConnClosed
    | ConnReconnect Int
      -- ^ Int and not Integer because threadDelay uses it
    deriving Show

wsError :: T.Text -> T.Text
wsError t = "Voice Websocket error - " <> t

connect :: T.Text -> (Connection -> IO a) -> IO a
connect endpoint = runSecureClient url port "/"
  where
    url = (T.unpack . T.takeWhile (/= ':')) endpoint
    port = (read . T.unpack . T.takeWhileEnd (/= ':')) endpoint

-- | Attempt to connect (and reconnect on disconnects) to the voice websocket.
voiceWebsocketLoop
    :: DiscordVoiceHandleWebsocket
    -> (WebsocketConnInfo, UserId)
    -> Chan T.Text
    -> IO ()
voiceWebsocketLoop (receives, sends) (info, userId) log = loop ConnStart 0
  where
    loop :: ConnLoopState -> Int -> IO ()
    loop s retries = do
        case s of
            ConnClosed -> do
                pure ()

            ConnStart -> do
                next <- try $ connect (wsInfoEndpoint info) $ \conn -> do
                    -- Send opcode 0 Identify
                    sendTextData conn $ encode $ Identify $ IdentifyPayload
                        { identifyPayloadServerId = wsInfoGuildId info
                        , identifyPayloadUserId = userId
                        , identifyPayloadSessionId = wsInfoSessionId info
                        , identifyPayloadToken = wsInfoToken info
                        }
                    -- Attempt to get opcode 2 Ready and Opcode 8 Hello in an
                    -- undefined order.
                    result <- waitForHelloReadyOr10Seconds conn
                    case result of
                        Nothing -> do
                            writeChan log $ wsError $
                                "did not receive a valid Opcode 2 and 8 " <>
                                    " after connection within 10 seconds"
                            pure ConnClosed
                        Just (interval, payload) -> do
                            -- All good! Start the heartbeating and send loops.
                            writeChan receives $ Right (Ready payload)
                            startEternalStream (WSConn conn info receives)
                                interval sends log
                -- Connection is now closed.
                case next :: Either SomeException ConnLoopState of
                    Left e -> do
                        writeChan log $ wsError $
                            "could not connect due to an exception: " <>
                                (T.pack $ show e)
                        writeChan receives $ Left $
                            VoiceWebsocketCouldNotConnect
                                "could not connect due to an exception"
                        loop ConnClosed 0
                    Right n -> loop n 0

            ConnReconnect previousInterval -> do
                next <- try $ connect (wsInfoEndpoint info) $ \conn -> do
                    -- Send opcode 7 Resume
                    sendTextData conn $ encode $
                        Resume (wsInfoGuildId info) (wsInfoSessionId info) (wsInfoToken info)
                    -- Attempt to get opcode 9 Resumed
                    msg <- getPayload conn log
                    case msg of
                        Right Resumed -> do
                            startEternalStream
                                (WSConn conn info receives)
                                    previousInterval sends log
                        Right m -> do
                            writeChan log $ wsError
                                "first message after resume should be opcode 9 Resumed"
                            writeChan receives $ Left $
                                VoiceWebsocketUnexpected m
                                    "first message after resume should be opcode 9 Resumed"
                            pure ConnClosed
                        Left m -> do
                            writeChan log $ wsError
                                "unexpected response to resumption"
                            writeChan receives $ Left $
                                VoiceWebsocketConnection m
                                    "unexpected response to resumption"
                            pure ConnClosed
                -- Connection is now closed.
                case next :: Either SomeException ConnLoopState of
                    Left _ -> do
                        writeChan log $ wsError
                            "could not resume, retrying after 10 seconds"
                        threadDelay $ 10 * (10^(6 :: Int))
                        loop (ConnReconnect previousInterval) (retries + 1)
                    Right n -> loop n 1

    -- | Wait for 10 seconds or received Ready and Hello, whichever comes first.
    -- Discord Docs does not specify the order in which Ready and Hello can
    -- arrive, hence the complicated recursive logic and racing.
    waitForHelloReadyOr10Seconds :: Connection -> IO (Maybe (Int, ReadyPayload))
    waitForHelloReadyOr10Seconds conn =
        either id id <$> race wait10Seconds (waitForHelloReady conn Nothing Nothing)

    -- | Wait 10 seconds, this is for fallback when Discord never sends the msgs
    -- to prevent deadlocking.
    wait10Seconds :: IO (Maybe (Int, ReadyPayload))
    wait10Seconds = do
        threadDelay $ 10 * 10^(6 :: Int)
        pure $ Nothing

    -- | Wait for both Opcode 2 Ready and Opcode 8 Hello, and return both
    -- responses in a Maybe (so that the type signature matches wait10seconds)
    waitForHelloReady
        :: Connection
        -> Maybe Int
        -> Maybe ReadyPayload
        -> IO (Maybe (Int, ReadyPayload))
    waitForHelloReady conn (Just x) (Just y) = pure $ Just (x, y)
    waitForHelloReady conn mb1 mb2 = do
        msg <- getPayload conn log
        case msg of
            Right (Ready payload) ->
                waitForHelloReady conn mb1 (Just payload)
            Right (Hello interval) ->
                waitForHelloReady conn (Just interval) mb2
            Right _ ->
                waitForHelloReady conn mb1 mb2
            Left _  ->
                waitForHelloReady conn mb1 mb2





getPayload
    :: Connection
    -> Chan T.Text
    -> IO (Either ConnectionException VoiceWebsocketReceivable)
getPayload conn log = try $ do
    msg' <- receiveData conn
    case eitherDecode msg' of 
        Right msg -> pure msg
        Left err  -> do
            writeChan log $ "Voice Websocket parse error - " <> T.pack err
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

-- | Create the sendable and heartbeat loops.
startEternalStream
    :: WebsocketConn
    -> Int
    -> Chan VoiceWebsocketSendable
    -> Chan T.Text
    -> IO ConnLoopState
startEternalStream wsconn interval sends log = do
    let err :: SomeException -> IO ConnLoopState
        err e = do
            writeChan log $ wsError $ "event stream error: " <> T.pack (show e)
            pure (ConnReconnect interval)
    handle err $ do
        sysSends <- newChan -- Chan for Heartbeat
        sendLoopId <- forkIO $ sendableLoop wsconn sysSends sends
        heartLoopId <- forkIO $ heartbeatLoop wsconn sysSends interval log

        finally (eventStream wsconn interval sysSends log) $
            (killThread heartLoopId >> killThread sendLoopId >> print "Exited eternal stream")

-- | Eternally stay on lookout for the connection. Writes to receivables channel.
eventStream
    :: WebsocketConn
    -> Int
    -> Chan VoiceWebsocketSendable
    -> Chan T.Text
    -> IO ConnLoopState
eventStream wsconn interval sysSends log = loop
  where
    loop :: IO ConnLoopState
    loop = do
        eitherPayload <- getPayloadTimeout (wsDataConnection wsconn) interval log
        case eitherPayload of
            -- Network-WebSockets, type ConnectionException
            Left (CloseRequest code str) -> do
                handleClose code str
            Left _ -> do
                writeChan log $ wsError
                    "connection exception in eventStream."
                pure (ConnReconnect interval)
            Right Reconnect -> do
                writeChan log $ wsError
                    "connection timed out, trying to reconnect again."
                pure (ConnReconnect interval)
            Right HeartbeatAck -> loop
            Right receivable -> do
                writeChan (wsDataReceivesChan wsconn) (Right receivable)
                loop
   
    handleClose :: Word16 -> BL.ByteString -> IO ConnLoopState
    handleClose code str = do
        let reason = TE.decodeUtf8 $ BL.toStrict str
        case code of
            -- from discord.py voice_client.py#L421
            1000 -> do
                writeChan log $ wsError reason
                -- Normal close
                pure ConnClosed
            4014 -> do
                -- VC deleted
                pure (ConnReconnect interval)
            4015 -> do
                -- VC crashed
                pure ConnClosed
            x    -> do
                writeChan log $ wsError $
                    "connection closed with code: [" <> T.pack (show code) <>
                        "] " <> reason
                pure ConnClosed


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
    sendTextData (wsDataConnection wsconn) $ encode payload
    sendableLoop wsconn sysSends usrSends

-- | Eternally send heartbeats through the sysSends channel
heartbeatLoop
    :: WebsocketConn
    -> Chan VoiceWebsocketSendable
    -> Int
    -> Chan T.Text
    -> IO ()
heartbeatLoop wsconn sysSends interval log = do
    threadDelay $ 1 * 10^(6 :: Int)
    forever $ do
        time <- round <$> getPOSIXTime
        writeChan sysSends $ Heartbeat $ time
        writeChan log $ "Heartbeat at " <> (T.pack $ show time)
        threadDelay $ interval * 1000
