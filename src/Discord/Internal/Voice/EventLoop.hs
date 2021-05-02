module Discord.Internal.Voice.EventLoop where

import           Control.Exception.Safe     ( try
                                            , SomeException
                                            )
import           Control.Monad              ( forever
                                            )
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock.POSIX
import           Wuss                       ( runSecureClient )
import           Network.Websockets         ( ConnectionException(..)
                                            , Connection
                                            , receiveData
                                            , sendTextData
                                            )

import           Discord.Internal.Types
import           Discord.Internal.Types.VoiceWebsocket

data VoiceWebsocketException = VoiceWebsocketCouldNotConnect T.Text
                             | VoiceWebsocketEventParseError T.Text
                             | VoiceWebsocketUnexpected VoiceWebsocketReceivable T.Text
                             | VoiceWebsocketConnection ConnectionException T.Text
                             deriving (Show)

type DiscordVoiceHandleWebsocket = (Chan (Either VoiceWebsocketException VoiceWebsocketReceivable), Chan VoiceWebsocketSendable)

-- | session_id, token, guild_id, endpoint
type WebsocketConnInfo = ConnInfo
    { sessionId  :: T.Text
    , token      :: T.Text
    , guildId    :: GuildId
    , endpoint   :: T.Text
    }

type WebsocketConn = ConnData
    { connection   :: Connection
    , connInfo     :: WebsocketConnInfo
    , receivesChan :: Chan (Either VoiceWebsocketException VoiceWebsocketReceivable)
    }

data ConnLoopState
    = ConnStart
    | ConnClosed
    | ConnReconnect
    deriving Show

wsError :: T.Text -> T.Text
wsError t = "Voice Websocket error - " <> t

connect :: T.Text -> (Connection -> IO a) -> IO a
connect url = runSecureClient url 443 ""

-- | Attempt to connect (and reconnect on disconnects) to the voice websocket.
voiceWebsocketLoop :: DiscordVoiceHandleWebsocket -> (WebsocketConnInfo, UserId) -> Chan T.Text -> IO ()
voiceWebsocketLoop (receives, sends) (info, userId) log = loop ConnStart 0
  where
    loop :: ConnLoopState -> Int -> IO ()
    loop s retries = do
        case s of
            ConnClosed -> pure ()

            ConnStart -> do
                next <- try $ connect (endpoint info) $ \conn -> do
                    -- Send opcode 0 Identify
                    sendTextData conn $ encode $ Identify $ IdentifyPayload
                        { identifyPayloadServerId = guildId info
                        , identifyPayloadUserId = userId
                        , identifyPayloadSessionId = sessionId info
                        , identifyPayloadToken = token info
                        }
                    -- Attempt to get opcode 2 Ready
                    msg <- getPayload conn log
                    case msg of
                        -- Not documented in the docs, but we presume
                        -- the Identify -> Ready flow is done before Hello
                        Right (Ready payload) -> do
                            -- Ready received, so now wait for Hello
                            msg2 <- getPayload conn log
                            case msg2 of
                                Right (Hello interval) -> do
                                    -- All good! Start the continuous stream.
                                    writeChan receives $ Right (Ready payload)
                                    startEternalStream
                                        (ConnData conn info receives)
                                            interval 0 sends log
                             
                        Right m -> do
                            writeChan log $ wsError 
                                "first message has to be opcode 4 Ready"
                            writeChan receives $ Left $
                                VoiceWebsocketUnexpected m
                                    "first message has to be opcode 4 Ready"
                            pure ConnClosed
                        Left m -> do
                            writeChan log $ wsError
                                "unexpected response to connection"
                            writeChan receives $ Left $
                                VoiceWebsocketUnexpected m
                                    "unexpected response to connection"
                            pure ConnClosed
                case next :: Either SomeException ConnLoopState of
                    Left _ -> do
                        writeChan log $ wsError
                            "could not connect due to an exception"
                        writeChan receives $ Left $
                            VoiceWebsocketConnection
                                "could not connect due to an exception"
                        pure ConnClosed
                    Right n -> loop n 0

            ConnReconnect -> do
                next <- try $ connect (endpoint info) $ \conn -> do
                    -- Send opcode 7 Resume
                    sendTextData conn $ encode $
                        Resume (guildId connInfo) (sessionId connInfo) (token connInfo)
                    -- Attempt to get opcode 9 Resumed
                    msg <- getPayload conn log
                    case msg of
                        Right Resumed -> do
                            startEternalStream
                                (ConnData conn info receives)
                                    interval sends log
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
                                VoiceWebsocketUnexpected m
                                    "unexpected response to resumption"
                            pure ConnClosed
                case next :: Either SomeException ConnLoopState of
                    Left _ -> do
                        writeChan log $ wsError
                            "could not resume, retrying after 10 seconds"
                        threadDelay $ 10 * (10^(6 :: Int))
                        loop ConnReconnect (retries + 1)
                    Right n -> loop n 1

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
    Left () -> pure (Right Reconnect) -- TODO stuff with reconnection and connectcode and stuff
    Right other -> pure other

-- | Create the sendable and heartbeat loops.
startEternalStream
    :: WebsocketConnInfo
    -> Integer
    -> Chan VoiceWebsocketSendable
    -> Chan T.Text
    -> IO ConnLoopState
startEternalStream connData interval sends log = do
    writeChan log $ wsError "eternal event stream started"
    let err :: SomeException -> IO ()
        err e = do
            writeChan log $ wsError $ "event stream error: " <> T.pack (show e)
            pure Reconnect
    handle err $ do
        sysSends <- newChan -- Chan for Heartbeat
        sendLoopId <- forkIO $ sendableLoop connData sysSends sends
        heartLoopId <- forkIO $ heartbeatLoop connData sysSends interval

        finally (eventStream connData interval sysSends) $
            (killThread heartLoopId >> killThread sendLoopId)

-- | Eternally stay on lookout for the connection. Writes to receivables channel.
eventStream
    :: WebsocketConnData
    -> Integer
    -> Chan VoiceWebsocketSendable
    -> IO ConnLoopState
eventStream connData interval sysSends = loop
  where
    loop :: IO ConnLoopState
    loop = do
        eitherPayload <- getPayloadTimeout (connection connData) interval log
        case eitherPayload of
            -- https://hackage.haskell.org/package/websockets-0.12.7.2/docs/Network-WebSockets.html#t:ConnectionException
            Left (CloseRequest code str) -> do
                writeChan log $ wsError $
                    "connection closed with code: " <> T.pack (show code)
                pure ConnClosed
            Left _ -> do
                writeChan log $ wsError
                    "connection exception in eventStream."
                pure Reconnect
            Right receivable ->
                writeChan (receivesChan connData) (Right receivable)
                loop

-- | Eternally send data from sysSends and usrSends channels
sendableLoop
    :: WebsocketConnData
    -> Chan VoiceWebsocketSendable
    -> Chan VoiceWebsocketSendable
    -> IO ()
sendableLoop connData sysSends usrSends = do
    -- Wait-time taken from discord-haskell/Internal.Gateway.EventLoop
    threadDelay $ round ((10^(6 :: Int)) * (62 / 120) :: Double)
    -- Get whichever possible, and send it
    payload <- either id id <$> race (readChan sysSends) (readChan usrSends)
    sendTextData (connection connData) $ encode payload
    sendableLoop

-- | Eternally send heartbeats through the sysSends channel
heartbeatLoop
    :: WebsocketConnData
    -> Chan VoiceWebsocketSendable
    -> Integer
    -> IO ()
heartbeatLoop connData sysSends interval = do
    threadDelay $ 1 * 10^(6 :: Int)
    forever $ do
        time <- round <$> getPOSIXTime
        writeChan sysSends $ Heartbeat $ time
        threadDelay $ interval * 1000
