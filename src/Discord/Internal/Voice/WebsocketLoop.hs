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
data WebsocketConnInfo = ConnInfo
    { sessionId  :: T.Text
    , token      :: T.Text
    , guildId    :: GuildId
    , endpoint   :: T.Text
    }

data WebsocketConn = ConnData
    { connection   :: Connection
    , connInfo     :: WebsocketConnInfo
    , receivesChan :: Chan (Either VoiceWebsocketException VoiceWebsocketReceivable)
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
connect url = runSecureClient (T.unpack url) 443 ""

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
                                            interval sends log
                             
                        Right m -> do
                            writeChan log $ wsError 
                                "first message has to be opcode 4 Ready"
                            writeChan receives $ Left $
                                VoiceWebsocketUnexpected m
                                    "first message has to be opcode 4 Ready"
                            pure ConnClosed
                        Left ce -> do
                            writeChan log $ wsError
                                "unexpected response to connection"
                            writeChan receives $ Left $
                                VoiceWebsocketConnection ce
                                    "unexpected response to connection"
                            pure ConnClosed
                case next :: Either SomeException ConnLoopState of
                    Left _ -> do
                        writeChan log $ wsError
                            "could not connect due to an exception"
                        writeChan receives $ Left $
                            VoiceWebsocketCouldNotConnect
                                "could not connect due to an exception"
                        loop ConnClosed 0
                    Right n -> loop n 0

            ConnReconnect previousInterval -> do
                next <- try $ connect (endpoint info) $ \conn -> do
                    -- Send opcode 7 Resume
                    sendTextData conn $ encode $
                        Resume (guildId info) (sessionId info) (token info)
                    -- Attempt to get opcode 9 Resumed
                    msg <- getPayload conn log
                    case msg of
                        Right Resumed -> do
                            startEternalStream
                                (ConnData conn info receives)
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
                case next :: Either SomeException ConnLoopState of
                    Left _ -> do
                        writeChan log $ wsError
                            "could not resume, retrying after 10 seconds"
                        threadDelay $ 10 * (10^(6 :: Int))
                        loop (ConnReconnect previousInterval) (retries + 1)
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
    Left () -> pure (Right Reconnect)
    Right other -> pure other

-- | Create the sendable and heartbeat loops.
startEternalStream
    :: WebsocketConn
    -> Int
    -> Chan VoiceWebsocketSendable
    -> Chan T.Text
    -> IO ConnLoopState
startEternalStream connData interval sends log = do
    writeChan log $ wsError "eternal event stream started"
    let err :: SomeException -> IO ConnLoopState
        err e = do
            writeChan log $ wsError $ "event stream error: " <> T.pack (show e)
            pure (ConnReconnect interval)
    handle err $ do
        sysSends <- newChan -- Chan for Heartbeat
        sendLoopId <- forkIO $ sendableLoop connData sysSends sends
        heartLoopId <- forkIO $ heartbeatLoop connData sysSends interval

        finally (eventStream connData interval sysSends log) $
            (killThread heartLoopId >> killThread sendLoopId)

-- | Eternally stay on lookout for the connection. Writes to receivables channel.
eventStream
    :: WebsocketConn
    -> Int
    -> Chan VoiceWebsocketSendable
    -> Chan T.Text
    -> IO ConnLoopState
eventStream connData interval sysSends log = loop
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
                pure (ConnReconnect interval)
            Right Reconnect -> do
                writeChan log $ wsError
                    "connection timed out."
                pure (ConnReconnect interval)
            Right receivable -> do
                writeChan (receivesChan connData) (Right receivable)
                loop

-- | Eternally send data from sysSends and usrSends channels
sendableLoop
    :: WebsocketConn
    -> Chan VoiceWebsocketSendable
    -> Chan VoiceWebsocketSendable
    -> IO ()
sendableLoop connData sysSends usrSends = do
    -- Wait-time taken from discord-haskell/Internal.Gateway.EventLoop
    threadDelay $ round ((10^(6 :: Int)) * (62 / 120) :: Double)
    -- Get whichever possible, and send it
    payload <- either id id <$> race (readChan sysSends) (readChan usrSends)
    sendTextData (connection connData) $ encode payload
    sendableLoop connData sysSends usrSends

-- | Eternally send heartbeats through the sysSends channel
heartbeatLoop
    :: WebsocketConn
    -> Chan VoiceWebsocketSendable
    -> Int
    -> IO ()
heartbeatLoop connData sysSends interval = do
    threadDelay $ 1 * 10^(6 :: Int)
    forever $ do
        time <- round <$> getPOSIXTime
        writeChan sysSends $ Heartbeat $ time
        threadDelay $ interval * 1000
