module Discord.Internal.Voice.UDPLoop where

import           Control.Concurrent         ( Chan
                                            , readChan
                                            , writeChan
                                            , forkIO
                                            , killThread
                                            )
import           Control.Exception.Safe     ( handle
                                            , SomeException
                                            , finally
                                            , try
                                            )
import           Data.Binary                ( encode
                                            , decode
                                            )
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Network.Socket
import           Network.Socket.ByteString.Lazy
                                            ( sendAll
                                            , recv
                                            )

import           Discord.Internal.Types.VoiceUDP


type DiscordVoiceHandleUDP
  = ( Chan VoiceUDPPacket
    , Chan VoiceUDPPacket
    )

data UDPConnInfo = UDPConnInfo
    { udpInfoSSRC :: Integer
    , udpInfoAddr :: T.Text
    , udpInfoPort :: Integer
    , udpInfoMode :: T.Text
    }

data UDPConn = UDPConn
    { udpDataInfo :: UDPConnInfo
    , udpDataSocket :: Socket
    }

data ConnLoopState
    = ConnClosed
    | ConnStart
    | ConnReconnect

udpError :: T.Text -> T.Text
udpError t = "Voice UDP error - " <> t

udpLoop :: DiscordVoiceHandleUDP -> UDPConnInfo -> Chan T.Text -> IO ()
udpLoop (receives, sends) connInfo log = loop ConnStart 0
  where
    loop :: ConnLoopState -> Int -> IO ()
    loop s retries = do
        case s of
            ConnClosed -> pure ()

            ConnStart  -> do
                next <- try $ do
                    let hints = defaultHints
                            { addrSocketType = Datagram
                            -- TIL while developing: Stream: TCP, Datagram: UDP
                            }
                    let ip = T.unpack $ udpInfoAddr connInfo
                    let port = show $ udpInfoPort connInfo
                    addr:_ <- getAddrInfo (Just hints) (Just ip) (Just port)
                    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
                    connect sock $ addrAddress addr
                    -- Connection succeded. Otherwise an Exception is propagated
                    -- in the IO monad.
                    writeChan log $ "UDP Connection initialised."

                    -- Perform IP discovery
                    -- https://discord.com/developers/docs/topics/voice-connections#ip-discovery
                    sendAll sock $ encode $ IPDiscovery (udpInfoSSRC connInfo) "" 0
                    msg <- decode <$> recv sock 74
                    writeChan receives msg

                    startEternalStream (UDPConn connInfo sock)
                        (receives, sends) log

                case next :: Either SomeException ConnLoopState of
                    Left e -> do
                        writeChan log $ udpError $ 
                            "could not start UDP conn due to an exception: " <>
                            (T.pack $ show e)
                        loop ConnClosed 0
                    Right n -> loop n 0

startEternalStream
    :: UDPConn
    -> DiscordVoiceHandleUDP
    -> Chan T.Text
    -> IO ConnLoopState
startEternalStream conn (receives, sends) log = do
    let handler :: SomeException -> IO ConnLoopState
        handler e = do
            writeChan log $ udpError $ "event stream error: " <> T.pack (show e)
            pure ConnReconnect
    handle handler $ do
        sendLoopId <- forkIO $ sendableLoop conn sends log
        receiveLoopId <- forkIO $ receivableLoop conn receives log

        finally (receivableLoop conn receives log >> pure ConnClosed)
            (killThread sendLoopId >> print "Exited UDP stream")

receivableLoop :: UDPConn -> Chan VoiceUDPPacket -> Chan T.Text -> IO ()
receivableLoop conn receives log = do
    msg <- decode <$> recv (udpDataSocket conn) 999
    print msg
    writeChan log "new packet received"
    writeChan receives msg
    receivableLoop conn receives log

sendableLoop :: UDPConn -> Chan VoiceUDPPacket -> Chan T.Text -> IO ()
sendableLoop conn sends log = do
    -- Immediately send the first packet available
    top <- readChan sends
    sendAll (udpDataSocket conn) $ encode top

    sendableLoop conn sends log
