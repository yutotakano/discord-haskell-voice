module Discord.Internal.Voice.UDPLoop where

import           Crypto.Saltine.Core.SecretBox
                                            ( Key(..)
                                            , Nonce(..)
                                            , secretboxOpen
                                            )
import qualified Crypto.Saltine.Class as SC
import           Control.Concurrent         ( Chan
                                            , readChan
                                            , writeChan
                                            , MVar
                                            , takeMVar
                                            , putMVar
                                            , forkIO
                                            , killThread
                                            )
import           Control.Exception.Safe     ( handle
                                            , SomeException
                                            , finally
                                            , try
                                            )
import           Data.Binary.Put            ( runPut
                                            )
import           Data.Binary                ( encode
                                            , putList
                                            , putWord8
                                            , decode
                                            )
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Maybe                 ( fromJust
                                            )
import           Data.Word                  ( Word8
                                            )
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

udpLoop :: DiscordVoiceHandleUDP -> UDPConnInfo -> MVar [Word8] -> Chan T.Text -> IO ()
udpLoop (receives, sends) connInfo syncKey log = loop ConnStart 0
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
                        (receives, sends) syncKey log

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
    -> MVar [Word8]
    -> Chan T.Text
    -> IO ConnLoopState
startEternalStream conn (receives, sends) syncKey log = do
    let handler :: SomeException -> IO ConnLoopState
        handler e = do
            writeChan log $ udpError $ "event stream error: " <> T.pack (show e)
            pure ConnReconnect
    handle handler $ do
        sendLoopId <- forkIO $ sendableLoop conn sends syncKey log

        finally (receivableLoop conn receives syncKey log >> pure ConnClosed)
            (killThread sendLoopId >> print "Exited UDP stream")

receivableLoop :: UDPConn -> Chan VoiceUDPPacket -> MVar [Word8] -> Chan T.Text -> IO ()
receivableLoop conn receives syncKey log = do
    msg' <- decode <$> recv (udpDataSocket conn) 999
    msg <- case msg' of
        SpeakingDataEncrypted nonce' og -> do
            byteKey <- takeMVar syncKey
            let key = fromJust $ SC.decode $ B.pack byteKey :: Key
            putMVar syncKey byteKey
            let nonce = fromJust $ SC.decode nonce'
            let deciphered = secretboxOpen key nonce $ BL.toStrict og
            case deciphered of
                Nothing -> do
                    writeChan log $ udpError $
                        "could not decipher audio message!"
                    pure $ MalformedPacket og
                Just x  -> do
                    pure $ SpeakingData x
        SpeakingDataEncryptedExtra nonce' og -> do
            -- Almost similar, but remove first 8 bytes of decoded audio
            byteKey <- takeMVar syncKey
            let key = fromJust $ SC.decode $ B.pack byteKey :: Key
            putMVar syncKey byteKey
            print $ B.length nonce'
            let nonce = fromJust $ SC.decode nonce'
            let deciphered = secretboxOpen key nonce $ BL.toStrict og
            case deciphered of
                Nothing -> do
                    writeChan log $ udpError $
                        "could not decipher audio message!"
                    pure $ MalformedPacket og
                Just x  -> do
                    pure $ SpeakingData $ B.drop 8 x
        other -> pure other

    print msg
    writeChan receives msg
    receivableLoop conn receives syncKey log

sendableLoop :: UDPConn -> Chan VoiceUDPPacket -> MVar [Word8] -> Chan T.Text -> IO ()
sendableLoop conn sends syncKey log = do
    -- Immediately send the first packet available
    top <- readChan sends
    sendAll (udpDataSocket conn) $ encode top

    sendableLoop conn sends syncKey log
