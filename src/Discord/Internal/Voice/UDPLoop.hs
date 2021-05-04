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
                                            , readMVar
                                            , forkIO
                                            , killThread
                                            , threadDelay
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

-- | Simple function to prepend error messages with a template.
udpError :: T.Text -> T.Text
udpError t = "Voice UDP error - " <> t

-- | Starts the UDP connection, performs IP discovery, writes the result to the
-- receivables channel, and then starts an eternal loop of sending and receiving
-- packets.
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

            ConnReconnect -> do
                -- No need to perform IP discovery.
                next <- try $ do
                    let hints = defaultHints
                            { addrSocketType = Datagram
                            }
                    let ip = T.unpack $ udpInfoAddr connInfo
                    let port = show $ udpInfoPort connInfo
                    addr:_ <- getAddrInfo (Just hints) (Just ip) (Just port)
                    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
                    connect sock $ addrAddress addr
                    -- Connection succeded. Otherwise an Exception is propagated
                    -- in the IO monad.
                    writeChan log $ "UDP Connection reinitialised."
                    startEternalStream (UDPConn connInfo sock)
                        (receives, sends) syncKey log
                case next :: Either SomeException ConnLoopState of
                    Left e -> do
                        writeChan log $ udpError $
                            "could not reconnect to UDP, will restart in 10 secs."
                        threadDelay $ 10 * (10^(6 :: Int))
                        loop ConnReconnect (retries + 1)
                    Right n -> loop n 1

-- | Starts the sendable loop in another thread, and starts the receivable
-- loop in the current thread. Once receivable is closed, closes sendable and
-- exits. Reconnects if a temporary IO exception occured.
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
            (killThread sendLoopId)

-- | Eternally receive a packet from the socket (max length 999, so practically
-- never fails). Decrypts audio data as necessary, and writes it to the
-- receivables channel.
receivableLoop
    :: UDPConn
    -> Chan VoiceUDPPacket
    -> MVar [Word8]
    -> Chan T.Text
    -> IO ()
receivableLoop conn receives syncKey log = do
    -- max length has to be specified but is irrelevant since it is so big
    msg' <- decode <$> recv (udpDataSocket conn) 999
    -- decrypt any encrypted audio packets
    msg <- case msg' of
        SpeakingDataEncrypted byteNonce og -> do
            byteKey <- readMVar syncKey
            let deciphered = decrypt byteKey byteNonce og
            case deciphered of
                Nothing -> do
                    writeChan log $ udpError $
                        "could not decipher audio message!"
                    pure $ MalformedPacket og
                Just x  -> pure $ SpeakingData x
        SpeakingDataEncryptedExtra byteNonce og -> do
            -- Almost similar, but remove first 8 bytes of decoded audio
            byteKey <- readMVar syncKey
            let deciphered = decrypt byteKey byteNonce og
            case deciphered of
                Nothing -> do
                    writeChan log $ udpError $
                        "could not decipher audio message!"
                    pure $ MalformedPacket og
                Just x  -> pure $ SpeakingData $ B.drop 8 x
        other -> pure other

    writeChan receives msg
    print msg
    receivableLoop conn receives syncKey log

-- | Eternally send the top packet in the sendable packet Chan. 
-- If the packet is raw audio, it will automatically encrypt it using the key.
sendableLoop
    :: UDPConn
    -> Chan VoiceUDPPacket
    -> MVar [Word8]
    -> Chan T.Text
    -> IO ()
sendableLoop conn sends syncKey log = do
    -- Immediately send the first packet available
    top <- readChan sends
    sendAll (udpDataSocket conn) $ encode top

    sendableLoop conn sends syncKey log

-- | Decrypt a sound packet using the provided Discord key and header nonce.
-- This does no error handling on misformatted key/nonce since this function is
-- only used in contexts where we are guaranteed they are valid.
decrypt :: [Word8] -> B.ByteString -> BL.ByteString -> Maybe B.ByteString
decrypt byteKey byteNonce og = secretboxOpen key nonce $ BL.toStrict og
  where
    key = fromJust $ SC.decode $ B.pack byteKey
    nonce = fromJust $ SC.decode byteNonce
