module Discord.Internal.Voice.UDPLoop where

import           Codec.Audio.Opus.Decoder
import           Crypto.Saltine.Core.SecretBox
                                            ( Key(..)
                                            , Nonce(..)
                                            , secretboxOpen
                                            , secretbox
                                            )
import qualified Crypto.Saltine.Class as SC
import qualified Control.Concurrent.BoundedChan as Bounded
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
import           Control.Lens.Operators     ( (#)
                                            )
import           Control.Monad.IO.Class     ( MonadIO
                                            )
import           Data.Binary                ( encode
                                            , decode
                                            )
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Builder
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time.Clock.POSIX      ( getPOSIXTime
                                            , POSIXTime
                                            )
import           Data.Maybe                 ( fromJust
                                            )
import           Data.Word                  ( Word8
                                            )
import           Network.Socket
import           Network.Socket.ByteString.Lazy
                                            ( sendAll
                                            , recv
                                            )

import           Discord.Internal.Types.Common
import           Discord.Internal.Types.VoiceUDP



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
        currentTime <- getPOSIXTime
        sendLoopId <- forkIO $ sendableLoop conn sends syncKey log 0 0 currentTime

        -- write ten frames of silence initially
        sequence_ $ replicate 10 $ Bounded.writeChan sends "\248\255\254"

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
    msg'' <- decode <$> recv (udpDataSocket conn) 999
    -- decrypt any encrypted audio packets to plain SpeakingData
    msg' <- case msg'' of
        SpeakingDataEncrypted header og -> do
            byteKey <- readMVar syncKey
            let nonce = createNonceFromHeader header
            let deciphered = decrypt byteKey nonce $ BL.toStrict og
            case deciphered of
                Nothing -> do
                    writeChan log $ udpError $
                        "could not decipher audio message!"
                    pure $ MalformedPacket $ BL.append (BL.fromStrict header) og
                Just x  -> pure $ SpeakingData x
        SpeakingDataEncryptedExtra header og -> do
            -- Almost similar, but remove first 8 bytes of decoded audio
            byteKey <- readMVar syncKey
            let nonce = createNonceFromHeader header
            let deciphered = decrypt byteKey nonce $ BL.toStrict og
            case deciphered of
                Nothing -> do
                    writeChan log $ udpError $
                        "could not decipher audio message!"
                    pure $ MalformedPacket $ BL.append (BL.fromStrict header) og
                Just x  -> pure $ SpeakingData $ B.drop 8 x
        other -> pure other

    -- decode speaking data's OPUS to raw PCM
    msg <- case msg' of
        SpeakingData bytes -> decodeOpusData bytes
        other -> print other >> pure other

    writeChan receives msg
    receivableLoop conn receives syncKey log

-- | Appends 12 empty bytes to form the 24-byte nonce for the secret box.
createNonceFromHeader :: B.ByteString -> B.ByteString
createNonceFromHeader h = B.append h $ B.concat $ replicate 12 $ B.singleton 0

-- | Eternally send the top packet in the sendable packet Chan. It assumes that
-- it is already OPUS-encoded. The function will encrypt it using the syncKey.
sendableLoop
    :: UDPConn
    -> Bounded.BoundedChan B.ByteString
    -- ^ Channel of OPUS-encoded audio data
    -> MVar [Word8]
    -- ^ The encryption key
    -> Chan T.Text
    -- ^ Logs
    -> Integer
    -- ^ Sequence number, modulo 65535
    -> Integer
    -- ^ Timestamp number, modulo 4294967295
    -> POSIXTime
    -> IO ()
sendableLoop conn sends syncKey log sequence timestamp startTime = do
    -- Immediately send the first packet available
    mbOpusBytes <- Bounded.tryReadChan sends
    case mbOpusBytes of
        Nothing -> do
            -- nothing could be read, so wait 20ms (no dynamic calculation
            -- required, because nothing demands accurate real-time)
            threadDelay $ round $ 20 * 10^(6 :: Int)
            currentTime <- getPOSIXTime
            sendableLoop conn sends syncKey log sequence timestamp currentTime
        Just opusBytes -> do
            let header = BL.toStrict $ encode $
                    Header 0x80 0x78 (fromIntegral sequence) (fromIntegral timestamp) $
                        fromIntegral $ udpInfoSSRC $ udpDataInfo conn
            let nonce = createNonceFromHeader header
            byteKey <- readMVar syncKey
            let encryptedOpus = BL.fromStrict $ encrypt byteKey nonce opusBytes

            -- send the header and the encrypted opus data
            sendAll (udpDataSocket conn) $
                encode $ SpeakingDataEncrypted header encryptedOpus

            -- wait a biiit less than 20ms before sending the next packet
            -- logic taken from discord.py discord/player.py L595
            let theoreticalNextTime = startTime + (20 / 1000)
            currentTime <- getPOSIXTime
            threadDelay $ round $ (max 0 $ theoreticalNextTime - currentTime) * 10^(6 :: Int)
            sendableLoop conn sends syncKey log
                (sequence + 1 `mod` 0xFFFF) (timestamp + 48*20 `mod` 0xFFFFFFFF) theoreticalNextTime

-- | Decrypt a sound packet using the provided Discord key and header nonce. The
-- argument is strict because it has to be strict when passed to Saltine anyway,
-- and having the same type signature leaves room for the caller to choose.
--
-- This does no error handling on misformatted key/nonce since this function is
-- only used in contexts where we are guaranteed they are valid.
decrypt :: [Word8] -> B.ByteString -> B.ByteString -> Maybe B.ByteString
decrypt byteKey byteNonce og = secretboxOpen key nonce og
  where
    key = fromJust $ SC.decode $ B.pack byteKey
    nonce = fromJust $ SC.decode byteNonce

-- | Encrypt a strict sound packet using the provided Discord key and header
-- nonce. The argument is strict because it has to be converted to strict
-- before passing onto Saltine anyway, and it leaves room for the caller of the
-- function to choose which laziness to use.
--
-- As with decryption, this function does no error handling on the format of the
-- key and nonce (key = 32 bytes, nonce = 24 bytes).
encrypt :: [Word8] -> B.ByteString -> B.ByteString -> B.ByteString
encrypt byteKey byteNonce og = secretbox key nonce og
  where
    key = fromJust $ SC.decode $ B.pack byteKey
    nonce = fromJust $ SC.decode byteNonce

decodeOpusData :: B.ByteString -> IO VoiceUDPPacket
decodeOpusData bytes = do
    let deCfg = _DecoderConfig # (opusSR48k, True)
    let deStreamCfg = _DecoderStreamConfig # (deCfg, 48*20, 0)
    decoder <- opusDecoderCreate deCfg
    decoded <- opusDecode decoder deStreamCfg bytes
    pure $ SpeakingData decoded

