{-# LANGUAGE ImportQualifiedPost #-}
{-|
Module      : Discord.Internal.Voice.UDPLoop
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

This module provides 'launchUdp', a function used to start a UDP socket and
perform initial handshaking with the Discord Voice UDP Endpoint. It will
continuously encrypt and send the Opus voice packets as received through the
specified Chan. The UDP (data-plane) thread is launched by the websocket
(control-plane) thread, however references to both are held by the main thread
which may terminate the threads using asynchronous exceptions (e.g. on leave).
-}
module Discord.Internal.Voice.UDPLoop
    ( launchUdp
    ) where

import Codec.Audio.Opus.Decoder
import Control.Concurrent
    ( Chan
    , writeChan
    , readMVar
    , forkIO
    , killThread
    , threadDelay
    , myThreadId
    )
import Control.Concurrent.BoundedChan qualified as Bounded
import Control.Exception.Safe ( SomeException, finally, try, bracket )
import Lens.Micro
import Data.Binary ( encode, decode )
import Data.ByteString.Lazy qualified as BL
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Time.Clock.POSIX
import Data.Time
import Network.Socket hiding ( socket )
import Network.Socket qualified as S ( socket )
import Network.Socket.ByteString.Lazy ( sendAll, recv )

import Discord.Internal.Types.VoiceCommon
import Discord.Internal.Types.VoiceUDP
import Discord.Internal.Voice.CommonUtils
import Discord.Internal.Voice.Encryption

-- | States of the UDP thread state machine.
data UDPState
    = UDPClosed
    | UDPStart
    | UDPReconnect

-- | A custom logging function that writes the date/time and the thread ID.
(✍) :: Chan T.Text -> T.Text -> IO ()
logChan ✍ log = do
    t <- formatTime defaultTimeLocale "%F %T %q" <$> getCurrentTime
    tid <- myThreadId
    writeChan logChan $ (T.pack t) <> " " <> (tshow tid) <> " " <> log

-- | A variant of (✍) that prepends the udpError text.
(✍!) :: Chan T.Text -> T.Text -> IO ()
logChan ✍! log = logChan ✍ ("!!! Voice UDP Error - " <> log)

-- | @runUDPClient@ is a helper for opening a UDP socket connection using the
-- Discord endpoint and performing an action using that connection. The socket
-- is closed afterwards.
--
-- The connection is wrapped in 'bracket', which allows cleanup after
-- synchronous exceptions.
runUDPClient :: AddrInfo -> (Socket -> IO a) -> IO a
runUDPClient addr things = bracket
    (S.socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
    close $ \sock -> do
        Network.Socket.connect sock $ addrAddress addr
        things sock

-- | @launchUdp@ starts the UDP connection, performs IP discovery, writes the
-- result to the receivables channel, and then starts an eternal loop of sending
-- and receiving packets.
launchUdp :: UDPLaunchOpts -> Chan T.Text -> IO ()
launchUdp opts log = loop UDPStart 0
  where
    loop :: UDPState -> Int -> IO ()
    loop UDPClosed _retries = pure ()
    loop UDPStart _retries = do
        next <- try $ do
            let hints = defaultHints
                    { addrSocketType = Datagram
                    -- TIL while developing: Stream: TCP, Datagram: UDP
                    }
            addr:_ <- getAddrInfo
                (Just hints)
                (Just $ T.unpack $ opts ^. ip)
                (Just $ show $ opts ^. port)

            runUDPClient addr $ \sock -> do
                -- Connection succeded. Otherwise an Exception is propagated
                -- in the IO monad.
                log ✍ "UDP Connection initialised."

                -- Perform IP discovery
                -- https://discord.com/developers/docs/topics/voice-connections#ip-discovery
                sendAll sock $ encode $ IPDiscovery (opts ^. ssrc) "" 0
                msg <- decode <$> recv sock 74
                writeChan (opts ^. udpHandle . _1) msg

                startForks (UDPConn opts sock) log

        case next :: Either SomeException UDPState of
            Left e -> do
                (✍!) log $ "could not start UDP conn due to an exception: " <>
                    (T.pack $ show e)
                loop UDPClosed 0
            Right n -> loop n 0

    loop UDPReconnect retries = do
        -- No need to perform IP discovery.
        next <- try $ do
            let hints = defaultHints
                    { addrSocketType = Datagram
                    }
            addr:_ <- getAddrInfo
                (Just hints)
                (Just $ T.unpack $ opts ^. ip)
                (Just $ show $ opts ^. port)

            runUDPClient addr $ \sock -> do
                -- Connection succeded. Otherwise an Exception is propagated
                -- in the IO monad.
                log ✍ "UDP Connection re-initialised."
                startForks (UDPConn opts sock) log

        case next :: Either SomeException UDPState of
            Left _e -> do
                log ✍! "could not reconnect to UDP, will restart in 10 secs."
                threadDelay $ 10 * (10^(6 :: Int))
                loop UDPReconnect (retries + 1)
            Right n -> loop n 1

-- | @startForks@ starts the sendable loop in another thread, and starts the
-- receivable loop in the current thread. Once receivable is closed, closes
-- sendable and exits.
startForks
    :: UDPConn
    -> Chan T.Text
    -> IO UDPState
startForks conn log = do
    currentTime <- getPOSIXTime
    sendLoopId <- forkIO $ sendableLoop conn log 0 0 currentTime

    -- write five frames of silence initially
    -- TODO: check if this is needed (is the 5 frames only for between voice,
    -- or also at the beginning like it is now?)
    sequence_ $ replicate 5 $ Bounded.writeChan (conn ^. launchOpts . udpHandle . _2) "\248\255\254"

    finally (receivableLoop conn log >> pure UDPClosed)
        (killThread sendLoopId)

-- | @receivableLoop@ eternally receives a packet from the socket (with a buffer
-- size of 999, so that it practically never fails). The function decrypts audio
-- data if it is encrypted, and relays it to the receivables channel.
receivableLoop
    :: UDPConn
    -> Chan T.Text
    -> IO ()
receivableLoop conn log = do
    -- max length has to be specified but is irrelevant since it is so big
    msg'' <- decode <$> recv (conn ^. socket) 999
    -- decrypt any encrypted audio packets to plain SpeakingData
    msg' <- case msg'' of
        SpeakingDataEncrypted header og -> do
            byteKey <- readMVar (conn ^. launchOpts . secretKey)
            let nonce = createNonceFromHeader header
            let deciphered = decrypt byteKey nonce $ BL.toStrict og
            case deciphered of
                Nothing -> do
                    log ✍! "could not decipher audio message!"
                    pure $ MalformedPacket $ BL.append (BL.fromStrict header) og
                Just x  -> pure $ SpeakingData x
        SpeakingDataEncryptedExtra header og -> do
            -- Almost similar, but remove first 8 bytes of decoded audio
            byteKey <- readMVar (conn ^. launchOpts . secretKey)
            let nonce = createNonceFromHeader header
            let deciphered = decrypt byteKey nonce $ BL.toStrict og
            case deciphered of
                Nothing -> do
                    log ✍! "could not decipher audio message!"
                    pure $ MalformedPacket $ BL.append (BL.fromStrict header) og
                Just x  -> pure $ SpeakingData $ B.drop 8 x
        other -> pure other

    -- log ✍ (tshow msg') -- TODO: debug, remove.
    -- decode speaking data's OPUS to raw PCM
    msg <- case msg' of
        SpeakingData bytes -> SpeakingData <$> decodeOpusData bytes
        other -> pure other

    writeChan (conn ^. launchOpts . udpHandle . _1) msg
    receivableLoop conn log

-- | Appends 12 empty bytes to a 12-byte nonce to form the 24-byte nonce for the
-- secret box.
createNonceFromHeader :: B.ByteString -> B.ByteString
createNonceFromHeader h = B.append h $ B.concat $ replicate 12 $ B.singleton 0

-- | @sendableLoop@ eternally sends the top packet in the sendable packet Chan.
-- It assumes that any audio is already Opus-encoded. The function will encrypt
-- it using the provided syncKey.
sendableLoop
    :: UDPConn
    -- ^ Connection details
    -> Chan T.Text
    -- ^ Logs
    -> Integer
    -- ^ Sequence number, modulo 65535
    -> Integer
    -- ^ Timestamp number, modulo 4294967295
    -> POSIXTime
    -- ^ The current time, which is used to dynamically calculate the delay so
    -- that audio packets are sent at just a little faster than 20ms intervals
    -- regardless of the speed at which they arrive in the sendables channel.
    -- This maximises the seamlessness of audio for other clients.
    -> IO ()
sendableLoop conn log sequence timestamp startTime = do
    -- Immediately send the first packet available
    mbOpusBytes <- Bounded.tryReadChan $ conn ^. launchOpts . udpHandle . _2
    case mbOpusBytes of
        Nothing -> do
            -- nothing could be read, so wait 20ms (no dynamic calculation
            -- required, because nothing demands accurate real-time)
            threadDelay $ round $ 20 * 10^(3 :: Int)
            currentTime <- getPOSIXTime
            sendableLoop conn log sequence timestamp currentTime
        Just opusBytes -> do
            let header = BL.toStrict $ encode $
                    Header 0x80 0x78 (fromIntegral sequence) (fromIntegral timestamp) $
                        fromIntegral $ conn ^. launchOpts . ssrc
            let nonce = createNonceFromHeader header
            byteKey <- readMVar $ conn ^. launchOpts . secretKey
            let encryptedOpus = BL.fromStrict $ encrypt byteKey nonce opusBytes

            -- send the header and the encrypted opus data
            sendAll (conn ^. socket) $
                encode $ SpeakingDataEncrypted header encryptedOpus

            -- wait a biiit less than 20ms before sending the next packet
            -- logic taken from discord.py discord/player.py L595
            let theoreticalNextTime = startTime + (20 / 1000)
            currentTime <- getPOSIXTime
            threadDelay $ round $ (max 0 $ theoreticalNextTime - currentTime) * 10^(6 :: Int)
            sendableLoop conn log
                (sequence + 1 `mod` 0xFFFF) (timestamp + 48*20 `mod` 0xFFFFFFFF) theoreticalNextTime

-- | @decodeOpusData@ decodes Opus data into dual-channel 16-bit little-endian
-- PCM bytes.
decodeOpusData :: B.ByteString -> IO B.ByteString
decodeOpusData bytes = do
    let deCfg = mkDecoderConfig opusSR48k True
    let deStreamCfg = mkDecoderStreamConfig deCfg (48*20*2) 0
    decoder <- opusDecoderCreate deCfg
    decoded <- opusDecode decoder deStreamCfg bytes
    pure decoded
