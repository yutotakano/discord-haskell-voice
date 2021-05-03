module Discord.Internal.Types.VoiceUDP where

import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Binary
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

data VoiceUDPPacket
    = IPDiscovery Integer T.Text Integer
    -- ^ ssrc, ip, port
    | SpeakingData BL.ByteString
    -- ^ raw audio bytes
    | MalformedPacket BL.ByteString
    deriving (Show, Eq)

instance Binary VoiceUDPPacket where
    get = do
        flags <- getWord8
        case flags of
            0 -> do
                _ <- getWord8
                _ <- getWord16be
                ssrc <- toInteger <$> getWord32be
                ip <- TE.decodeUtf8 . B.takeWhile (/= 0) <$> getByteString 64
                port <- toInteger <$> getWord16be
                pure $ IPDiscovery ssrc ip port
            128 -> do
                _ <- getWord8
                seq <- getWord16be
                timestamp <- getWord32be
                ssrc <- getWord32be
                a <- getRemainingLazyByteString
                pure $ SpeakingData a
            other -> do
                a <- getRemainingLazyByteString
                pure $ MalformedPacket $ BL.singleton other `BL.append` a
    put (IPDiscovery ssrc ip port) = do
        putWord16be 1 -- 1 is request, 2 is response
        putWord16be 70 -- specified in docs
        putWord32be $ fromIntegral ssrc
        putLazyByteString $ BL.replicate 64 0 -- 64 empty bytes
        putWord16be $ fromIntegral port
    put (SpeakingData raw) = do
        putWord16be 1
    put (MalformedPacket a) = do
        putLazyByteString a
