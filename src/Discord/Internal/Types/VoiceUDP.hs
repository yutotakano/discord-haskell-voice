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
    | SpeakingData B.ByteString
    | SpeakingDataEncrypted B.ByteString BL.ByteString
    -- ^ nonce, and encrypted audio bytes
    | SpeakingDataEncryptedExtra B.ByteString BL.ByteString
    -- ^ nonce, and encrypted audio bytes with extended header inside
    | UnknownPacket BL.ByteString
    | MalformedPacket BL.ByteString
    deriving (Show, Eq)

instance Binary VoiceUDPPacket where
    get = do
        flags <- lookAhead getWord8
        case flags of
            0x0 -> do
                _ <- getWord16be
                _ <- getWord16be
                ssrc <- toInteger <$> getWord32be
                ip <- TE.decodeUtf8 . B.takeWhile (/= 0) <$> getByteString 64
                port <- toInteger <$> getWord16be
                pure $ IPDiscovery ssrc ip port
            0x80 -> do
                -- Receiving audio is undocumented but should be pretty much
                -- the same as sending, according to several GitHub issues.
                header <- lookAhead $ getByteString 12
                -- Attach 12 empty bytes to create the 24 requires, as per the
                -- Discord docs.
                let nonce = B.append header $ B.concat $
                        replicate 12 $ B.singleton 0
                _ <- getWord16be
                seq <- getWord16be
                timestamp <- getWord32be
                ssrc <- getWord32be
                a <- getRemainingLazyByteString
                pure $ SpeakingDataEncrypted nonce a
            0x90 -> do
                -- undocumented, but it seems to also be audio data
                -- When it is 0x90, the encrypted spoken data contains an
                -- extended header (0x90 is sent from Chromium on browser Discord
                -- but 0x80 is from Desktop Discord)
                --
                -- https://github.com/bwmarrin/discordgo/issues/423
                -- https://github.com/discord/discord-api-docs/issues/231
                header <- lookAhead $ getByteString 12
                let nonce = B.append header $ B.concat $
                        replicate 12 $ B.singleton 0
                _ <- getWord16be
                seq <- getWord16be
                timestamp <- getWord32be
                ssrc <- getWord32be
                a <- getRemainingLazyByteString
                pure $ SpeakingDataEncryptedExtra nonce a
            other -> do
                a <- getRemainingLazyByteString
                pure $ UnknownPacket a
    put (IPDiscovery ssrc ip port) = do
        putWord16be 1 -- 1 is request, 2 is response
        putWord16be 70 -- specified in docs
        putWord32be $ fromIntegral ssrc
        putLazyByteString $ BL.replicate 64 0 -- 64 empty bytes
        putWord16be $ fromIntegral port
    put (SpeakingDataEncrypted nonce raw) = do
        putWord16be 1
    put (MalformedPacket a) = do
        putLazyByteString a
