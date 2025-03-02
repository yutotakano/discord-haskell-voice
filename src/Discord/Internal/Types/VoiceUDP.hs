{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : Discord.Internal.Types.VoiceUDP
Description : Strictly for internal use only. See Discord.Voice for the public interface.
Copyright   : (c) 2021-2022 Yuto Takano
              (c) 2025-PRESENT discord-haskell-voice Contributors
License     : MIT
Maintainer  : Yuto Takano <moa17stock@gmail.com>

= WARNING

This module is considered __internal__.

The Package Versioning Policy __does not apply__.

The contents of this module may change __in any way whatsoever__ and __without__
__any warning__ between minor versions of this package.

= Description

This module defines basic types for the communication packets in the Discord
Voice UDP socket. Binary instances are defined for the header and the body
payload, as according to the official Discord documentation for v4 of the gateway.
-}
module Discord.Internal.Types.VoiceUDP
    ( module Discord.Internal.Types.VoiceUDP
    ) where

import Lens.Micro
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary
import Data.ByteString.Lazy qualified as BL
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

-- | @VoiceUDPPacket@ represents a UDP packet that may be sent to or received
-- from Discord's voice UDP connection. Technically, some of these like
-- 'UnknownPacket' or 'MalformedPacket' will only ever be received and never
-- sent, so this type unifies two slightly different concepts into one. Perhaps
-- in the future it might be split into different types.
--
-- Binary instances are defined for packing and unpacking to bytes.
data VoiceUDPPacket
    = IPDiscovery Integer T.Text Integer
    -- ^ Uplink and Downlink IP discovery packet for performing IP discovery.
    -- The three fields are for SSRC, IP and port. For uplink, specify the
    -- client SSRC and leave the rest blank/0. The downlink packet will contain
    -- the discovered client IP and port.
    | SpeakingData B.ByteString
    -- ^ Decrypted speaking data in Opus or PCM format. This is implemented for
    -- completeness and is never used in the uplink. We thus make no guarantees
    -- over its format or decoding.
    | SpeakingDataEncrypted B.ByteString BL.ByteString
    -- ^ RTP Header followed by encrypted Opus audio bytes. This is used both in
    -- the uplink and downlink. In downlink, it is decrypted into 'SpeakingData'.
    | SpeakingDataEncryptedExtra B.ByteString BL.ByteString
    -- ^ RTP Header followed by encrypted Opus audio bytes with an extended
    -- header inside. This is implemented for completeness and is never used in
    -- the uplink. It is similar to 'SpeakingDataEncrypted' but contains some
    -- extra data alongside audio. Anecdotally, it seems to be that users on
    -- Discord Desktop send 'SpeakingDataEncrypted' to bots, while users on
    -- Discord on Chromium browsers send 'SpeakingDataEncryptedExtra' to bots.
    | UnknownPacket BL.ByteString
    -- ^ A packet that couldn't be decoded into any of the other options.
    | MalformedPacket BL.ByteString
    -- ^ A packet that could be decoded into encrypted audio data, but for which
    -- decryption failed.
    deriving stock (Show, Eq)

-- | @_IPDiscovery@ is a 'Traversal'' for manipulating the fields of the
-- 'IPDiscovery' UDP packet. It is a no-op for other packet types.
--
-- The following code returns @Just IPDiscovery(..)@ for IPDiscovery packets and
-- Nothing for all other packet types.
--
-- @@
-- payload :: Maybe VoiceUDPPacket
-- payload = packet ^? _IPDiscovery
-- @@
_IPDiscovery :: Traversoal' VoiceUDPPacket (Integer, T.Text, Integer)
_IPDiscovery f (IPDiscovery ssrc ip port) = (\(a, b, c) -> IPDiscovery a b c) <$> f (ssrc, ip, port)
_IPDiscovery _ packet = pure packet

-- | @VoiceUDPPacketHeader@ represents the RTP header for an uplink UDP audio
-- packet. It is to be constructed manually.
data VoiceUDPPacketHeader
    = Header Word8 Word8 Word16 Word32 Word32
    -- ^ The fields in order are: version + flags (0x80), payload type (0x78),
    -- sequence, timestamp, and SSRC.

-- | The binary instance for 'VoiceUDPPacketHeader' is a simple big-endian
-- binary encoder and decoder. Discord requires numbers to be big-endian.
instance Binary VoiceUDPPacketHeader where
    get = do
        ver <- getWord8
        pl <- getWord8
        seq <- getWord16be
        timestamp <- getWord32be
        ssrc <- getWord32be
        pure $ Header ver pl seq timestamp ssrc
    put (Header ver pl seq timestamp ssrc) = do
        putWord8 ver
        putWord8 pl
        putWord16be seq
        putWord32be timestamp
        putWord32be ssrc

-- | The binary instance for 'VoiceUDPPacket'encodes and decodes all documented
-- UDP packet types. Numbers are encoded as big-endian per Discord's docs.
-- Decryption does not take place for audio packets, so the output will be
-- either 'SpeakingDataEncrypted' or 'SpeakingDataEncryptedExtra', where the
-- difference is whether the speaker was using a Chromium web browser or Discord
-- Desktop. Decryption not taking place also means that the 'get' definition
-- for this instance will never output a 'MalformedPacket' (for decryption
-- failures), although it may output 'UnknownPacket' (on seeing undocumented
-- packet types).
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
                header <- getByteString 12
                a <- getRemainingLazyByteString
                pure $ SpeakingDataEncrypted header a
            0x90 -> do
                -- undocumented, but it seems to also be audio data
                -- When it is 0x90, the encrypted spoken data contains an
                -- extended header (0x90 is sent from Chromium on browser Discord
                -- but 0x80 is from Desktop Discord)
                --
                -- https://github.com/bwmarrin/discordgo/issues/423
                -- https://github.com/discord/discord-api-docs/issues/231
                header <- getByteString 12
                a <- getRemainingLazyByteString
                pure $ SpeakingDataEncryptedExtra header a
            _other -> do
                a <- getRemainingLazyByteString
                pure $ UnknownPacket a
    put (IPDiscovery ssrc _ip port) = do
        putWord16be 1 -- 1 is request, 2 is response
        putWord16be 70 -- specified in docs
        putWord32be $ fromIntegral ssrc
        putLazyByteString $ BL.replicate 64 0 -- 64 empty bytes
        putWord16be $ fromIntegral port
    put (SpeakingDataEncrypted header a) = do
        putByteString header
        putLazyByteString a
    put (MalformedPacket a) = putLazyByteString a

    -- Other datatypes should logically never be used with put.
    put _ = undefined
