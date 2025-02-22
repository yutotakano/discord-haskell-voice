{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : Discord.Internal.Types.VoiceUDP
Description : Strictly for internal use only. See Discord.Voice for the public interface.
Copyright   : (c) 2021-2022 Yuto Takano
License     : MIT
Maintainer  : moa17stock@gmail.com

= WARNING

This module is considered __internal__.

The Package Versioning Policy __does not apply__.

The contents of this module may change __in any way whatsoever__ and __without__
__any warning__ between minor versions of this package.

= Description

This module defines basic types for the communication packets in the Discord
Voice UDP socket. Binary instances are defined for the header and the body
payload, as according to the official Discord documentation for v4 of the gateway.

Prisms are defined using TemplateHaskell for VoiceUDPPacket.
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

data VoiceUDPPacket
    = IPDiscovery Integer T.Text Integer
    -- ^ ssrc, ip, port
    | SpeakingData B.ByteString
    | SpeakingDataEncrypted B.ByteString BL.ByteString
    -- ^ header, and encrypted audio bytes
    | SpeakingDataEncryptedExtra B.ByteString BL.ByteString
    -- ^ header, and encrypted audio bytes with extended header inside
    | UnknownPacket BL.ByteString
    | MalformedPacket BL.ByteString
    deriving stock (Show, Eq)

_IPDiscovery :: Traversal' VoiceUDPPacket (Integer, T.Text, Integer)
_IPDiscovery f (IPDiscovery ssrc ip port) = (\(a, b, c) -> IPDiscovery a b c) <$> f (ssrc, ip, port)
_IPDiscovery _ packet = pure packet

data VoiceUDPPacketHeader
    = Header Word8 Word8 Word16 Word32 Word32 

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

-- $(makePrisms ''VoiceUDPPacket)
