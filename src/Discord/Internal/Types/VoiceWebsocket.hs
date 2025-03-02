{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : Discord.Internal.Types.VoiceWebsocket
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

This module defines basic types for the communication packets in the Discord
Voice control plane gateway. Some ToJSON and FromJSON instances are defined,
according to the official Discord documentation for v4 of the gateway.
-}
module Discord.Internal.Types.VoiceWebsocket
    ( module Discord.Internal.Types.VoiceWebsocket
    ) where

import Control.Applicative ( (<|>) )
import Lens.Micro
import Data.Aeson
import Data.Aeson.Types
import Data.Text qualified as T
import Data.Word ( Word8 )

import Discord.Internal.Types.Prelude

-- | @VoiceWebsocketReceivable@ represents a JSON websocket packet that could be
-- received from Discord's voice control plane gateway.
data VoiceWebsocketReceivable
    = Ready ReadyPayload                            -- Opcode 2
    | SessionDescription T.Text [Word8]             -- Opcode 4
    | SpeakingR SpeakingPayload                     -- Opcode 5
    | HeartbeatAck Int                              -- Opcode 6
    | Hello Int                                     -- Opcode 8
      -- ^ Int because this is heartbeat, and threadDelay uses it
    | Resumed                                       -- Opcode 9
    | ClientDisconnect UserId                       -- Opcode 13
    | UnknownOPCode Integer Object                  -- Opcode unknown
    | ParseError T.Text                             -- Internal use
    | Reconnect                                     -- Internal use
    deriving stock (Show, Eq)

-- | @_Ready@ is a 'Traversal'' for manipulating the payload of the
-- 'Ready' voice gateway receivable packet. It is a no-op for other packet types.
--
-- The following code returns @Just Ready(..)@ for Ready packets and Nothing for
-- all other packet types.
--
-- @
-- payload :: Maybe VoiceWebsocketReceivable
-- payload = packet ^? _Ready
-- @
_Ready :: Traversal' VoiceWebsocketReceivable ReadyPayload
_Ready f (Ready rp) = Ready <$> f rp
_Ready _ rp = pure rp

-- | @_SessionDescription@ is a 'Traversal'' for manipulating the payload of the
-- 'SessionDescription' voice gateway receivable packet. It is a no-op for other
-- packet types.
--
-- The following code returns @Just SessionDescription(..)@ for
-- SessionDescription packets and Nothing for all other packet types.
--
-- @
-- payload :: Maybe VoiceWebsocketReceivable
-- payload = packet ^? _SessionDescription
-- @
_SessionDescription :: Traversal' VoiceWebsocketReceivable (T.Text, [Word8])
_SessionDescription f (SessionDescription t bytes) = uncurry SessionDescription <$> f (t, bytes)
_SessionDescription _ sd = pure sd

-- | @_Hello@ is a 'Traversal'' for manipulating the payload of the 'Hello'
-- voice gateway receivable packet. It is a no-op for other packet types.
--
-- The following code returns @Just Hello(..)@ for Hello packets and Nothing for
-- all other packet types.
--
-- @
-- payload :: Maybe VoiceWebsocketReceivable
-- payload = packet ^? _Hello
-- @
_Hello :: Traversal' VoiceWebsocketReceivable Int
_Hello f (Hello a) = Hello <$> f a
_Hello _ a = pure a

-- | @VoiceWebsocketSendable@ represents a JSON websocket packet that could be
-- sent to Discord's voice control plane gateway.
data VoiceWebsocketSendable
    = Identify IdentifyPayload                      -- Opcode 0
    | SelectProtocol SelectProtocolPayload          -- Opcode 1
    | Heartbeat Int                                 -- Opcode 3
      -- ^ Int because threadDelay uses it
    | Speaking SpeakingPayload                      -- Opcode 5
    | Resume GuildId T.Text T.Text                  -- Opcode 7
    deriving stock (Show, Eq)

-- | The payload of the voice control plane gateway downlink packet Opcode 2
-- Ready.
data ReadyPayload = ReadyPayload
    { readyPayloadSSRC  :: Integer
    -- ^ The 32-bit SSRC identifier for our bot client. SSRC is an identifier
    -- used by other voice clients to distinguish which packets are from which
    -- speaker.
    , readyPayloadIP    :: T.Text
    -- ^ The UDP IP to connect to for the data plane.
    , readyPayloadPort  :: Integer
    -- ^ The UDP port to connect to for the data plane.
    , readyPayloadModes :: [T.Text]
    -- ^ Supported encryption modes. Per Discord's documentation, we should
    -- support at least @aead_xchacha20_poly1305_rtpsize@, or
    -- @aead_aes256_gcm_rtpsize@ if available.
    -- , readyPayloadHeartbeatInterval
    -- ^ The Ready payload also contains heartbeat_interval, but this should not
    -- be used per Discord's docs. Instead, the Hello payload contains the
    -- correct heartbeat interval.
    }
    deriving stock (Show, Eq)

-- | The payload of the voice control plane gateway uplink packet Opcode 5
-- Speaking.
data SpeakingPayload = SpeakingPayload
    { speakingPayloadMicrophone :: Bool
    -- ^ Whether this is a normal microphone transmission of audio.
    , speakingPayloadSoundshare :: Bool
    -- ^ Whether this is a companion context audio for video, and the speaking
    -- indicator shouldn't light up.
    , speakingPayloadPriority   :: Bool
    -- ^ Whether the audio should be lower other speakers as a priority speaker.
    , speakingPayloadDelay      :: Integer
    -- ^ The delay field is not documented except that it should be set to 0 for
    -- all bots that use the voice gateway.
    , speakingPayloadSSRC       :: Integer
    -- ^ The SSRC for the bot client.
    }
    deriving stock (Show, Eq)

-- | The payload of the voice control plane gateway uplink packet Opcode 0
-- Identify.
data IdentifyPayload = IdentifyPayload
    { identifyPayloadServerId  :: GuildId
    -- ^ Server where bot wants to join the voice call
    , identifyPayloadUserId    :: UserId
    -- ^ User ID for the bot
    , identifyPayloadSessionId :: T.Text
    -- ^ Session ID obtained from the normal gateway.
    , identifyPayloadToken     :: T.Text
    -- ^ Token obtained from the normal gateway.
    }
    deriving stock (Show, Eq)

-- | The payload for the voice control plane gateway uplink packet Opcode 1
-- Select Protocol.
data SelectProtocolPayload = SelectProtocolPayload
    { selectProtocolPayloadProtocol :: T.Text
    -- ^ Can only be "udp" as far as we understand -- Discord hasn't explained
    , selectProtocolPayloadIP       :: T.Text
    -- ^ Our client's external IP for the UDP thread, which we discover through
    -- IP Discovery.
    , selectProtocolPayloadPort     :: Integer
    -- ^ Our client's external port for the UDP thread, which we discover through
    -- IP Discovery.
    , selectProtocolPayloadMode     :: T.Text
    -- ^ Our selected encryption mode, e.g. "aead_aes256_gcm_rtpsize"
    }
    deriving stock (Show, Eq)

instance FromJSON VoiceWebsocketReceivable where
    parseJSON = withObject "payload" $ \o -> do
        op <- o .: "op" :: Parser Integer
        case op of
            2 -> do
                od <- o .: "d"
                ssrc <- od .: "ssrc"
                ip <- od .: "ip"
                port <- od .: "port"
                modes <- od .: "modes"
                pure $ Ready $ ReadyPayload ssrc ip port modes
            4 -> do
                od <- o .: "d"
                mode <- od .: "mode"
                secretKey <- od .: "secret_key"
                pure $ SessionDescription mode secretKey
            5 -> do
                od <- o .: "d"
                speaking <-
                    -- speaking field can be a number or a boolean.
                    -- This is undocumented in the docs. God, discord.
                    (od .: "speaking" :: Parser Int) <|>
                        (do
                            s <- od .: "speaking" :: Parser Bool
                            case s of
                                True  -> pure 1
                                False -> pure 0
                        )

                let (priority, rest1) = speaking `divMod` 4
                let (soundshare, rest2) = rest1 `divMod` 2
                let microphone = rest2
                delay <- od .:? "delay" .!= 0
                -- The delay key is not present when we receive this data, but
                -- present when we send it, I think? not documented anywhere.
                ssrc <- od .: "ssrc"
                pure $ SpeakingR $ SpeakingPayload
                    { speakingPayloadMicrophone = toEnum microphone
                    , speakingPayloadSoundshare = toEnum soundshare
                    , speakingPayloadPriority   = toEnum priority
                    , speakingPayloadDelay      = delay
                    , speakingPayloadSSRC       = ssrc
                    }
            6 -> do
                od <- o .: "d"
                pure $ HeartbeatAck od
            8 -> do
                od <- o .: "d"
                interval <- od .: "heartbeat_interval"
                pure $ Hello interval
            9 -> pure Resumed
            13 -> do
                od <- o .: "d"
                uid <- od .: "user_id"
                pure $ ClientDisconnect uid
            _ -> pure $ UnknownOPCode op o

instance ToJSON VoiceWebsocketSendable where
    toJSON (Identify payload) = object
        [ "op" .= (0 :: Int)
        , "d"  .= object
            [ "server_id"  .= identifyPayloadServerId payload
            , "user_id"    .= identifyPayloadUserId payload
            , "session_id" .= identifyPayloadSessionId payload
            , "token"      .= identifyPayloadToken payload
            ]
        ]
    toJSON (SelectProtocol payload) = object
        [ "op" .= (1 :: Int)
        , "d"  .= object
            [ "protocol" .= selectProtocolPayloadProtocol payload
            , "data"     .= object
                [ "address" .= selectProtocolPayloadIP payload
                , "port"    .= selectProtocolPayloadPort payload
                , "mode"    .= selectProtocolPayloadMode payload
                ]
            ]
        ]
    toJSON (Heartbeat i) = object
        [ "op" .= (3 :: Int)
        , "d"  .= i
        ]
    toJSON (Speaking payload) = object
        [ "op" .= (5 :: Int)
        , "d"  .= object
            [ "speaking" .=
                ( fromEnum (speakingPayloadMicrophone payload)
                + fromEnum (speakingPayloadSoundshare payload) * 2
                + fromEnum (speakingPayloadPriority payload) * 4
                )
            , "delay"    .= speakingPayloadDelay payload
            , "ssrc"     .= speakingPayloadSSRC payload
            ]
        ]
    toJSON (Resume gid session token) = object
        [ "op" .= (7 :: Int)
        , "d"  .= object
            [ "server_id"  .= gid
            , "session_id" .= session
            , "token"      .= token
            ]
        ]
