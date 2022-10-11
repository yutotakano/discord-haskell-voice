{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : Discord.Internal.Types.VoiceWebsocket
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
Voice Gateway. Some ToJSON and FromJSON instances are defined, as according to
the official Discord documentation for v4 of the gateway.

Prisms are defined using TemplateHaskell for VoiceWebsocketReceivable.
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

_Ready :: Traversal' VoiceWebsocketReceivable ReadyPayload
_Ready f (Ready rp) = Ready <$> f rp
_Ready _ rp = pure rp

_SessionDescription :: Traversal' VoiceWebsocketReceivable (T.Text, [Word8])
_SessionDescription f (SessionDescription t bytes) = uncurry SessionDescription <$> f (t, bytes)
_SessionDescription _ sd = pure sd

_Hello :: Traversal' VoiceWebsocketReceivable Int
_Hello f (Hello a) = Hello <$> f a
_Hello _ a = pure a


data VoiceWebsocketSendable
    = Identify IdentifyPayload                      -- Opcode 0
    | SelectProtocol SelectProtocolPayload          -- Opcode 1
    | Heartbeat Int                                 -- Opcode 3
      -- ^ Int because threadDelay uses it
    | Speaking SpeakingPayload                      -- Opcode 5
    | Resume GuildId T.Text T.Text                  -- Opcode 7
    deriving stock (Show, Eq)

data ReadyPayload = ReadyPayload
    { readyPayloadSSRC  :: Integer -- contains the 32-bit SSRC identifier
    , readyPayloadIP    :: T.Text
    , readyPayloadPort  :: Integer
    , readyPayloadModes :: [T.Text]
    -- , readyPayloadHeartbeatInterval <- This should not be used, as per Discord documentation
    }
    deriving stock (Show, Eq)

data SpeakingPayload = SpeakingPayload
    { speakingPayloadMicrophone :: Bool
    , speakingPayloadSoundshare :: Bool
    , speakingPayloadPriority   :: Bool
    , speakingPayloadDelay      :: Integer
    , speakingPayloadSSRC       :: Integer
    }
    deriving stock (Show, Eq)

data IdentifyPayload = IdentifyPayload
    { identifyPayloadServerId  :: GuildId
    , identifyPayloadUserId    :: UserId
    , identifyPayloadSessionId :: T.Text
    , identifyPayloadToken     :: T.Text
    }
    deriving stock (Show, Eq)

data SelectProtocolPayload = SelectProtocolPayload
    { selectProtocolPayloadProtocol :: T.Text
    , selectProtocolPayloadIP       :: T.Text
    , selectProtocolPayloadPort     :: Integer
    , selectProtocolPayloadMode     :: T.Text
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

-- $(makePrisms ''VoiceWebsocketReceivable)
