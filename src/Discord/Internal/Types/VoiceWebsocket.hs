module Discord.Internal.Types.VoiceWebsocket where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.ByteString as B

import           Discord.Internal.Types.Prelude

data VoiceWebsocketReceivable
    = Ready ReadyPayload                            -- Opcode 2
    | SessionDescription T.Text [B.ByteString]      -- Opcode 4
    | Speaking SpeakingPayload                      -- Opcode 5
    | HeartbeatAck                                  -- Opcode 6
    | Hello Integer                                 -- Opcode 8
    | Resumed                                       -- Opcode 9

data VoiceWebsocketSendable
    = Identify IdentifyPayload                      -- Opcode 0
    | SelectProtocol SelectProtocolPayload          -- Opcode 1
    | Heartbeat Integer                             -- Opcode 3
    | Speaking SpeakingPayload                      -- Opcode 5
    | Resume GuildId T.Text T.Text                  -- Opcode 7

data ReadyPayload = ReadyPayload
    { readyPayloadSSRC  :: Integer -- contains the 32-bit SSRC identifier
    , readyPayloadIP    :: T.Text
    , readyPayloadPort  :: Integer
    , readyPayloadModes :: [T.Text]
    -- , readyPayloadHeartbeatInterval <- This should not be used, as per Discord documentation
    }
    deriving (Show, Eq)

data SpeakingPayload = SpeakingPayload
    { speakingPayloadMicrophone :: Bool
    , speakingPayloadSoundshare :: Bool
    , speakingPayloadPriority   :: Bool
    , speakingPayloadDelay      :: Integer
    , speakingPayloadSSRC       :: Integer
    }
    deriving (Show, Eq)

data IdentifyPayload = IdentifyPayload
    { identifyPayloadServerId  :: GuildId
    , identifyPayloadUserId    :: UserId
    , identifyPayloadSessionId :: T.Text
    , identifyPayloadToken     :: T.Text
    }
    deriving (Show, Eq)

data SelectProtocolPayload = SelectProtocolPayload
    { selectProtocolPayloadProtocol :: T.Text
    , selectProtocolPayloadIP       :: T.Text
    , selectProtocolPayloadPort     :: Integer
    , selectProtocolPayloadMode     :: T.Text
    }
    deriving (Show, Eq)

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
                pure $ ReadyPayload ssrc ip port modes
            4 -> do
                od <- o .: "d"
                mode <- od .: "mode"
                secretKey <- od .: "secret_key"
                pure $ SessionDescription mode secretKey
            5 -> do
                od <- o .: "d"
                speaking <- od .: "speaking"
                (priority, rest1) = speaking `divMod` 4
                (soundshare, rest2) = rest1 `divMod` 2
                microphone = rest2
                delay <- od .: "delay"
                ssrc <- od .: "ssrc"
                pure $ SpeakingPayload
                    { speakingPayloadMicrophone = toEnum microphone
                    , speakingPayloadSoundshare = toEnum soundshare
                    , speakingPayloadPriority   = toEnum priority
                    , speakingPayloadDelay      = delay
                    , speakingPayloadSSRC       = ssrc
                    }
            6 -> pure HeartbeatAck
            8 -> do
                od <- o .: "d"
                interval <- od .: "heartbeat_interval"
                pure $ Hello interval
            9 -> pure Resumed
            _ -> fail $ "Unknown Voice Websocket Receivable, opcode " <> show op

instance ToJSON VoiceWebsocketSendable where
    toJSON (Identify payload) = object
        [ "op" .= (0 :: Int)
        , "d"  .= object
            [ "server_id"  .= identifyPayloadServerId payload
            , "user_id"    .= identifyPayloadUserId payload
            , "session_id" .= identifyPayloadSessionId payload
            , "token"      .= identifyPayloadToken token
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
        , "d"  .= if i < 0 then "null" else show i
        ]
    toJSON (Speaking payload) = object
        [ "op" .= (5 :: Int)
        , "d"  .= object
            [ "speaking" .=
                ( speakingPayloadMicrophone payload
                + speakingPayloadSoundshare payload * 2
                + speakingPayloadPriority payload * 4
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
