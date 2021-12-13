{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Discord.Internal.Types.Common where

import Control.Concurrent (Chan, MVar, ThreadId)
import Control.Concurrent.BoundedChan qualified as Bounded
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString qualified as B
import Data.Text qualified as T
import Discord
import Discord.Internal.Types
import Discord.Internal.Types.VoiceUDP
import Discord.Internal.Types.VoiceWebsocket
import Network.Socket
import Network.WebSockets (ConnectionException, Connection)

-- | ExceptT outside so threads in state can be killed as necessary.
-- If it is ReaderT state (ExceptT error handler) a, then when an error occurs,
-- it will be caught by the ExceptT and returned as an Either into the ReaderT
-- monad, but what we want to do most of the time is to kill the thread stored
-- in the reader state. Mutating the state is not possible, nor is it safe to
-- kill a thread which the PID is still actively stored in a reader. So we use
-- ExceptT outside, so the error is propagated there, and since there is no
-- reader capturing the state, the thread can be killed.
type Voice a =
    ExceptT VoiceError (ReaderT DiscordMultiVoiceHandle DiscordHandler) a

data VoiceError
    = VoiceNotAvailable
    | NoServerAvailable
    | InvalidPayloadOrder
    deriving (Show, Eq)

-- | Represents a voice connection handle to a specific voice channel.
data DiscordVoiceHandle = DiscordVoiceHandle
    { -- | The guild id of the voice channel.
      dvGuildId :: GuildId
    , -- | The channel id of the voice channel.
      dvChannelId :: ChannelId
    , -- | The websocket thread id and handle.
      dvWebsocket :: (ThreadId, DiscordVoiceHandleWebsocket)
    , -- | The UDP thread id and handle.
      dvUDP :: (ThreadId, DiscordVoiceHandleUDP)
    , -- | The SSRC of the voice connection, specified by Discord.
      dvSSRC :: Integer
    }

{- | Represents a "stream" or a "broadcast", which is a list of voice connection
 handles that share the same audio stream.
-}
data DiscordMultiVoiceHandle = DiscordMultiVoiceHandle
    { -- | The list of voice connection handles.
      dmvVoiceHandles :: MVar [DiscordVoiceHandle]
    , -- | The mutex used to synchronize access to the list of voice connection
      dmvMutEx :: MVar ()
    , -- | The channel used to send audio data to the audio stream.
      dmvSends :: Bounded.BoundedChan B.ByteString
    }

data VoiceWebsocketException
    = VoiceWebsocketCouldNotConnect T.Text
    | VoiceWebsocketEventParseError T.Text
    | VoiceWebsocketUnexpected VoiceWebsocketReceivable T.Text
    | VoiceWebsocketConnection ConnectionException T.Text
    deriving (Show)

type VoiceWebsocketReceiveChan =
    Chan (Either VoiceWebsocketException VoiceWebsocketReceivable)

type VoiceWebsocketSendChan = Chan VoiceWebsocketSendable

type DiscordVoiceHandleWebsocket =
    (VoiceWebsocketReceiveChan, VoiceWebsocketSendChan)

type DiscordVoiceHandleUDP =
    ( Chan VoiceUDPPacket -- can receive various stuff
    , Bounded.BoundedChan B.ByteString -- but can only send audio
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

-- | session_id, token, guild_id, endpoint
data WebsocketConnInfo = WSConnInfo
    { wsInfoSessionId :: T.Text
    , wsInfoToken :: T.Text
    , wsInfoGuildId :: GuildId
    , wsInfoEndpoint :: T.Text
    }

data WebsocketConn = WSConn
    { wsDataConnection :: Connection
    , wsDataConnInfo :: WebsocketConnInfo
    , wsDataReceivesChan ::
        Chan (Either VoiceWebsocketException VoiceWebsocketReceivable)
    }
