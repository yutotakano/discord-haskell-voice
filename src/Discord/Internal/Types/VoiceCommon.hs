{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Discord.Internal.Types.VoiceCommon where

import Control.Concurrent (Chan, MVar, ThreadId)
import Control.Concurrent.BoundedChan qualified as Bounded
import Control.Lens (makeFields)
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString qualified as B
import Data.Text qualified as T
import Discord
import Discord.Types
import Discord.Internal.Gateway.EventLoop ( GatewayException(..) )
import Discord.Internal.Types.VoiceUDP
import Discord.Internal.Types.VoiceWebsocket
import Network.Socket
import Network.WebSockets (ConnectionException, Connection)
import GHC.Weak (Weak)

-- | ExceptT is on the base so threads in state can be killed as necessary.
-- If it is ReaderT on the base, then when an error occurs, it will be caught by
-- the ExceptT and returned as an Either into the ReaderT monad, but what we want
-- to do most of the time is to kill the thread stored in the reader state.
-- Mutating the state is not possible, nor is it safe to kill a thread which the
-- PID is still actively stored in a reader. So we use ExceptT on the base, so
-- the error is propagated there, and since there is no reader capturing the
-- state, the thread can be killed.
type Voice a =
    ReaderT DiscordBroadcastHandle (ExceptT VoiceError DiscordHandler) a

data VoiceError
    = VoiceNotAvailable
    | NoServerAvailable
    | InvalidPayloadOrder
    deriving (Show, Eq)

-- | Represents a voice connection handle to a specific voice channel.
data DiscordVoiceHandle = DiscordVoiceHandle
    { -- | The guild id of the voice channel.
      discordVoiceHandleGuildId :: GuildId
    , -- | The channel id of the voice channel.
      discordVoiceHandleChannelId :: ChannelId
    , -- | The websocket thread id and handle.
      discordVoiceHandleWebsocket :: (Weak ThreadId, (VoiceWebsocketReceiveChan, VoiceWebsocketSendChan))
    , -- | The UDP thread id and handle.
      discordVoiceHandleUdp :: (Weak ThreadId, (VoiceUDPReceiveChan, VoiceUDPSendChan))
    , -- | The SSRC of the voice connection, specified by Discord.
      discordVoiceHandleSSRC :: Integer
    }

{- | Represents a "stream" or a "broadcast", which is a list of voice connection
 handles that share the same audio stream.
-}
data DiscordBroadcastHandle = DiscordBroadcastHandle
    { -- | The list of voice connection handles.
      discordBroadcastHandleVoiceHandles :: MVar [DiscordVoiceHandle]
    , -- | The mutex used to synchronize access to the list of voice connection
      discordBroadcastHandleMutEx :: MVar ()
    , -- | The channel used to send audio data to the audio stream.
      discordBroadcastHandleSends :: Bounded.BoundedChan B.ByteString
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

type VoiceUDPReceiveChan = Chan VoiceUDPPacket

type VoiceUDPSendChan = Bounded.BoundedChan B.ByteString

data WebsocketLaunchOpts = WebsocketLaunchOpts
    { websocketLaunchOptsBotUserId     :: UserId
    , websocketLaunchOptsSessionId     :: T.Text
    , websocketLaunchOptsToken         :: T.Text
    , websocketLaunchOptsGuildId       :: GuildId
    , websocketLaunchOptsEndpoint      :: T.Text
    , websocketLaunchOptsGatewayEvents :: Chan (Either GatewayException Event)
    , websocketLaunchOptsWsHandle      :: (VoiceWebsocketReceiveChan, VoiceWebsocketSendChan)
    , websocketLaunchOptsUdpInfo       :: (MVar (Weak ThreadId), MVar (VoiceUDPReceiveChan, VoiceUDPSendChan))
    , websocketLaunchOptsSsrc          :: MVar Integer
    }

data WebsocketConn = WebsocketConn
    { websocketConnConnection    :: Connection
    , websocketConnLaunchOpts    :: WebsocketLaunchOpts
    }

data UDPLaunchOpts = UDPLaunchOpts
    { udpLaunchOptsSsrc :: Integer
    , udpLaunchOptsIp   :: T.Text
    , udpLaunchOptsPort :: Integer
    , udpLaunchOptsMode :: T.Text
    }

data UDPConn = UDPConn
    { udpConnLaunchOpts :: UDPLaunchOpts
    , udpConnSocket     :: Socket
    }

$(makeFields ''DiscordVoiceHandle)
$(makeFields ''DiscordBroadcastHandle)
$(makeFields ''WebsocketLaunchOpts)
$(makeFields ''WebsocketConn)
$(makeFields ''UDPLaunchOpts)
$(makeFields ''UDPConn)
