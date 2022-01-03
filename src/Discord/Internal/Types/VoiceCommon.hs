{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-|
Module      : Discord.Internal.Types.VoiceCommon
Description : Strictly for internal use only. See Discord.Voice for the public interface.
Copyright   : (c) Yuto Takano (2021)
License     : MIT
Maintainer  : moa17stock@gmail.com

= WARNING

This module is considered __internal__.

The Package Versioning Policy __does not apply__.

The contents of this module may change __in any way whatsoever__ and __without__
__any warning__ between minor versions of this package.

= Description

This module defines the types for handles, errors, base monads, and other types
applicable to both the UPD and Websocket components of the Voice API. Many of
the structures defined in this module have Lenses derived for them using
Template Haskell.
-}
module Discord.Internal.Types.VoiceCommon where

import Control.Concurrent ( Chan, MVar, ThreadId )
import Control.Concurrent.BoundedChan qualified as Bounded
import Control.Exception.Safe ( Exception )
import Control.Lens ( makeFields )
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Word ( Word8 )
import GHC.Weak ( Weak )
import Network.Socket
import Network.WebSockets ( ConnectionException, Connection )

import Discord
import Discord.Types
import Discord.Internal.Gateway.EventLoop ( GatewayException(..) )
import Discord.Internal.Types.VoiceUDP
import Discord.Internal.Types.VoiceWebsocket

-- | @Voice@ is a type synonym for a ReaderT and ExceptT composition of monad
-- transformers over the @DiscordHandler@ monad. It holds references to
-- voice connections/threads. The content of the reader handle is strictly
-- internal, and is entirely subject to change irrespective of the Package
-- Versioning Policy. @Voice@ is still provided as a type synonym rather than a
-- @newtype@ to take advantage of existing instances for various type-classes.
--
-- Developer Note: ExceptT is on the base rather than ReaderT, so that when a
-- critical exception/error occurs in @Voice@, it can propagate down the
-- transformer stack, kill the threads referenced in the Reader state as
-- necessary, and halt the entire computation and return to @DiscordHandler@.
-- If ExceptT were on top of ReaderT, then errors would be swallowed before it
-- propagates below ReaderT, and the monad would not halt there, continuing
-- computation with an unstable state.
type Voice =
    ReaderT DiscordBroadcastHandle (ExceptT VoiceError DiscordHandler)

-- | @VoiceError@ represents the potential errors when initialising a voice
-- connection. It does /not/ account for errors that occur after the initial
-- handshake (technically, because they are in IO and not ExceptT).
data VoiceError
    = VoiceNotAvailable
    | NoServerAvailable
    | InvalidPayloadOrder
    deriving (Show, Eq)

-- | @SubprocessException@ is an Exception that may be thrown when a subprocess
-- such as FFmpeg encounters an error.
--
-- TODO: This has never actually been seen, so it's untested whether it works.
data SubprocessException = SubprocessException String deriving (Eq, Show)
instance Exception SubprocessException

-- | @DiscordVoiceHandle@ represents the handles for a single voice connection
-- (to a specific voice channel).
--
-- Lenses are defined for this type using Template Haskell.
data DiscordVoiceHandle = DiscordVoiceHandle
    { discordVoiceHandleGuildId :: GuildId
      -- ^ The guild id of the voice channel.
    , discordVoiceHandleChannelId :: ChannelId
      -- ^ The channel id of the voice channel.
    , discordVoiceHandleWebsocket :: (Weak ThreadId, (VoiceWebsocketReceiveChan, VoiceWebsocketSendChan))
      -- ^ The websocket thread id and handle.
    , discordVoiceHandleUdp :: (Weak ThreadId, (VoiceUDPReceiveChan, VoiceUDPSendChan))
      -- ^ The UDP thread id and handle.
    , discordVoiceHandleSsrc :: Integer
      -- ^ The SSRC of the voice connection, specified by Discord. This is
      -- required in the packet sent when updating the Speaking indicator, so is
      -- maintained in this handle.
    }

-- | @DiscordBroadcastHandle@ represents a "stream" or a "broadcast", which is
-- a mutable list of voice connection handles that share the same audio stream.
--
-- Lenses are defined for this type using Template Haskell.
data DiscordBroadcastHandle = DiscordBroadcastHandle
    { discordBroadcastHandleVoiceHandles :: MVar [DiscordVoiceHandle]
      -- ^ The list of voice connection handles.
    , discordBroadcastHandleMutEx :: MVar ()
      -- ^ The mutex used to synchronize access to the list of voice connection
    }

-- | Deprecated.
-- TODO: remove, unused
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

-- | @WebsocketLaunchOpts@ represents all the data necessary to start a
-- Websocket connection to Discord's Voice Gateway.
--
-- Lenses are defined for this type using Template Haskell.
data WebsocketLaunchOpts = WebsocketLaunchOpts
    { websocketLaunchOptsBotUserId     :: UserId
    , websocketLaunchOptsSessionId     :: T.Text
    , websocketLaunchOptsToken         :: T.Text
    , websocketLaunchOptsGuildId       :: GuildId
    , websocketLaunchOptsEndpoint      :: T.Text
    , websocketLaunchOptsGatewayEvents :: Chan (Either GatewayException Event)
    , websocketLaunchOptsWsHandle      :: (VoiceWebsocketReceiveChan, VoiceWebsocketSendChan)
    , websocketLaunchOptsUdpTid        :: MVar (Weak ThreadId)
    , websocketLaunchOptsUdpHandle     :: (VoiceUDPReceiveChan, VoiceUDPSendChan)
    , websocketLaunchOptsSsrc          :: MVar Integer
    }

-- | @WebsocketConn@ represents an active connection to Discord's Voice Gateway
-- websocket, and contains the Connection as well as the options that launched
-- it.
--
-- Lenses are defined for this type using Template Haskell.
data WebsocketConn = WebsocketConn
    { websocketConnConnection    :: Connection
    , websocketConnLaunchOpts    :: WebsocketLaunchOpts
    }

-- | @UDPLaunchOpts@ represents all the data necessary to start a UDP connection
-- to Discord. Field names for this ADT are cased weirdly because I want to keep
-- the "UDP" part uppercase in the type and data constructor. Since field
-- accessors are rarely used anyway (lenses are preferred instead), we can
-- write the field prefixes as "uDP" and take advantage of Lenses as normal.
-- 
-- Lenses are defined for this type using Template Haskell.
data UDPLaunchOpts = UDPLaunchOpts
    { uDPLaunchOptsSsrc :: Integer
    , uDPLaunchOptsIp   :: T.Text
    , uDPLaunchOptsPort :: Integer
    , uDPLaunchOptsMode :: T.Text
    , uDPLaunchOptsUdpHandle :: (VoiceUDPReceiveChan, VoiceUDPSendChan)
    , uDPLaunchOptsSecretKey :: MVar [Word8]
    }

-- | @UDPConn@ represents an active UDP connection to Discord, and contains the
-- Socket as well as the options that launched it.
--
-- Lenses are defined for this type using Template Haskell.
data UDPConn = UDPConn
    { uDPConnLaunchOpts :: UDPLaunchOpts
    , uDPConnSocket     :: Socket
    }

$(makeFields ''DiscordVoiceHandle)
$(makeFields ''DiscordBroadcastHandle)
$(makeFields ''WebsocketLaunchOpts)
$(makeFields ''WebsocketConn)
$(makeFields ''UDPLaunchOpts)
$(makeFields ''UDPConn)
