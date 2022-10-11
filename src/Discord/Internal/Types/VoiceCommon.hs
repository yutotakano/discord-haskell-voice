{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : Discord.Internal.Types.VoiceCommon
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

This module defines the types for handles, errors, base monads, and other types
applicable to both the UPD and Websocket components of the Voice API. Many of
the structures defined in this module have Lenses derived for them using
Template Haskell.
-}
module Discord.Internal.Types.VoiceCommon
    ( module Discord.Internal.Types.VoiceCommon
    ) where

import Conduit
import Control.Concurrent ( Chan, MVar, ThreadId )
import Control.Concurrent.BoundedChan qualified as Bounded
import Control.Exception.Safe ( Exception, MonadMask, MonadCatch )
import Lens.Micro.TH ( makeFields )
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.List.NonEmpty
import Data.Text qualified as T
import Data.Word ( Word8 )
import GHC.Weak ( Weak )
import Network.Socket
import Network.WebSockets ( ConnectionException, Connection )

import Discord
import Discord.Types
import Discord.Internal.Types.VoiceUDP
import Discord.Internal.Types.VoiceWebsocket

-- | @Voice@ is a newtype Monad containing a composition of ReaderT and ExceptT
-- transformers over the @DiscordHandler@ monad. It holds references to
-- voice connections/threads. The content of the reader handle is strictly
-- internal and is hidden deliberately behind the newtype wrapper.
--
-- Developer Note: ExceptT is on the base rather than ReaderT, so that when a
-- critical exception/error occurs in @Voice@, it can propagate down the
-- transformer stack, kill the threads referenced in the Reader state as
-- necessary, and halt the entire computation and return to @DiscordHandler@.
-- If ExceptT were on top of ReaderT, then errors would be swallowed before it
-- propagates below ReaderT, and the monad would not halt there, continuing
-- computation with an unstable state.
newtype Voice a = Voice
    { unVoice :: ReaderT DiscordBroadcastHandle (ExceptT VoiceError DiscordHandler) a
    } deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    -- ^ MonadIO gives the ability to perform 'liftIO'.
    , MonadReader DiscordBroadcastHandle
    -- ^ MonadReader is for internal use, to read the held broadcast handle.
    , MonadError VoiceError
    -- ^ MonadError is for internal use, to propagate errors.
    , MonadFail
    -- ^ MonadFail is for internal use, identical in function to the MonadFail
    -- instance of ReaderT.
    , MonadThrow
    -- ^ MonadThrow, MonadCatch, and MonadMask are for internal use, to utilise
    -- exception handling functions like @bracket@.
    , MonadCatch
    , MonadMask
    )

data FFmpegFilter
    = Reverb Int
    -- ^ Integer is milliseconds to delay the echo. 0 - 50 is the ideal range.

data AudioCodec
    = OPUSCodec
    -- ^ The audio is in OPUS format which can be directly sent to Discord.
    | PCMCodec
    -- ^ The audio is in PCM format which can be encoded and sent to Discord.
    | ProbeCodec String
    -- ^ The audio is in an unknown format, but we want to probe it using
    -- `ffprobe` (executable name specified) first to determine the best
    -- pipeline for the audio. This can sometimes result in more efficient
    -- audio pipelines with minimal transcoding, at the expense of an initial
    -- delay probing for the codec.
    | UnknownCodec
    -- ^ The audio is in an unknown format, and we want to unconditionally
    -- run FFmpeg on it to convert it to OPUS which we can send to Discord.
    deriving stock (Eq)

data AudioCodecNoFurtherFFmpeg
    = OPUSFinalOutput
    | PCMFinalOutput
    deriving stock (Eq)

-- | The pipeline of the audio processing before it is given to the Haskell side.
data AudioPipeline = AudioPipeline
    { audioPipelineNeedsFFmpeg :: Bool
    -- ^ Whether the pipeline needs FFmpeg to do transformations or transcoding.
    , audioPipelineOutputCodec :: AudioCodecNoFurtherFFmpeg
    -- ^ What format the pipeline will output its audio. If FFmpeg is required,
    -- this is the codec that FFmpeg will output. If FFmpeg is not required, this
    -- is the original codec of the file/input.
    }

data AudioTransformation
    = FFmpegTransformation (NonEmpty FFmpegFilter)
    | HaskellTransformation (ConduitT B.ByteString B.ByteString (ResourceT DiscordHandler) ())
    | (NonEmpty FFmpegFilter) ::.: (ConduitT B.ByteString B.ByteString (ResourceT DiscordHandler) ())

instance Show AudioTransformation where
    show (FFmpegTransformation _filters) = "<FFmpeg Transformations>"
    show (HaskellTransformation _conduit) = "<Haskell Transformations>"
    show (_a ::.: _b) = "<FFmpeg Transformations> ::.: <Haskell Transformations>"

-- | Datatype to use for playing stuff
data AudioResource = AudioResource
    { audioResourceStream :: Either String BL.ByteString
    -- ^ The stream source. If it's Left, the value is a URL or filepath that
    -- can be passed to ffmpeg. If it is Right, it contains the lazy ByteString
    -- for the entire audio.
    , audioResourceYouTubeDLInfo :: Maybe Object
    -- ^ If this audio resource was created through youtube-dl, then other
    -- metadata listed by youtube-dl.
    , audioResourceTransform :: Maybe AudioTransformation
    -- ^ Any transformations to perform on the audio resource, both via ffmpeg
    -- filters and via ByteString Conduits.
    }
    deriving stock Show

-- | @VoiceError@ represents the potential errors when initialising a voice
-- connection. It does /not/ account for errors that occur after the initial
-- handshake (technically, because they are in IO and not ExceptT).
data VoiceError
    = VoiceNotAvailable
    | NoServerAvailable
    | InvalidPayloadOrder
    deriving stock (Show, Eq)

-- | @SubprocessException@ is an Exception that may be thrown when a subprocess
-- such as FFmpeg encounters an error.
--
-- TODO: This has never actually been seen, so it's untested whether it works.
data SubprocessException = SubprocessException String deriving stock (Eq, Show)
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
    deriving stock (Show)

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

$(makeFields ''AudioPipeline)
$(makeFields ''AudioResource)
$(makeFields ''DiscordVoiceHandle)
$(makeFields ''DiscordBroadcastHandle)
$(makeFields ''WebsocketLaunchOpts)
$(makeFields ''WebsocketConn)
$(makeFields ''UDPLaunchOpts)
$(makeFields ''UDPConn)
