{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-|
Module      : Discord.Internal.Types.VoiceCommon
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

This module defines the types for handles, errors, base monads, and other types
applicable to both the UDP and Websocket components of the Voice API. Many of
the structures defined in this module have Lenses derived for them using
Template Haskell for easier field access.
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

-- | @Voice@ is a newtype Monad containing a ReaderT transformer over the
-- @'Discord.DiscordHandler'@ monad. You can think of it as another layer of
-- possible actions: The first layer is 'Discord.DiscordHandler' which gives you
-- Discord API capabilities on top of IO, and the second layer is 'Voice' which
-- gives you voice capabilities on top of 'DiscordHandler'. As such, you can
-- only run a 'Voice' monad computation from within 'DiscordHandler' using a
-- function called 'Discord.Voice.runVoice'.
--
-- This monad's ReaderT transformer holds references to voice connections and
-- thread handles. The content of the reader handle is strictly
-- internal and is hidden deliberately behind the newtype wrapper. If you'd like
-- experiment with the internals, importing "Discord.Internal.Types.VoiceCommon"
-- will give you access to the 'unVoice' function to unwrap the internal handles.
-- Please submit pull requests on GitHub for any cool functionality you've built
-- that might fit the library!
newtype Voice a = Voice
    { unVoice :: ReaderT DiscordBroadcastHandle DiscordHandler a
    } deriving newtype -- we use the newtype strategy so it's mostly ReaderT
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    -- ^ MonadIO gives the ability to perform 'liftIO' and run IO actions. To
    -- run 'DiscordHandler' actions from within Voice, use
    -- 'Discord.Voice.liftDiscord'.
    , MonadUnliftIO
    -- ^ MonadUnliftIO gives us the ability to come back into the monadic
    -- context from within a lifted IO context. We don't need it in internal
    -- code, but the instance is provided to make it more convenient for users
    -- to use e.g. UnliftIO.forkIO from inside Voice without having to manually
    -- unwrap and capture state.
    , MonadReader DiscordBroadcastHandle
    -- ^ MonadReader is for internal use, to read the held broadcast handle.
    , MonadFail
    -- ^ MonadFail is for internal use, identical in function to the MonadFail
    -- instance of ReaderT.
    , MonadThrow
    -- ^ MonadThrow, MonadCatch, and MonadMask are for internal use, to utilise
    -- exception handling functions like @bracket@.
    , MonadCatch
    , MonadMask
    )

-- | A type to hint the library at your audio resource's codec. This is used with
-- 'Discord.Voice.play' to guide the library about the audio codec if you
-- already know it, which might prevent unnecessary transcoding or probing. For
-- example, if you already have a bytestream or file that is known to be encoded
-- with Opus/Ogg, you can choose 'OPUSCodec'. For web media and other unknown
-- sources, a safe choice is either 'UnknownCodec' or 'ProbeCodec'.
--
-- Note that choosing 'OPUSCodec' or 'PCMCodec' doesn't necessarily exclude the
-- possibility of transcoding using FFmpeg, since you may have specified some
-- audio transformations (like custom FFmpeg flags) which will need FFmpeg. The
-- purpose of hinting the library of the codec using this datatype is so that in
-- the best-case, when you haven't specified any transformation, we don't have
-- to shell out to FFmpeg.
data AudioCodec
    = OPUSCodec
    -- ^ The audio is known to be in OPUS format which can be directly sent to
    -- Discord if there are no transformations to apply.
    | PCMCodec
    -- ^ The audio is known to be in 16-bit little-endian PCM format which can
    -- be encoded within the library and sent to Discord. You will require
    -- @libopus@ to be installed. See the library README which should guide to
    -- you to installation instructions for each OS.
    | ProbeCodec String
    -- ^ The audio is in an unknown format, but we want to probe it using
    -- @ffprobe@ (specifying the execuable name) first to determine the best
    -- codec hint automatically. For example, @ffprobe@ can discover that the
    -- resource is already in Opus format, in which case the behaviour becomes
    -- identical to 'OPUSCodec'. This can sometimes result in more efficient
    -- audio processing with minimal transcoding, but requires an initial delay
    -- to probe the codec.
    | UnknownCodec
    -- ^ The audio is in an unknown format, and we want to unconditionally
    -- run FFmpeg on it to convert it to OPUS which we can send to Discord. This
    -- is a safe bet for every purpose. It requires @ffmpeg@ to be installed.
    deriving stock (Eq)

-- | A datatype representing whether an audio transformation pipeline segment
-- should finish with Opus or PCM bytes before reaching into the Haskell side.
-- If we are using FFmpeg, this refers to what FFmpeg should output. If we
-- aren't using FFmpeg, this represents what the input file codec is, since that
-- is what will be processed in the Haskell side.
data AudioCodecNoFurtherFFmpeg
    = OPUSFinalOutput
    -- ^ The pipeline segment ends in Opus format. Usually this means we'll just
    -- send the output to Discord afterwards.
    | PCMFinalOutput
    -- ^ The pipeline segment ends in PCM format. Usually this means we have
    -- some transformation to operate on PCM before encoding it and sending it
    -- to Discord within the library.
    deriving stock (Eq)

-- | The pipeline of the audio processing before it is given to the Haskell side.
--
-- Lenses are defined for this type using Template Haskell. You can use them
-- to make accessing fields easier, like:
-- @
-- need = pipeline ^. needFFmpeg
-- @
data AudioPipeline = AudioPipeline
    { audioPipelineNeedsFFmpeg :: Bool
    -- ^ Whether the pipeline needs FFmpeg to do transformations or transcoding.
    , audioPipelineOutputCodec :: AudioCodecNoFurtherFFmpeg
    -- ^ What format the pipeline will output its audio. If FFmpeg is required,
    -- this is the codec that FFmpeg will output. If FFmpeg is not required, this
    -- is the original codec of the file/input.
    }

-- | Possible transformations to apply to the audio resource. You can choose
-- an 'FFmpegTransformation', which is essentially just arbitrary flags to pass
-- to FFmpeg, or 'HaskellTransformation', which is a conduit to operate on PCM
-- bytes. You can also do a combination.
data AudioTransformation
    = FFmpegTransformation (FilePath -> [String])
    -- ^ Transform the audio using FFmpeg with custom arguments. The no-op
    -- transformation here is to use 'Discord.Voice.defaultFfmpegArgs' as the
    -- transformation.
    --
    -- By using 'FFmpegTransformation', the audio resource will unconditionally
    -- go through FFmpeg (and thus requires @ffmpeg@ to be installed), even if
    -- your resource is already in Opus format, and even if you specified
    -- 'OpusCodec' or 'PCMCodec'.
    | HaskellTransformation (ConduitT B.ByteString B.ByteString (ResourceT DiscordHandler) ())
    -- ^ Transform the audio using a Haskell conduit that operates on PCM audio.
    -- We recommend using this in conjunction with
    -- 'Discord.Voice.Conduit.packInt16CT' and 'Discord.Voice.Conduit.unpackInt16CT',
    -- so that you can operate on each 16-bit sample at a time instead of a byte
    -- at a time.
    --
    -- By using 'HaskellTransformation', the audio resource will be transcoded
    -- to PCM if it wasn't already, which means it will go through FFmpeg (and
    -- thus requires @ffmpeg@ to be installed) if you didn't specify 'PCMCodec'.
    | (FilePath -> [String]) :.->: (ConduitT B.ByteString B.ByteString (ResourceT DiscordHandler) ())
    -- ^ Transform the audio using a FFmpeg with custom arguments first, then
    -- make FFmpeg output PCM bytes and apply a custom Haskell conduit
    -- transformation on the result. This is a combination of
    -- 'FFmpegTransformation' and 'HaskellTransformation', and will
    -- unconditionally shell out into FFmpeg (and thus requires @ffmpeg@ to be
    -- installed) even if the source is PCM or Opus.

-- | The show instance is just for debugging purposes: it does not show the
-- actual contents of the transformation, and is thus unlawful (the output of
-- 'show' cannot be parsed back using 'read').
instance Show AudioTransformation where
    show (FFmpegTransformation func) = "<FFmpeg Flags Transformation>"
    show (HaskellTransformation _conduit) = "<Haskell Conduit Byte Transformations>"
    show (_a :.->: _b) = "<FFmpeg Flags Transformation> :FollowedBy: <Haskell Conduit Byte Transformations>"

-- | A data type that represents an audio resource to be played. You can
-- construct this type using functions such as
-- 'Discord.Voice.createYoutubeResource' or 'Discord.Voice.createFileResource'.
--
-- Lenses are defined for this type using Template Haskell. You can use them
-- to make accessing fields easier, like:
-- @
-- metadata = res ^. youtubeDLInfo
-- @
data AudioResource = AudioResource
    { audioResourceStream :: Either String BL.ByteString
    -- ^ The stream source. If it's Left, the value is a URL or filepath that
    -- will be passed to FFmpeg. If it is Right, it contains the lazy ByteString
    -- for the entire audio.
    , audioResourceYouTubeDLInfo :: Maybe Object
    -- ^ If this audio resource was created through yt-dlp using
    -- 'Discord.Voice.createYoutubeResource', then this field will be Just, and
    -- will contain JSON metadata returned by yt-dlp, such as the uploader,
    -- date, etc. We make no promises on the specific keys or data present in
    -- this field, as it's entirely populated by yt-dlp -- please see their
    -- documentation for the output of the @-j@ flag (which we use).
    --
    -- Use Aeson functions to parse the keys in this field. For example, to find
    -- the URL of the resource (which is also contained in 'audioResourceStream',
    -- run:
    --
    -- @
    -- url :: Maybe String
    -- url = flip parseMaybe (audioResourceYouTubeDLInfo res) $ \o -> o .: "url"
    -- @
    , audioResourceTransform :: Maybe AudioTransformation
    -- ^ Any transformations to perform on the audio resource.
    }
    deriving stock Show

-- | @VoiceError@ represents the potential errors when initialising a voice
-- connection. It does /not/ account for errors that occur after the initial
-- handshake (technically, because they are in IO and not ExceptT).
data VoiceError
    = VoiceNotAvailable
    | NoServerAvailable
    deriving stock (Show, Eq)

instance Exception VoiceError

-- | @DiscordVoiceHandle@ represents the handles for a single voice connection
-- to a specific voice channel. This is all the information that is maintained
-- by the main thread, and contains thread ID and bidirectional communication
-- channels for Voice Websocket and Voice UDP threads.
--
-- Lenses are defined for this type using Template Haskell. You can use them
-- to make accessing fields easier, like:
-- @
-- mySSRC = handle ^. ssrc
-- @
data DiscordVoiceHandle = DiscordVoiceHandle
    { discordVoiceHandleGuildId :: GuildId
      -- ^ The guild id of the voice channel.
    , discordVoiceHandleChannelId :: ChannelId
      -- ^ The channel id of the voice channel.
    , discordVoiceHandleWebsocket :: (Weak ThreadId, (VoiceWebsocketReceiveChan, VoiceWebsocketSendChan))
      -- ^ The websocket thread id and bidirectional communication handles.
    , discordVoiceHandleUdp :: (Weak ThreadId, (VoiceUDPReceiveChan, VoiceUDPSendChan))
      -- ^ The UDP thread id and bidirectional communication handles.
    , discordVoiceHandleSsrc :: Integer
      -- ^ The SSRC of the voice connection, specified by Discord. This is
      -- required in the packet sent when updating the Speaking indicator, so is
      -- maintained in this handle.
    }

-- | @DiscordBroadcastHandle@ represents a "stream" or a "broadcast", which is
-- a mutable list of voice connection handles that share the same audio stream.
--
-- Lenses are defined for this type using Template Haskell. You can use them
-- to make accessing fields easier, like:
-- @
-- myHandles = broadcastHandle ^. voiceHandles
-- @
data DiscordBroadcastHandle = DiscordBroadcastHandle
    { discordBroadcastHandleVoiceHandles :: MVar [DiscordVoiceHandle]
      -- ^ The list of voice connection handles. Do not modify this list without
      -- having acquired 'discordBroadcastHandleMutEx'.
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

-- | A type alias for a communication channel from the Websocket thread to the
-- main thread, which may contain an error or a received websocket message.
type VoiceWebsocketReceiveChan =
    Chan (Either VoiceWebsocketException VoiceWebsocketReceivable)

-- | A type alias for a communication channel from the main thread to the
-- Websocket thread, which can contain messages to be sent.
type VoiceWebsocketSendChan = Chan VoiceWebsocketSendable

-- | A type alias for a communication channel from the UDP thread to the main
-- thread, which may contain received voice data. Note that we don't support
-- voice receive in this library so this channel is provided only for type
-- completeness and isn't very useful.
type VoiceUDPReceiveChan = Chan VoiceUDPPacket

-- | A type alias for a communication channel from the main thread to the UDP
-- thread, which can contain Opus packets to be sent.
type VoiceUDPSendChan = Bounded.BoundedChan B.ByteString

-- | @WebsocketLaunchOpts@ represents all the data necessary to start the
-- Websocket thread, which will connect to Discord Voice Gateway for the voice
-- control plane.
--
-- Lenses are defined for this type using Template Haskell. You can use them
-- to make accessing fields easier, like:
-- @
-- myId = opts ^. botUserId
-- @
data WebsocketLaunchOpts = WebsocketLaunchOpts
    { websocketLaunchOptsBotUserId     :: UserId
    -- ^ The user ID of the running bot, which is needed for the voice gateway
    -- uplink Opcode 0 Voice Identify.
    , websocketLaunchOptsSessionId     :: T.Text
    -- ^ The session ID obtained from the normal gateway in downlink Opcode 0
    -- Dispatch (event: Voice State Update). We need this for the voice gateway
    -- uplink Opcode 0 Voice Identify.
    , websocketLaunchOptsToken         :: T.Text
    -- ^ The join token obtained from the normal gateway in downlink Opcode 0
    -- Dispatch (event: Voice Server Update). We need this for the voice gateway
    -- uplink Opcode 0 Voice Identify.
    , websocketLaunchOptsGuildId       :: GuildId
    -- ^ The guild ID for where we are joining the voice call, obtained from
    -- the normal gateway in downlink Opcode 0 Dispatch (event: Voice Server
    -- Update). We need this for the voice gateway uplink Opcode 0 Voice Identify.
    , websocketLaunchOptsEndpoint      :: T.Text
    -- ^ The endpoint to open the websocket connection to. This is obtained from
    -- the normal gateway in downlink Opcode 0 Dispatch (event: Voice Server
    -- Update).
    , websocketLaunchOptsWsHandle      :: (VoiceWebsocketReceiveChan, VoiceWebsocketSendChan)
    -- ^ Communication channels to the main thread that launches the websocket
    -- thread. The first tuple item is for voice gateway messages received from
    -- Discord, and the second is for messages to be sent to Discord.
    , websocketLaunchOptsUdpTid        :: MVar (Weak ThreadId)
    -- ^ A reference to the thread ID of the UDP thread, launched by the
    -- websocket thread once the control plane has been set up.
    , websocketLaunchOptsUdpHandle     :: (VoiceUDPReceiveChan, VoiceUDPSendChan)
    -- ^ Communication channels to the UDP thread that we launch from the
    -- websocket thread. The first tuple item is for UDP packets to be received
    -- from  Discord, and the second is for those to be sent to Discord.
    , websocketLaunchOptsSsrc          :: MVar Integer
    -- ^ The SSRC for the websocket connection, which is a sort of identifier.
    -- This is obtained as part of the voice websocket setup, in the voice
    -- gateway Opcode 2 Ready payload.
    }

-- | @WebsocketConn@ represents an active connection to Discord's Voice gateway
-- websocket, and contains the 'Connection' as well as the options that launched
-- it.
--
-- Lenses are defined for this type using Template Haskell. You can use them
-- to make accessing fields easier, like:
-- @
-- opts = wsConn ^. launchOpts
-- @
data WebsocketConn = WebsocketConn
    { websocketConnConnection    :: Connection
    , websocketConnLaunchOpts    :: WebsocketLaunchOpts
    }

-- | @UDPLaunchOpts@ represents all the data necessary to start a UDP connection
-- to Discord. This is constructed in the websocket thread using information
-- gathered mainly from the voice gateway downlink Opcode 2 Ready payload.
-- Field names for this ADT are cased weirdly, because we want to generate
-- lenses for them using Template Haskell.
--
-- Lenses are defined for this type using Template Haskell. You can use them
-- to make accessing fields easier, like:
-- @
-- mySsrc = opts ^. ssrc
-- @
data UDPLaunchOpts = UDPLaunchOpts
    { uDPLaunchOptsSsrc :: Integer
    , uDPLaunchOptsIp   :: T.Text
    , uDPLaunchOptsPort :: Integer
    , uDPLaunchOptsMode :: T.Text
    , uDPLaunchOptsUdpHandle :: (VoiceUDPReceiveChan, VoiceUDPSendChan)
    , uDPLaunchOptsSecretKey :: MVar [Word8]
    }

-- | @UDPConn@ represents an active UDP connection to Discord, and contains the
-- 'Socket' as well as the options that launched it.
--
-- Lenses are defined for this type using Template Haskell.You can use them
-- to make accessing fields easier, like:
-- @
-- sock = opts ^. socket
-- @
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
