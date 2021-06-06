module Discord.Internal.Voice
    ( joinVoice
    , leaveVoice
    , playPCM
    ) where

import           Codec.Audio.Opus.Encoder
import           Control.Concurrent.Async           ( race
                                                    )
import qualified Control.Concurrent.BoundedChan as Bounded
import           Control.Concurrent                 ( ThreadId
                                                    , threadDelay
                                                    , killThread
                                                    , forkIO
                                                    , Chan
                                                    , dupChan
                                                    , newChan
                                                    , readChan
                                                    , writeChan
                                                    , newEmptyMVar
                                                    , readMVar
                                                    , putMVar
                                                    )
import           Control.Exception.Safe             ( SomeException
                                                    , handle
                                                    )
import           Control.Lens.Operators             ( (#)
                                                    )
import           Control.Monad.Reader               ( ask
                                                    , liftIO
                                                    )
import           Control.Monad                      ( when
                                                    )
import           Data.Aeson
import           Data.Aeson.Types                   ( parseMaybe
                                                    , parseEither
                                                    )
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe                         ( fromJust
                                                    )
import qualified Data.Text as T

import           Discord.Internal.Types             ( GuildId
                                                    , ChannelId
                                                    , Event
                                                    , GatewaySendable(..)
                                                    , UpdateStatusVoiceOpts
                                                    )
import           Discord.Internal.Voice.WebsocketLoop
import           Discord.Internal.Voice.UDPLoop
import           Discord.Internal.Types.VoiceWebsocket
import           Discord.Internal.Types.VoiceUDP
import           Discord.Internal.Types.VoiceError
import           Discord.Internal.Types             ( GuildId
                                                    , UserId
                                                    , User(..)
                                                    , UpdateStatusVoiceOpts(..)
                                                    , Event( UnknownEvent )
                                                    )
import           Discord.Internal.Gateway.EventLoop
                                                    ( GatewayException(..)
                                                    )
import           Discord.Internal.Gateway.Cache     ( Cache(..)
                                                    )
import           Discord.Handle                     ( discordHandleGateway
                                                    , discordHandleLog
                                                    )
import           Discord                            ( DiscordHandler
                                                    , sendCommand
                                                    , readCache
                                                    )

data DiscordVoiceThreadId
    = DiscordVoiceThreadIdWebsocket ThreadId
    | DiscordVoiceThreadIdUDP ThreadId

data DiscordVoiceHandle = DiscordVoiceHandle
    { discordVoiceGuildId           :: GuildId
    , discordVoiceHandleWebsocket   :: DiscordVoiceHandleWebsocket
    , discordVoiceHandleUDP         :: DiscordVoiceHandleUDP
    , discordVoiceHandleSSRC        :: Integer
    , discordVoiceThreads           :: [DiscordVoiceThreadId]
    }

-- | Joins a voice channel and initialises all the threads, ready to stream.
-- Returns Nothing if an error was encountered somewhere. 
--
-- (TODO: consider switching to Either Error DiscordVoiceHandle, like restCall
-- in the original module? But what should the error type be and where should it
-- be defined, when it's so interweaved in both Websocket, UDP, and Gateway?)
joinVoice
    :: GuildId
    -> ChannelId
    -> Bool
    -> Bool
    -> DiscordHandler (Either VoiceError DiscordVoiceHandle)
joinVoice gid cid mute deaf = do
    -- Duplicate the event channel, so we can read without taking data from event handlers
    h <- ask
    let (_events, _, _) = discordHandleGateway h
    events <- liftIO $ dupChan _events
    
    -- Send opcode 4
    sendCommand $ UpdateStatusVoice $ UpdateStatusVoiceOpts
        { updateStatusVoiceOptsGuildId = gid
        , updateStatusVoiceOptsChannelId = Just cid
        , updateStatusVoiceOptsIsMuted = mute
        , updateStatusVoiceOptsIsDeaf = deaf
        }

    -- Wait for Opcode 0 Voice State Update and Voice Server Update
    -- for a maximum of 5 seconds
    result <- liftIO $ loopUntilEvents events

    case result of
        Nothing -> do
            -- did not respond in time: no permission? or discord offline?
            pure $ Left VoiceNotAvailable
        Just (_, _, _, Nothing) -> do
            -- If endpoint is null, according to Docs, no servers are available.
            pure $ Left NoServerAvailable
        Just (sessionId, token, guildId, Just endpoint) -> do
            let connInfo = WSConnInfo
                    { wsInfoSessionId = sessionId
                    , wsInfoToken     = token
                    , wsInfoGuildId   = guildId
                    , wsInfoEndpoint  = endpoint
                    }
            -- Get the current user ID, and pass it on with all the other data
            uid <- getCacheUserId
            liftIO $ startVoiceThreads connInfo uid $ discordHandleLog h

-- | Get the user ID of the bot from the cache.
getCacheUserId :: DiscordHandler UserId
getCacheUserId = do
    cache <- readCache
    pure $ userId $ _currentUser cache

-- | Loop a maximum of 5 seconds, or until both Voice State Update and
-- Voice Server Update has been received.
loopUntilEvents
    :: Chan (Either GatewayException Event)
    -> IO (Maybe (T.Text, T.Text, GuildId, Maybe T.Text))
loopUntilEvents events = rightToMaybe <$> race wait5 (waitForBoth Nothing Nothing)
  where
    wait5 :: IO ()
    wait5 = threadDelay (5 * 10^(6 :: Int))

    -- | Wait for both VOICE_STATE_UPDATE and VOICE_SERVER_UPDATE.
    -- The order is undefined in docs, so this function will recursively fill
    -- up the two Maybe arguments until both are Just.
    waitForBoth
        :: Maybe T.Text
        -> Maybe (T.Text, GuildId, Maybe T.Text)
        -> IO (T.Text, T.Text, GuildId, Maybe T.Text)
    waitForBoth (Just a) (Just (b, c, d)) = pure (a, b, c, d)
    waitForBoth mb1 mb2 = do
        top <- readChan events
        case top of
            Right (UnknownEvent "VOICE_STATE_UPDATE" obj) -> do
                -- Conveniently, we can just pass the result of parseMaybe
                -- back recursively.
                let sessionId = flip parseMaybe obj $ \o -> do
                        o .: "session_id"
                waitForBoth sessionId mb2
            Right (UnknownEvent "VOICE_SERVER_UPDATE" obj) -> do
                let result = flip parseMaybe obj $ \o -> do
                        token <- o .: "token"
                        guildId <- o .: "guild_id"
                        endpoint <- o .: "endpoint"
                        pure (token, guildId, endpoint)
                waitForBoth mb1 result
            Right _ -> waitForBoth mb1 mb2
            Left _  -> waitForBoth mb1 mb2

-- | Selects the right element as a Maybe
rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)  = Nothing
rightToMaybe (Right x) = Just x

-- | Start the Websocket thread, which will create the UDP thread
startVoiceThreads
    :: WebsocketConnInfo
    -> UserId
    -> Chan T.Text
    -> IO (Either VoiceError DiscordVoiceHandle)
startVoiceThreads connInfo uid log = do
    -- First create the websocket (which will automatically try to identify)
    events <- newChan -- types are inferred from line below
    sends <- newChan
    syncKey <- newEmptyMVar
    websocketId <- forkIO $ voiceWebsocketLoop (events, sends) (connInfo, uid) log
    
    -- The first event is either a Right (Ready payload) or Left errors
    e <- readChan events
    case e of
        Right (Ready p) -> do
            -- Now try to create the UDP thread
            let udpInfo = UDPConnInfo
                    { udpInfoSSRC = readyPayloadSSRC p
                    , udpInfoAddr = readyPayloadIP p
                    , udpInfoPort = readyPayloadPort p
                    , udpInfoMode = "xsalsa20_poly1305"
                    -- ^ Too much hassle to implement all encryption modes.
                    }
            
            byteReceives <- newChan
            byteSends <- Bounded.newBoundedChan 60
            udpId <- forkIO $ udpLoop (byteReceives, byteSends) udpInfo syncKey log

            -- the first packet is a IP Discovery response.
            (IPDiscovery _ ip port) <- readChan byteReceives
            
            -- signal to the voice websocket using Opcode 1 Select Protocol
            writeChan sends $ SelectProtocol $ SelectProtocolPayload
                { selectProtocolPayloadProtocol = "udp"
                , selectProtocolPayloadIP       = ip
                , selectProtocolPayloadPort     = port
                , selectProtocolPayloadMode     = "xsalsa20_poly1305"
                }

            -- the next thing we receive in the websocket *should* be an
            -- Opcode 4 Session Description
            f <- readChan events
            case f of
                Right (SessionDescription _ key) -> do
                    putMVar syncKey key
                    pure $ Right $ DiscordVoiceHandle
                        { discordVoiceGuildId         = wsInfoGuildId connInfo
                        , discordVoiceHandleWebsocket = (events, sends)
                        , discordVoiceHandleUDP       = (byteReceives, byteSends)
                        , discordVoiceHandleSSRC      = udpInfoSSRC udpInfo
                        , discordVoiceThreads         =
                            [ DiscordVoiceThreadIdWebsocket websocketId
                            , DiscordVoiceThreadIdUDP udpId
                            ]
                        }
                Right a -> do
                    -- If this is ever called, refactor this entire case stmt
                    -- to a waitForSessionDescription, as it means another
                    -- event could be in the Websocket before SessionDescription
                    print "owo"
                    print a
                    killThread udpId
                    killThread websocketId
                    pure $ Left $ InvalidPayloadOrder
                Left  _ -> do
                    killThread udpId
                    killThread websocketId
                    pure $ Left $ InvalidPayloadOrder
        Right _ -> do
            killThread websocketId
            pure $ Left $ InvalidPayloadOrder
        Left  _ -> do
            killThread websocketId
            pure $ Left $ InvalidPayloadOrder

-- | Leave the current voice session by killing all relevant threads, and
-- sending a Voice State Update to the gateway.
leaveVoice :: DiscordVoiceHandle -> DiscordHandler ()
leaveVoice handle = do
    mapM_ (liftIO . killThread . toId) (discordVoiceThreads handle)
    
    sendCommand $ UpdateStatusVoice $ UpdateStatusVoiceOpts
        { updateStatusVoiceOptsGuildId = discordVoiceGuildId handle
        , updateStatusVoiceOptsChannelId = Nothing
        , updateStatusVoiceOptsIsMuted = False
        , updateStatusVoiceOptsIsDeaf = False
        }

  where
    toId t = case t of
        DiscordVoiceThreadIdWebsocket a -> a
        DiscordVoiceThreadIdUDP a -> a


-- | Play a stereo, 16-bit little-endian raw PCM, with 48,000 Hz.
playPCM :: DiscordVoiceHandle -> BL.ByteString -> DiscordHandler ()
playPCM handle source = do
    let thereAreMore = not $ BL.null source
    liftIO $ writeChan (snd $ discordVoiceHandleWebsocket handle) $ Speaking $ SpeakingPayload
        { speakingPayloadMicrophone = thereAreMore
        , speakingPayloadSoundshare = False
        , speakingPayloadPriority   = False
        , speakingPayloadDelay      = 0
        , speakingPayloadSSRC       = discordVoiceHandleSSRC handle
        }
    when thereAreMore $ repeatedlySend source
  where
    repeatedlySend restSource = do
        let sends = snd (discordVoiceHandleUDP handle)
        if not (BL.null restSource) then do
            liftIO $ print $ BL.length restSource
            let (clip, remains) = BL.splitAt (48*20) restSource
            let enCfg = _EncoderConfig # (opusSR48k, True, app_audio)
            let enStreamCfg = _StreamConfig # (enCfg, 48*20, 1275)
            encoder <- opusEncoderCreate enCfg
            encoded <- opusEncode encoder enStreamCfg $ BL.toStrict clip

            liftIO $ Bounded.writeChan sends encoded
            repeatedlySend remains
        else do
            liftIO $ sequence_ $ replicate 10 $ Bounded.writeChan sends "\248\255\254"
            playPCM handle restSource

