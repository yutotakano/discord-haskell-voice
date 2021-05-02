module Discord.Internal.Voice where

import           Control.Concurrent.Async           ( race
                                                    )
import           Control.Concurrent                 ( ThreadId
                                                    , threadDelay
                                                    , forkIO
                                                    , Chan
                                                    , dupChan
                                                    , newChan
                                                    , readChan
                                                    , writeChan
                                                    , readMVar
                                                    )
import           Control.Monad.Reader               ( ask
                                                    , liftIO
                                                    )
import           Data.Aeson
import           Data.Aeson.Types                   ( parseMaybe
                                                    )
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
import           Discord.Internal.Types.VoiceWebsocket
import           Discord.Internal.Types             ( GuildId
                                                    , UserId
                                                    , User(..)
                                                    , UpdateStatusVoiceOpts(..)
                                                    , Event(..)
                                                    )
import           Discord.Internal.Gateway.EventLoop
                                                    ( GatewayException(..)
                                                    )
import           Discord.Internal.Gateway.Cache     ( Cache(..)
                                                    )
import           Discord.Handle                     ( discordHandleGateway
                                                    , discordHandleLog
                                                    , discordHandleCache
                                                    )
import           Discord                            ( DiscordHandler
                                                    , sendCommand
                                                    )

data DiscordVoiceThreadId
    = DiscordVoiceThreadIdWebsocket ThreadId
    | DiscordVoiceThreadIdUDP ThreadId

type DiscordVoiceHandleUDP
  = ( Chan (Either VoiceWebsocketException VoiceWebsocketReceivable)
    , Chan VoiceWebsocketSendable
    )

data DiscordVoiceHandle = DiscordVoiceHandle
    { discordVoiceHandleWebsocket   :: DiscordVoiceHandleWebsocket
    , discordVoiceHandleUDP         :: DiscordVoiceHandleUDP
    , discordVoiceThreads           :: [DiscordVoiceThreadId]
    }

-- | Joins a voice channel and initialises all the threads, ready to stream.
joinVoice
    :: GuildId
    -> ChannelId
    -> Bool
    -> Bool
    -> DiscordHandler (Maybe DiscordVoiceHandle)
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

    -- Loop for a maximum of 5 seconds
    result <- liftIO $ loopUntilEvents events
    case result of
        Nothing -> do
            liftIO $ writeChan (discordHandleLog h)
                "Discord did not respond to opcode 4 in time. "
                    "Perhaps it may not have permission to join."
            pure Nothing
        Just (_, _, _, Nothing) -> do
            liftIO $ writeChan (discordHandleLog h)
                "Discord did not give a good endpoint. " <>
                    "Perhaps it is down for maintenance."
            pure Nothing
        Just (sessionId, token, guildId, Just endpoint) -> do
            let connData = ConnInfo
                    { sessionId = sessionId
                    , token     = token
                    , guildId   = guildId
                    , endpoint  = endpoint
                    }
            -- Get the current user ID, and pass it on with all the other data
            eCache <- liftIO $ readMVar $ snd $ discordHandleCache h 
            case eCache of
                Left _ -> do
                    liftIO $ writeChan (discordHandleLog h)
                        "Could not get current user."
                    pure Nothing
                Right cache -> do
                    let uid = userId $ _currentUser cache

                    a <- liftIO $ startVoiceThreads connData uid $ discordHandleLog h
                    pure $ Just a

-- | Loop a maximum of 5 seconds, or until both Voice State Update and
-- Voice Server Update has been received.
loopUntilEvents
    :: Chan (Either GatewayException Event)
    -> IO (Maybe (T.Text, T.Text, GuildId, Maybe T.Text))
loopUntilEvents events = eitherRight <$> race wait5 waitForBoth
  where
    wait5 :: IO ()
    wait5 = threadDelay (5 * 10^(6 :: Int))

    waitForBoth :: IO (T.Text, T.Text, GuildId, Maybe T.Text)
    waitForBoth = do
        waitForBoth' Nothing Nothing

    -- | Wait for both VOICE_STATE_UPDATE and VOICE_SERVER_UPDATE.
    -- The order is undefined in docs.
    waitForBoth'
        :: Maybe T.Text
        -> Maybe (T.Text, GuildId, Maybe T.Text)
        -> IO (T.Text, T.Text, GuildId, Maybe T.Text)
    waitForBoth' (Just a) (Just (b, c, d)) = pure (a, b, c, d)
    waitForBoth' mb1 mb2 = do
        top <- readChan events
        case top of
            Right (UnknownEvent "VOICE_STATE_UPDATE" obj) -> do
                -- Parse the unknown event, and call waitForVoiceServer
                -- We assume "d -> session_id" always exists because Discord says so
                let sessionId = fromJust $ flip parseMaybe obj $ \o -> do
                        d <- o .: "d"
                        d .: "session_id"
                waitForBoth' (Just sessionId) mb2
            Right (UnknownEvent "VOICE_SERVER_UDPATE" obj) -> do
                let result = fromJust $ flip parseMaybe obj $ \o -> do
                        d <- o .: "d"
                        token <- d .: "token"
                        guildId <- d .: "guildId"
                        endpoint <- d .: "endpoint"
                        pure (token, guildId, endpoint)
                waitForBoth' mb1 (Just result)
            Right _ -> waitForBoth' mb1 mb2
            Left _  -> waitForBoth' mb1 mb2

-- | Selects the right element as a Maybe
eitherRight :: Either a b -> Maybe b
eitherRight (Left _)  = Nothing
eitherRight (Right x) = Just x

-- | Start the Websocket thread, which will create the UDP thread
startVoiceThreads :: WebsocketConnInfo -> UserId -> Chan T.Text -> IO DiscordVoiceHandle
startVoiceThreads connData uid log = do
    -- The UDP thread is not created immediately, so what is returned is
    -- an MVar to the data. We will wait and read from it.
    events <- newChan -- types are inferred from line below
    sends <- newChan
    websocketId <- forkIO $ voiceWebsocketLoop (events, sends) (connData, uid) log

    pure $ DiscordVoiceHandle
        { discordVoiceHandleWebsocket = (events, sends)
        , discordVoiceThreads =
            [ DiscordVoiceThreadIdWebsocket websocketId
            ]
        }
     

