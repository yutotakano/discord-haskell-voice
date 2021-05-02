module Discord.Internal.Voice where

import Control.Concurrent.Chan          ( dupChan
                                        )
import Control.Concurrent               ( ThreadId
                                        , threadDelay
                                        )
import Control.Monad.Reader             ( ask
                                        )
import Data.Aeson                       ( parseMaybe
                                        )
import Data.Maybe                       ( fromJust
                                        )

import Discord.Internal.Types           ( GuildId
                                        , ChannelId
                                        , Event
                                        , GatewaySendable(..)
                                        , UpdateStatusVoiceOpts
                                        )
import Discord.Handle                   ( discordHandleGateway
                                        )
import Discord                          ( DiscordHandler
                                        )

data DiscordVoiceThreadId = DiscordVoiceThreadIdGateway ThreadId
                          | DiscordVoiceThreadIdWebsocket ThreadId
                          | DiscordVoiceThreadIdUDP ThreadId

type DiscordVoiceHandleGateway = (Chan (Either GatewayException Event), Chan GatewaySendable)

data DiscordVoiceHandle = DiscordVoiceHandle
    { discordVoiceHandleGateway     :: DiscordHandleGateway
    , discordVoiceHandleWebsocket   :: DiscordVoiceHandleWebsocket
    , discordVoiceHandleUDP         :: DiscordVoiceHandleUDP
    , discordVoiceThreads           :: [DiscordVoiceThreadId]
    }

-- | Joins a voice channel and initialises all the threads, ready to stream.
joinVoice :: GuildId -> ChannelId -> Bool -> Bool -> DiscordHandler DiscordVoiceHandle
joinVoice gid cid mute deaf = do
    -- Duplicate the event channel, so we can read without taking data from event handlers
    h <- ask
    (_events, _, _) <- discordHandleGateway h

    events <- dupChan _events
    
    -- Send opcode 4
    sendCommand $ UpdateStatusVoice $ UpdateStatusVoiceOpts
        { updateStatusVoiceOptsGuildId = gid
        , updateStatusVoiceOptsChannelId = Just cid
        , updateStatusVoiceOptsIsMuted = mute
        , updateStatusVoiceOptsIsDeaf = deaf
        }

    -- Loop for a maximum of 5 seconds
    result <- liftIO loopUntilEvents
    case result of
        Left e                   ->
            liftIO $ writeChan (discordHandleLog h) "Discord did not respond to opcode 4 in time."
        Right (_, _, _, Nothing) ->
            liftIO $ writeChan (discordHandeLog h) "Discord did not give a good endpoint."
        Right connData ->
            liftIO $ startVoiceThreads connData
            
-- | Loop a maximum of 5 seconds, or until both Voice State Update and Voice Server Update has been received
loopUntilEvents :: Chan (Either GatewayException Event) -> IO (Maybe (T.Text, T.Text, GuildId, Maybe T.Text))
loopUntilEvents events = eitherRight <$> race (threadDelay (5 * 10^(6 :: Int))) waitForVoiceState
  where
    -- | Wait for the VOICE_STATE_UPDATE event
    waitForVoiceState :: IO (T.Text, T.Text, GuildId, Maybe T.Text)
    waitForVoiceState = do
        top <- readChan events
        case top of
            Right (UnknownEvent "VOICE_STATE_UPDATE" obj) -> do
                -- Parse the unknown event, and call waitForVoiceServer
                -- We assume "d -> session_id" always exists because Discord says so
                let sessionId = fromJust $ parseMaybe $ do
                        d <- obj .: "d"
                        d .: "session_id"
                waitForVoiceServer sessionId
            Right _ -> waitForVoiceState
            Left _  -> waitForVoiceState

    -- | Wait for the VOICE_SERVER_UPDATE event
    waitForVoiceServer :: T.Text -> IO (T.Text, T.Text, GuildId, Maybe T.Text)
    waitForVoiceServer sessionId = do
        top <- readChan events
        case top of
            Right (UnknownEvent "VOICE_SERVER_UDPATE" obj) -> 
                fromJust $ parseMaybe $ do
                   d <- obj .: "d"
                   token <- d .: "token"
                   guildId <- d .: "guildId"
                   endpoint <- d .: "endpoint"
                   pure $ (sessionId, token, guildId, Just $ endpoint)
            Right _ -> waitForVoiceServer
            Left _  -> waitForVoiceServer
   
-- | Selects the right element as a Maybe
eitherRight :: Either a b -> Maybe b
eitherRight (Left _)  = Nothing
eitherRight (Right x) = Just x

-- | Start the Websocket thread, which will create the UDP thread
startVoiceThreads :: IO DiscordVoiceHandle
startVoiceThreads = do
    -- The UDP thread is not created immediately, so what is returned is
    -- an MVar to the data. We will wait and read from it.
    (websocket, websocketId, mUdp, mUdpId) <- forkIO $ voiceWebsocketLoop
     

