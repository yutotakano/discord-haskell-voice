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
    { discordVoiceHandleWebsocket   :: DiscordVoiceHandleWebsocket
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
        Nothing ->
            liftIO $ writeChan (discordHandleLog h) "Discord did not respond to opcode 4 in time."
        Just (_, _, _, Nothing) ->
            liftIO $ writeChan (discordHandeLog h) "Discord did not give a good endpoint."
        Just (sessionId, token, guildId, Just endpoint) -> do
            let connData = (sessionId, token, guildId, endpoint) :: WebsocketConnData
            -- Get the current user ID, and pass it on with all the other data
            eCache <- liftIO $ readMVar $ discordHandleCache h :: (Either (Cache, GatewayException) Cache)
            case eCache of
                Left _ ->
                    liftIO $ writeChan (discordHandeLog h) "Could not get current user."
                Right cache ->
                    let user = _currentUser cache
                    let userId = userId user

                    liftIO $ startVoiceThreads connData userId $ discordHandleLog h

-- | Loop a maximum of 5 seconds, or until both Voice State Update and Voice Server Update has been received
loopUntilEvents :: Chan (Either GatewayException Event) -> IO (Maybe (T.Text, T.Text, GuildId, Maybe T.Text))
loopUntilEvents events = eitherRight <$> race (threadDelay (5 * 10^(6 :: Int))) waitForBoth
  where
    waitForBoth :: IO (T.Text, T.Text, GuildId, Maybe T.Text)
    waitForBoth = do
        waitForBoth' Nothing Nothing

    -- | Wait for both VOICE_STATE_UPDATE and VOICE_SERVER_UPDATE, order is undefined in docs
    waitForBoth' :: Maybe T.Text -> Maybe (T.Text, GuildId, Maybe T.Text) -> IO (T.Text, T.Text, GuildId, Maybe T.Text)
    waitForBoth' (Just a) (Just (b, c, d) = pure $ (a, b, c, d)
    waitForBoth' mb1 mb2 = do
        top <- readChan events
        case top of
            Right (UnknownEvent "VOICE_STATE_UPDATE" obj) -> do
                -- Parse the unknown event, and call waitForVoiceServer
                -- We assume "d -> session_id" always exists because Discord says so
                let sessionId = fromJust $ parseMaybe $ do
                        d <- obj .: "d"
                        d .: "session_id"
                waitForBoth' (Just sessionId) mb2
            Right (UnknownEvent "VOICE_SERVER_UDPATE" obj) -> 
                let result = fromJust $ parseMaybe $ do
                   d <- obj .: "d"
                   token <- d .: "token"
                   guildId <- d .: "guildId"
                   endpoint <- d .: "endpoint"
                   pure $ (token, guildId, endpoint)
                waitForBoth' mb1 (Just result)
            Right _ -> waitForBoth' mb1 mb2
            Left _  -> waitForBoth' mb1 mb2

-- | Selects the right element as a Maybe
eitherRight :: Either a b -> Maybe b
eitherRight (Left _)  = Nothing
eitherRight (Right x) = Just x

-- | Start the Websocket thread, which will create the UDP thread
startVoiceThreads :: WebsocketConnData -> UserId -> Chan T.Text -> IO DiscordVoiceHandle
startVoiceThreads connData uid log = do
    -- The UDP thread is not created immediately, so what is returned is
    -- an MVar to the data. We will wait and read from it.
    events <- newChan :: IO (Chan (Either VoiceWebsocketException Event))
    sends <- newChan
    websocketId <- forkIO $ voiceWebsocketLoop (events, sends) (connData uid) log

    pure $ DiscordVoiceHandle
        { discordVoiceHandleWebsocket = (events, sends)
        , discordVoiceThreads =
            [ websocketId
            , udpId
            ]
        }
     

