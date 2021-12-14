{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
module Discord.Internal.Voice
    ( runVoice
    ) where

import Codec.Audio.Opus.Encoder
import Control.Concurrent.Async ( race )
import Control.Concurrent.BoundedChan qualified as Bounded
import Control.Concurrent
    ( ThreadId
    , threadDelay
    , killThread
    , forkIO
    , Chan
    , dupChan
    , newChan
    , readChan
    , writeChan
    , MVar
    , newEmptyMVar
    , newMVar
    , readMVar
    , putMVar
    , withMVar
    , tryPutMVar
    , modifyMVar_
    )
import Control.Exception.Safe ( SomeException, handle, finally )
import Control.Lens
import Control.Monad.Reader ( ask, liftIO, runReaderT )
import Control.Monad.Except ( runExceptT, throwError)
import Control.Monad.Trans ( lift )
import Control.Monad ( when, void )
import Data.Aeson
import Data.Aeson.Types ( parseMaybe )
import Data.ByteString.Lazy qualified as BL
import Data.Maybe ( fromJust )
import Data.Text qualified as T
import System.Process ( CreateProcess(..), StdStream(..), proc, createProcess )

import Discord ( DiscordHandler, sendCommand, readCache )
import Discord.Handle ( discordHandleGateway, discordHandleLog )
import Discord.Internal.Gateway.Cache ( Cache(..) )
import Discord.Internal.Gateway.EventLoop
    ( GatewayException(..)
    , GatewayHandle(..)
    )
import Discord.Internal.Types.VoiceCommon
import Discord.Internal.Types
    ( GuildId
    , ChannelId
    , UserId
    , User(..)
    , GatewaySendable(..)
    , UpdateStatusVoiceOpts(..)
    , Event(..)
    , UpdateStatusVoiceOpts
    )
import Discord.Internal.Voice.WebsocketLoop
import Discord.Internal.Voice.UDPLoop

data DiscordVoiceResult
    = Success
    | Failure VoiceError

-- | Send a Gateway Websocket Update Voice State command (Opcode 4). Used to
-- indicate that the client voice status (deaf/mute) as well as the channel
-- they are active on.
updateStatusVoice
    :: GuildId
    -- ^ Id of Guild
    -> Maybe ChannelId
    -- ^ Id of the voice channel client wants to join (Nothing if disconnecting)
    -> Bool
    -- ^ Whether the client muted
    -> Bool
    -- ^ Whether the client deafened
    -> Voice ()
updateStatusVoice a b c d = lift $ lift $ sendCommand $ UpdateStatusVoice $ UpdateStatusVoiceOpts a b c d

-- | Execute the voice actions stored in the Voice monad, by first initialising
-- a Bounded chan and a mutex for sending. These are universal across all actions
-- within a voice monad (e.g. multiple joins), and this is what enables things
-- like multi-vc broadcast streaming.
runVoice :: Voice () -> DiscordHandler (Either VoiceError ())
runVoice action = do
    voiceHandles <- liftIO $ newMVar []
    mutEx <- liftIO $ newMVar ()
    sends <- liftIO $ Bounded.newBoundedChan 500 -- 10 seconds worth of 20ms
    let initialState = DiscordBroadcastHandle voiceHandles mutEx sends

    result <- finally (runExceptT $ flip runReaderT initialState $ action) $ do
        -- Wrap cleanup action in @finally@ to ensure we always close the
        -- threads even if an exception occurred.
        finalState <- liftIO $ readMVar voiceHandles
        mapMOf_ (traverse . websocket . _1) (liftIO . killThread) finalState
        mapMOf_ (traverse . udp . _1) (liftIO . killThread) finalState

    pure result

-- | Join a specific voice channel. The @guildId@ parameter will hopefully be
-- removed when discord-haskell fully caches channels internally (this is a TODO).
join :: GuildId -> ChannelId -> Voice ()
join guildId channelId = do
    h <- lift $ lift $ ask
    -- Duplicate the event channel, so we can read without taking data from event handlers
    events <- liftIO $ dupChan $ gatewayHandleEvents $ discordHandleGateway h

    -- To join a voice channel, we first need to send Voice State Update (Opcode
    -- 4) to the gateway, which will then send us two responses, Dispatch Event
    -- (Voice State Update) and Dispatch Event (Voice Server Update).
    updateStatusVoice guildId (Just channelId) False False

    (liftIO . doOrTimeout 5) (waitForVoiceStatusServerUpdate events) >>= \case
        Nothing -> do
            -- did not respond in time: no permission? or discord offline?
            throwError VoiceNotAvailable
        Just (_, _, _, Nothing) -> do
            -- If endpoint is null, according to Docs, no servers are available.
            throwError NoServerAvailable
        Just (sessionId, token, guildId, Just endpoint) -> do
            -- create the sending and receiving channels for Websocket
            wsChans <- liftIO $ (,) <$> newChan <*> newChan
            -- thread id and handles for UDP
            udpHandlesM <- liftIO $ (,) <$> newEmptyMVar <*> newEmptyMVar
            -- ssrc to be filled in during initial handshake
            ssrcM <- liftIO $ newEmptyMVar

            let wsOpts = WebsocketLaunchOpts sessionId token guildId endpoint
                    events wsChans udpHandlesM ssrcM

            -- fork a thread to start the websocket thread in the DiscordHandler
            -- monad using the current Reader state. Not much of a problem
            -- since many of the fields are mutable references.
            wsTid <- liftIO $ forkIO $ flip runReaderT h $
                launchWebsocket wsOpts $ discordHandleLog h
            
            -- modify the current Voice monad state to add the newly created
            -- UDP and Websocket handles (a handle consists of thread id and
            -- send/receive channels).
            voiceState <- ask
            -- TODO: check if readMVar ever blocks if the UDP thread fails to
            -- launch. Handle somehow? Perhaps with exception throwTo?
            udpTid <- liftIO $ readMVar $ udpHandlesM ^. _1
            udpChans <- liftIO $ readMVar $ udpHandlesM ^. _2
            ssrc <- liftIO $ readMVar ssrcM

            -- Add the new voice handles to the list of handles
            liftIO $ modifyMVar_ (voiceState ^. voiceHandles) $ \handles -> do
                let newHandle = DiscordVoiceHandle guildId channelId
                        (wsTid, wsChans) (udpTid, udpChans) ssrc
                pure (newHandle : handles)

-- | Continuously take the top item in the gateway event channel until both
-- Dispatch Event VOICE_STATE_UPDATE and Dispatch Event VOICE_SERVER_UPDATE
-- are received.
--
-- The order is undefined in docs, so this function will block until both
-- are received in any order.
waitForVoiceStatusServerUpdate
    :: Chan (Either GatewayException Event)
    -> IO (T.Text, T.Text, GuildId, Maybe T.Text)
waitForVoiceStatusServerUpdate = loopForBothEvents Nothing Nothing
  where
    loopForBothEvents
        :: Maybe T.Text
        -> Maybe (T.Text, GuildId, Maybe T.Text)
        -> Chan (Either GatewayException Event)
        -> IO (T.Text, T.Text, GuildId, Maybe T.Text)
    loopForBothEvents (Just a) (Just (b, c, d)) events = pure (a, b, c, d)
    loopForBothEvents mb1 mb2 events = readChan events >>= \case
        -- Parse UnknownEvent, which are events not handled by discord-haskell.
        Right (UnknownEvent "VOICE_STATE_UPDATE" obj) -> do
            -- Conveniently, we can just pass the result of parseMaybe
            -- back recursively.
            let sessionId = flip parseMaybe obj $ \o -> do
                    o .: "session_id"
            loopForBothEvents sessionId mb2 events
        Right (UnknownEvent "VOICE_SERVER_UPDATE" obj) -> do
            let result = flip parseMaybe obj $ \o -> do
                    token <- o .: "token"
                    guildId <- o .: "guild_id"
                    endpoint <- o .: "endpoint"
                    pure (token, guildId, endpoint)
            loopForBothEvents mb1 result events
        _ -> loopForBothEvents mb1 mb2 events

-- | Perform an IO action for a maximum of @sec@ seconds.
doOrTimeout :: Int -> IO a -> IO (Maybe a)
doOrTimeout sec longAction = rightToMaybe <$> race waitSecs longAction
  where
    waitSecs :: IO ()
    waitSecs = threadDelay (sec * 10^(6 :: Int))

-- | Selects the right element as a Maybe
rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _)  = Nothing
rightToMaybe (Right x) = Just x
