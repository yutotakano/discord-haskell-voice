{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
module Discord.Internal.Voice
    ( liftDiscord
    , runVoice
    , join
    ) where

import Codec.Audio.Opus.Encoder
import Control.Concurrent.Async ( race )
import Control.Concurrent
    ( ThreadId
    , threadDelay
    , killThread
    , forkIO
    , mkWeakThreadId
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
import Control.Concurrent.MSemN qualified as MSemN
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
import GHC.Weak ( deRefWeak, Weak )
import System.Process ( CreateProcess(..), StdStream(..), proc, createProcess )

import Discord ( DiscordHandler, sendCommand, readCache )
import Discord.Handle ( discordHandleGateway, discordHandleLog )
import Discord.Internal.Gateway.Cache ( Cache(..) )
import Discord.Internal.Gateway.EventLoop
    ( GatewayException(..)
    , GatewayHandle(..)
    )
import Discord.Internal.Types
    ( GuildId
    , ChannelId
    , UserId
    , User(..)
    , GatewaySendable(..)
    , UpdateStatusVoiceOpts(..)
    , Event(..)
    )
import Discord.Internal.Types.VoiceCommon
import Discord.Internal.Voice.CommonUtils
import Discord.Internal.Voice.WebsocketLoop

-- | Send a Gateway Websocket Update Voice State command (Opcode 4). Used to
-- indicate that the client voice status (deaf/mute) as well as the channel
-- they are active on.
-- This is not in the Voice monad because it has to be used after all voice
-- actions end, to quit the voice channels. It also has no benefit, since it
-- would cause extra transformer wrapping/unwrapping.
updateStatusVoice
    :: GuildId
    -- ^ Id of Guild
    -> Maybe ChannelId
    -- ^ Id of the voice channel client wants to join (Nothing if disconnecting)
    -> Bool
    -- ^ Whether the client muted
    -> Bool
    -- ^ Whether the client deafened
    -> DiscordHandler ()
updateStatusVoice a b c d = sendCommand $ UpdateStatusVoice $ UpdateStatusVoiceOpts a b c d

-- | @liftDiscord@ lifts a computation in DiscordHandler into a computation in
-- Voice. This is useful for performing DiscordHandler/IO actions inside the
-- Voice monad.
liftDiscord :: DiscordHandler a -> Voice a
liftDiscord = lift . lift

-- | Execute the voice actions stored in the Voice monad.
--
-- A single mutex and sending packet channel is used throughout all voice
-- connections within the actions, which enables multi-channel broadcasting.
-- The following demonstrates how a single playback is streamed to multiple
-- connections.
--
-- @@
-- runVoice $ do
--     join (read "123456789012345") (read "67890123456789012")
--     join (read "098765432123456") (read "12345698765456709")
--     play "http://example.com/audio"
-- @@
--
-- The return type of @runVoice@ represents result status of the voice computation.
-- It is isomorphic to @Maybe@, but the use of Either explicitly denotes that
-- the correct/successful/"Right" behaviour is (), and that the potentially-
-- existing value is of failure.
runVoice :: Voice () -> DiscordHandler (Either VoiceError ())
runVoice action = do
    voiceHandles <- liftIO $ newMVar []
    mutEx <- liftIO $ newMVar ()
    -- The following initialises a semaphore-limited bounded channel to limit
    -- the number of data in the UDP broadcast socket. This is to handle the case
    -- where the writer of voice data is much faster than the consumer (i.e.
    -- realtime). Using a MSemN is easier than BoundedChan because it allows
    -- dupChan in the many reader fork threads, whereas BoundedChan does not
    -- support dupChan and it would be very complicated to work around it.
    sends <- liftIO $ newChan
    sendsLim <- liftIO $ MSemN.new 500 -- 10 seconds worth of 20ms

    let initialState = DiscordBroadcastHandle voiceHandles mutEx (sendsLim, sends)

    result <- finally (runExceptT $ flip runReaderT initialState $ action) $ do
        -- Wrap cleanup action in @finally@ to ensure we always close the
        -- threads even if an exception occurred.
        finalState <- liftIO $ readMVar voiceHandles
        
        let killWkThread :: Weak ThreadId -> IO ()
            killWkThread tid = deRefWeak tid >>= \case
                Nothing -> pure ()
                Just x  -> killThread x

        mapMOf_ (traverse . websocket . _1) (liftIO . killWkThread) finalState
        mapMOf_ (traverse . udp . _1) (liftIO . killWkThread) finalState
        mapMOf_ (traverse . guildId) (\x -> updateStatusVoice x Nothing False False) finalState

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
    lift $ lift $ updateStatusVoice guildId (Just channelId) False False

    (liftIO . doOrTimeout 5000) (waitForVoiceStatusServerUpdate events) >>= \case
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
            udpSends <- (^. sends . _2) <$> ask
            udpChans <- liftIO $ (,) <$> newChan <*> dupChan udpSends
            udpTidM <- liftIO newEmptyMVar
            -- ssrc to be filled in during initial handshake
            ssrcM <- liftIO $ newEmptyMVar

            uid <- userId . cacheCurrentUser <$> (lift $ lift $ readCache)
            let wsOpts = WebsocketLaunchOpts uid sessionId token guildId endpoint
                    events wsChans udpTidM udpChans ssrcM

            -- fork a thread to start the websocket thread in the DiscordHandler
            -- monad using the current Reader state. Not much of a problem
            -- since many of the fields are mutable references.
            wsTid <- liftIO $ forkIO $ launchWebsocket wsOpts $ discordHandleLog h
            
            wsTidWeak <- liftIO $ mkWeakThreadId wsTid

            -- modify the current Voice monad state to add the newly created
            -- UDP and Websocket handles (a handle consists of thread id and
            -- send/receive channels).
            voiceState <- ask
            -- TODO: check if readMVar ever blocks if the UDP thread fails to
            -- launch. Handle somehow? Perhaps with exception throwTo?
            udpTid <- liftIO $ readMVar udpTidM
            ssrc <- liftIO $ readMVar ssrcM

            -- Add the new voice handles to the list of handles
            liftIO $ modifyMVar_ (voiceState ^. voiceHandles) $ \handles -> do
                let newHandle = DiscordVoiceHandle guildId channelId
                        (wsTidWeak, wsChans) (udpTid, udpChans) ssrc
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

-- | Helper function to update the speaking indicator for the bot.
--
-- Soundshare and priority are const as False, don't see bots needing them.
-- If and when required, add Bool signatures to this function.
updateSpeakingStatus :: DiscordVoiceHandle -> Bool -> IO ()
updateSpeakingStatus handle micStatus =
    writeChan (snd $ discordVoiceHandleWebsocket handle) $ Speaking $ SpeakingPayload
        { speakingPayloadMicrophone = micStatus
        , speakingPayloadSoundshare = False
        , speakingPayloadPriority   = False
        , speakingPayloadDelay      = 0
        , speakingPayloadSSRC       = (discordVoiceSSRC handle)
        }
