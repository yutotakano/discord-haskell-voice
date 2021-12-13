module Discord.Internal.Voice
    ( joinVoice
    , leaveVoice
    , updateSpeakingStatus
    , playPCM
    , playFFmpeg
    , playYTDL
    , DiscordVoiceHandle(..)
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
                                                    , MVar
                                                    , newEmptyMVar
                                                    , newMVar
                                                    , readMVar
                                                    , putMVar
                                                    , withMVar
                                                    , tryPutMVar
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
                                                    , void
                                                    )
import           Data.Aeson
import           Data.Aeson.Types                   ( parseMaybe
                                                    )
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe                         ( fromJust
                                                    )
import qualified Data.Text as T
import           System.Process                     ( CreateProcess(..)
                                                    , StdStream(..)
                                                    , proc
                                                    , createProcess
                                                    )

import           Discord.Internal.Voice.WebsocketLoop
import           Discord.Internal.Voice.UDPLoop
import           Discord.Internal.Types.Common
import           Discord.Internal.Types             ( GuildId
                                                    , ChannelId
                                                    , UserId
                                                    , User(..)
                                                    , GatewaySendable(..)
                                                    , UpdateStatusVoiceOpts(..)
                                                    , Event(..)
                                                    , UpdateStatusVoiceOpts
                                                    )
import           Discord.Internal.Gateway.EventLoop ( GatewayException(..)
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

data DiscordVoiceResult
    = Success
    | Failure VoiceError

-- | Send a Gateway Websocket Update Voice State command (Opcode 4). Used when
-- the client wants to join, move, or disconnect from a voice channel.
updateStatusVoice
    :: GuildId
    -- ^ Id of Guild
    -> Maybe ChannelId
    -- ^ Id of the voice channel client wants to join (Nothing if disconnecting)
    -> Bool
    -- ^ Is the client muted
    -> Bool
    -- ^ Is the client deafened
    -> DiscordHandler ()
updateStatusVoice a b c d = sendCommand $ UpdateStatusVoice $ UpdateStatusVoiceOpts a b c d

-- | Execute the voice actions stored in the Voice monad, by first initialising
-- a Bounded chan and a mutex for sending. These are universal across all actions
-- within a voice monad (e.g. multiple joins), and this is what enables things
-- like multi-vc broadcast streaming.
runVoice :: Voice () -> DiscordHander (Either VoiceError ())
runVoice action = do

    voiceHandles <- liftIO $ newMVar []
    mutEx <- liftIO $ newMVar ()
    sends <- liftIO $ Bounded.newBoundedChan 500 -- 10 seconds worth of 20ms
    let initialState = DiscordMultiVoiceHandle voiceHandles mutEx sends

    result <- runExceptT $ runReaderT initialState $ action

    finalState <- liftIO $ readMVar voiceHandles
    mapM_ (killThread . fst . dvWebsocket) finalState
    mapM_ (killThread . fst . dvUDP) finalState


-- | Get the user ID of the bot from the cache.
getCacheUserId :: DiscordHandler UserId
getCacheUserId = userId . _currentUser <$> readCache

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

-- | Loop until both VOICE_STATE_UPDATE and VOICE_SERVER_UPDATE are received.
-- The order is undefined in docs, so this function will recursively fill
-- up two Maybe arguments until both are Just.
waitOpcode2And4
    :: Chan (Either GatewayException Event)
    -> IO (T.Text, (T.Text, GuildId, Maybe T.Text))
waitOpcode2And4 events = loopForBoth Nothing Nothing
  where
    loopForBoth
        :: Maybe T.Text
        -> Maybe (T.Text, GuildId, Maybe T.Text)
        -> IO (T.Text, (T.Text, GuildId, Maybe T.Text))
    loopForBoth (Just a) (Just (b, c, d)) = pure (a, (b, c, d))
    loopForBoth mb1 mb2 = do
        top <- readChan events
        case top of
            -- Parse UnknownEvent, which are events not handled by discord-haskell.
            Right (UnknownEvent "VOICE_STATE_UPDATE" obj) -> do
                -- Conveniently, we can just pass the result of parseMaybe
                -- back recursively.
                let sessionId = flip parseMaybe obj $ \o -> do
                        o .: "session_id"
                loopForBoth sessionId mb2
            Right (UnknownEvent "VOICE_SERVER_UPDATE" obj) -> do
                let result = flip parseMaybe obj $ \o -> do
                        token <- o .: "token"
                        guildId <- o .: "guild_id"
                        endpoint <- o .: "endpoint"
                        pure (token, guildId, endpoint)
                loopForBoth mb1 result
            _ -> loopForBoth mb1 mb2
