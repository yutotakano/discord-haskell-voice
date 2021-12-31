module Main where

import           Conduit
import           Control.Concurrent.STM.TVar
import           Control.Monad              ( when
                                            , guard
                                            , void
                                            , forever
                                            )
import           Data.List                  ( intercalate
                                            )
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified StmContainers.Map as M
import qualified Discord.Requests as R
import           Discord.Types
import           Discord.Voice
import           Discord
import           Options.Applicative
import           UnliftIO                   ( liftIO
                                            , atomically
                                            )

data BotAction
    = JoinVoice ChannelId
    | LeaveVoice ChannelId
    | PlayInVoice String
    | ChangeVolume Int
    deriving ( Read )

data GuildContext = GuildContext
    { songQueries :: [String]
    , volume      :: TVar Int -- volume
    , leaveFunc   :: Voice () -- function to leave voice channel
    }

parser :: ParserInfo BotAction
parser = info
    ( helper <*> subparser
        ( command "join"
            ( flip info (progDesc "Join a voice channel") $
                JoinVoice <$>
                argument auto (metavar "CHANID" <> help "Voice Channel ID"))
        <> command "leave"
            ( flip info (progDesc "Leave a voice channel") $
                LeaveVoice <$>
                argument auto (metavar "CHANID" <> help "Voice Channel ID"))
        <> command "queue"
            ( flip info (progDesc "Play something!") $
                PlayInVoice . intercalate " " <$>
                some (argument str (metavar "QUERY" <> help "Search query/URL")))
        <> command "volume"
            ( flip info (progDesc "Change the volume for this server!") $
                ChangeVolume <$>
                argument auto (metavar "VOLUME" <> help "Integer volume"))
        )
    ) fullDesc

main :: IO ()
main = do
    tok <- TIO.readFile "./examples/auth-token.secret"

    queries <- M.newIO
    t <- runDiscord $ def
        { discordToken = tok
        , discordOnStart = pure ()
        , discordOnEnd = liftIO $ putStrLn "Ended"
        , discordOnEvent = eventHandler queries
        , discordOnLog = \s -> TIO.putStrLn s
        }
    putStrLn "Exiting..."

eventHandler :: M.Map String GuildContext -> Event -> DiscordHandler ()
eventHandler contexts (MessageCreate msg) = do
    let args = map T.unpack $ T.words $ messageText msg
    case args of
        ("bot":_) -> case (execParserPure defaultPrefs parser $ tail args) of
            Success x -> handleCommand contexts msg x
            Failure failure ->
                void $ restCall $ R.CreateMessage (messageChannel msg) $ T.pack $
                    fst $ renderFailure failure "bot"
        _ -> pure ()
eventHandler _ _ = pure ()

handleCommand :: M.Map String GuildContext -> Message -> BotAction -> DiscordHandler ()
handleCommand contexts msg (JoinVoice cid) =
    -- Check if the message is sent in a server.
    case messageGuild msg of
        Nothing  -> pure ()
        Just gid -> do
            result <- runVoice $ do
                leave <- join gid cid
                volume <- liftIO $ newTVarIO 100
                liftDiscord $ atomically $ M.insert (GuildContext [] volume leave) (show gid) contexts
                -- Forever, read the top of the queue and play it.
                forever $ do
                    context <- liftDiscord $ atomically $ M.lookup (show gid) contexts
                    case context of
                        Nothing -> pure ()
                        Just (GuildContext [] _ _) -> pure ()
                        Just (GuildContext (x:xs) _ _) -> do
                            liftDiscord $ atomically $ M.insert (GuildContext xs volume leave) (show gid) contexts
                            let adjustVolume = awaitForever $ \current -> do
                                    v' <- liftIO $ readTVarIO volume
                                    yield $ round (fromIntegral current * (fromIntegral v' / 100))
                            playYouTube' x $ packTo16C .| adjustVolume .| packFrom16C 

            case result of
                Left e -> liftIO $ print e >> pure ()
                Right _ -> pure ()

handleCommand contexts msg (LeaveVoice cid) =
    case messageGuild msg of
        Nothing -> pure ()
        Just gid -> do
            context <- atomically $ M.lookup (show gid) contexts
            case context of
                Nothing -> pure ()
                Just (GuildContext _ _ leave) -> do
                    void $ atomically $ M.delete (show gid) contexts
                    void $ runVoice leave

handleCommand contexts msg (PlayInVoice q) =
    case messageGuild msg of
        Nothing -> pure ()
        Just gid -> do
            resultQueue <- atomically $ do
                context <- M.lookup (show gid) contexts
                case context of
                    Nothing -> do
                        volume <- newTVar 100
                        M.insert (GuildContext [q] volume (pure ())) (show gid) contexts
                        pure [q]
                    Just (GuildContext xs v leave) -> do
                        M.insert (GuildContext (xs ++ [q]) v leave) (show gid) contexts
                        pure $ xs ++ [q]
            void $ restCall $ R.CreateMessage (messageChannel msg) $
                (T.pack $ "Queued for playback: " <> show resultQueue)

handleCommand contexts msg (ChangeVolume amount) =
    case messageGuild msg of
        Nothing -> pure ()
        Just gid -> do
            context <- atomically $ M.lookup (show gid) contexts
            case context of
                Nothing -> pure ()
                Just (GuildContext q v l) -> do
                    atomically $ swapTVar v amount
                    void $ restCall $ R.CreateMessage (messageChannel msg) $
                        (T.pack $ "Volume set to " <> show amount)
