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
import           Data.Maybe                 ( fromJust
                                            )
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified StmContainers.Map as M
import qualified Discord.Requests as R
import           Discord.Types
import           Discord.Voice
import           Discord.Voice.Conduit
import           Discord
import           Options.Applicative
import           UnliftIO                   ( liftIO
                                            , atomically
                                            )

data BotAction
    = JoinVoice ChannelId
    | LeaveVoice ChannelId
    | PlayVoice String
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
        <> command "play"
            ( flip info (progDesc "Queue something to play!") $
                PlayVoice . intercalate " " <$>
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
eventHandler contexts (MessageCreate msg) = case messageGuildId msg of
    Nothing  -> pure ()
    Just gid -> do
        -- the message was sent in a server
        let args = map T.unpack $ T.words $ messageContent msg
        case args of
            ("bot":_) -> case (execParserPure defaultPrefs parser $ tail args) of
                Success x -> handleCommand contexts msg gid x
                Failure failure ->
                    void $ restCall $ R.CreateMessage (messageChannelId msg) $ T.pack $
                        fst $ renderFailure failure "bot"
            _ -> pure ()
eventHandler _ _ = pure ()

handleCommand :: M.Map String GuildContext -> Message -> GuildId -> BotAction -> DiscordHandler ()
handleCommand contexts msg gid (JoinVoice cid) = do
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
                            yield $ round $ fromIntegral current * (fromIntegral v' / 100)
                    playYouTube' x $ packInt16C .| adjustVolume .| unpackInt16C

    case result of
        Left e -> liftIO $ print e >> pure ()
        Right _ -> pure ()

handleCommand contexts msg gid (LeaveVoice cid) = do
    context <- atomically $ M.lookup (show gid) contexts
    case context of
        Nothing -> pure ()
        Just (GuildContext _ _ leave) -> do
            void $ atomically $ M.delete (show gid) contexts
            void $ runVoice leave

handleCommand contexts msg gid (PlayVoice q) = do
    resultQueue <- atomically $ do
        context <- M.lookup (show gid) contexts
        case context of
            Nothing -> pure []
            Just (GuildContext xs v leave) -> do
                M.insert (GuildContext (xs ++ [q]) v leave) (show gid) contexts
                pure $ xs ++ [q]
    void $ restCall $ R.CreateMessage (messageChannelId msg) $ case resultQueue of
        [] -> T.pack $ "Can't play something when I'm not in a voice channel!"
        xs -> T.pack $ "Queued for playback: " <> show resultQueue

handleCommand contexts msg gid (ChangeVolume amount) = do
    context <- atomically $ M.lookup (show gid) contexts
    case context of
        Nothing -> pure ()
        Just (GuildContext q v l) -> do
            atomically $ swapTVar v amount
            void $ restCall $ R.CreateMessage (messageChannelId msg) $
                (T.pack $ "Volume set to " <> show amount <> " / 100")
