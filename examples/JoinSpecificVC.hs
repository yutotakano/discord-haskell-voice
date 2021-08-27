module Main where

import           Control.Concurrent         ( threadDelay
                                            , MVar
                                            , newMVar
                                            , readMVar
                                            , swapMVar
                                            )
import           Control.Monad              ( when
                                            , guard
                                            , void
                                            )
import qualified Data.ByteString.Lazy as BL
import           Data.List                  ( intercalate )
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import           Discord.Voice              ( joinVoice   -- <-- This is the magic!
                                            , leaveVoice
                                            , playYTDL
                                            , DiscordVoiceHandle(..)
                                            )
import qualified Discord.Requests as R
import           Discord.Types
import           Discord
import           Options.Applicative
import qualified StmContainers.Map as Map
import           UnliftIO                   ( liftIO
                                            , atomically
                                            )

data BotAction
    = JoinVoice GuildId ChannelId
    | LeaveVoice ChannelId
    | PlayInVoice ChannelId [String]
    deriving ( Read )

parser :: ParserInfo BotAction
parser = info
    ( helper <*> subparser
        ( command "join"
            ( flip info (progDesc "Join a voice channel") $
                JoinVoice <$>
                argument auto (metavar "GUILDID" <> help "Guild Id") <*>
                argument auto (metavar "CHANID" <> help "Voice Channel ID"))
        <> command "leave"
            ( flip info (progDesc "Leave a voice channel") $
                LeaveVoice <$>
                argument auto (metavar "CHANID" <> help "Voice Channel ID"))
        <> command "play"
            ( flip info (progDesc "Play something!") $
                PlayInVoice <$>
                argument auto (metavar "CHANID" <> help "Voice Channel ID") <*>
                some (argument str (metavar "QUERY" <> help "Search query/URL")))
        )
    ) fullDesc

main :: IO ()
main = do
    tok <- TIO.readFile "./examples/auth-token.secret"

    -- store a multithreaded list of tuple maps (channelId, voice chat handle)
    voiceHandles <- Map.newIO

    t <- runDiscord $ def
        { discordToken = tok
        , discordOnStart = pure ()
        , discordOnEnd = liftIO $ putStrLn "Ended"
        , discordOnEvent = eventHandler voiceHandles
        , discordOnLog = \s -> TIO.putStrLn s
        }
    putStrLn "Exiting..."

eventHandler :: Map.Map String DiscordVoiceHandle -> Event -> DiscordHandler ()
eventHandler voiceHandles (MessageCreate msg) = do
    let args = map T.unpack $ T.words $ messageText msg
    when (head args == "bot") $ do
        let result = execParserPure defaultPrefs parser $ tail args
        case result of
            Success (JoinVoice gid cid) -> do
                isInAlready <- liftIO $ atomically $ Map.lookup (show cid) voiceHandles
                case isInAlready of
                    Just _ ->
                        void $ restCall $ R.CreateMessage (messageChannel msg) $
                            "I'm already in that VC!"
                    Nothing -> do
                        eVc <- joinVoice gid cid False False
                        case eVc of
                            Left e ->
                                void $ restCall $ R.CreateMessage (messageChannel msg) $
                                    (T.pack $ "Couldn't join!" <> show e)
                            Right vc ->
                                liftIO $ atomically $ Map.insert vc (show cid) voiceHandles
            Success (LeaveVoice cid) -> do
                isInAlready <- liftIO $ atomically $ Map.lookup (show cid) voiceHandles
                case isInAlready of
                    Just vcHandle -> do
                        leaveVoice vcHandle
                        liftIO $ atomically $ Map.delete (show cid) voiceHandles
                    _ -> pure ()
            Success (PlayInVoice cid q) -> do
                isInAlready <- liftIO $ atomically $ Map.lookup (show cid) voiceHandles
                case isInAlready of
                    Just vcHandle ->
                        playYTDL vcHandle (intercalate " " q) "youtube-dl" "ffmpeg"
                    _ -> pure ()
            Failure failure ->
                void $ restCall $ R.CreateMessage (messageChannel msg) $ T.pack $
                    fst $ renderFailure failure "bot"
eventHandler _ _ = pure ()

