module Main where

import           Control.Concurrent         ( threadDelay
                                            )
import           Control.Monad              ( forM_
                                            )
import qualified Data.Text.IO as TIO
import           Discord
import           Discord.Voice              ( joinVoice   -- <-- This is the magic!
                                            , leaveVoice
                                            )
import qualified Discord.Requests as R
import           Discord.Types
import           UnliftIO                   ( liftIO
                                            )

main :: IO ()
main = do
    tok <- TIO.readFile "./examples/auth-token.secret"

    t <- runDiscord $ def { discordToken = tok
                          , discordOnStart = startHandler
                          , discordOnEnd = liftIO $ putStrLn "Ended"
                          , discordOnEvent = eventHandler
                          , discordOnLog = \s -> TIO.putStrLn s
                          }
    putStrLn "Finished!"

-- | Literally just joins this specific one and chills. lol.
eventHandler :: Event -> DiscordHandler ()
eventHandler event =
    case event of
        MessageCreate msg ->
            case messageText msg of
                "---joinVC" -> do
                    mbVc <- joinVoice (read "765660106259562498") (read "765660106259562502") False False
                    -- mbVc <- joinVoice (read "768810076201811989") (read "768810076201811993") False False
                    case mbVc of
                        Nothing -> liftIO $ print "whoops"
                        Just vc -> do
                            -- automatically leave after 30 seconds
                            liftIO $ threadDelay $ 30 * 10^(6 :: Int)
                            leaveVoice vc
                _ -> pure ()
        _ -> pure ()

startHandler :: DiscordHandler ()
startHandler = pure ()
