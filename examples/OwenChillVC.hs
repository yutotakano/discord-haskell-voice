module Main where

import           Control.Concurrent         ( threadDelay
                                            )
import           Control.Monad              ( forM_
                                            )
import qualified Data.Text.IO as TIO
import           Discord
import           Discord.Internal.Voice     ( joinVoice   -- <-- This is the magic!
                                            )
import qualified Discord.Requests as R
import           Discord.Types
import           UnliftIO                   ( liftIO
                                            )

main :: IO ()
main = do
    tok <- TIO.readFile "./examples/production.secret"

    t <- runDiscord $ def { discordToken = tok
                          , discordOnStart = startHandler
                          , discordOnEnd = liftIO $ putStrLn "Ended"
                          , discordOnEvent = eventHandler
                          , discordOnLog = \s -> TIO.putStrLn s
                          }
    putStrLn "Finished!"

eventHandler :: Event -> DiscordHandler ()
eventHandler event = pure ()

-- | Literally just joins this specific one and chills. lol.
startHandler :: DiscordHandler ()
startHandler = do
    -- _ <- joinVoice (read "755798054455738489") (read "836947624137326623") False False
    liftIO $ threadDelay $ 4 * 10^(6 :: Int)
    _ <- joinVoice (read "768810076201811989") (read "768810076201811993") False False
    pure ()
