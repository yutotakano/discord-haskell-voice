module Main where

import           Control.Concurrent         ( threadDelay
                                            )
import           Control.Monad              ( forM_
                                            )
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import           Discord.Voice              ( joinVoice   -- <-- This is the magic!
                                            , leaveVoice
                                            , playPCM
                                            )
import qualified Discord.Requests as R
import           Discord.Types
import           Discord
import           UnliftIO                   ( liftIO
                                            )

main :: IO ()
main = do
    tok <- TIO.readFile "./examples/auth-token.secret"

    t <- runDiscord $ def { discordToken = tok
                          , discordOnStart = pure ()
                          , discordOnEnd = liftIO $ putStrLn "Ended"
                          , discordOnEvent = eventHandler
                          , discordOnLog = \s -> TIO.putStrLn s
                          }
    putStrLn "Finished!"

-- | Literally just joins this specific one and chills. lol.
eventHandler :: Event -> DiscordHandler ()
eventHandler (MessageCreate msg) =
    case messageText msg of
        "-joinVC" -> playSomeMusic msg
        _         -> pure ()
eventHandler _ = pure ()

playSomeMusic :: Message -> DiscordHandler ()
playSomeMusic msg = do
    eVc <- joinVoice (read "765660106259562498") (read "765660106259562502") False False
    -- eVc <- joinVoice (read "768810076201811989") (read "768810076201811993") False False
    case eVc of
        Left e -> do
            _ <- restCall $ R.CreateMessage (messageChannel msg) $
                (T.pack $ "Couldn't join!" <> show e)
            pure ()
        Right vc -> do
            contents <- liftIO $ BL.readFile "./music.raw"
            playPCM vc contents
            leaveVoice vc
            pure ()

