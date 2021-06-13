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
import qualified Data.Map as Map
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import           Discord.Voice              ( joinVoice   -- <-- This is the magic!
                                            , leaveVoice
                                            , playYTDL
                                            , DiscordVoiceHandle
                                            )
import qualified Discord.Requests as R
import           Discord.Types
import           Discord
import           UnliftIO                   ( liftIO
                                            )

main :: IO ()
main = do
    tok <- TIO.readFile "./examples/production.secret"

    -- store a multithreaded list of tuple maps (channelId, voice chat handle)
    voiceHandles <- newMVar Map.empty

    t <- runDiscord $ def
        { discordToken = tok
        , discordOnStart = pure ()
        , discordOnEnd = liftIO $ putStrLn "Ended"
        , discordOnEvent = eventHandler voiceHandles
        , discordOnLog = \s -> TIO.putStrLn s
        }
    putStrLn "Exiting..."

eventHandler :: MVar (Map.Map T.Text DiscordVoiceHandle) -> Event -> DiscordHandler ()
eventHandler voiceHandles (MessageCreate msg) = do
    let splits = T.splitOn " " $ messageText msg
    guard (length splits > 0)

    case head splits of
        "-join" -> do
            guard (length splits == 3)

            let (vcGid:vcCid:[]) = tail splits
            currentHandles <- liftIO $ readMVar voiceHandles
            if Map.member vcCid currentHandles then do
                _ <- restCall $ R.CreateMessage (messageChannel msg) $
                    "I'm already in that VC!"
                pure ()
            else do
                eVc <- joinVoice (read $ T.unpack vcGid) (read $ T.unpack vcCid) False False
                case eVc of
                    Left e -> do
                        _ <- restCall $ R.CreateMessage (messageChannel msg) $
                            (T.pack $ "Couldn't join!" <> show e)
                        pure ()
                    Right vc -> do
                        void $ liftIO $ swapMVar voiceHandles $
                            Map.insert vcCid vc currentHandles
        "-leave" -> do
            guard (length splits == 2)

            let (vcCid:[]) = tail splits
            currentHandles <- liftIO $ readMVar voiceHandles
            when (Map.member vcCid currentHandles) $ do
                leaveVoice $ currentHandles Map.! vcCid
                void $ liftIO $ swapMVar voiceHandles $
                    Map.delete vcCid currentHandles

        "-play" -> do
            guard (length splits > 2)

            let (vcCid:rest) = tail splits
            currentHandles <- liftIO $ readMVar voiceHandles
            when (Map.member vcCid currentHandles) $ do
                playYTDL (currentHandles Map.! vcCid) (T.unpack $ T.intercalate " " rest) "youtube-dl" "ffmpeg"

        _ -> pure ()
eventHandler _ _ = pure ()

