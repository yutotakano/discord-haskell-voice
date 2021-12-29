module Main where

import           Control.Monad              ( forM_
                                            , forever
                                            )
import           Control.Monad.Trans        ( lift )
import           Conduit
import qualified Data.Text.IO as TIO
import           Discord
import           Discord.Voice
import qualified Discord.Requests as R
import           Discord.Types
import           UnliftIO                   ( liftIO
                                            )
import Control.Concurrent

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

startHandler :: DiscordHandler ()
startHandler = do
    Right partialGuilds <- restCall R.GetCurrentUserGuilds

    result <- runVoice $ do
        forM_ partialGuilds $ \pg -> do
            Right guild <- liftDiscord $ restCall $ R.GetGuild (partialGuildId pg)
            Right chans <- liftDiscord $ restCall $ R.GetGuildChannels (guildId guild)

            case filter isVoiceChannel chans of
                (c:_) -> join (guildId guild) (channelId c)
                _     -> pure ()

        -- play something, then sit around in silence for 30 seconds
        playYouTube "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
        liftIO $ threadDelay $ 30 * 1000 * 1000

    liftIO $ print result
    pure ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

isVoiceChannel :: Channel -> Bool
isVoiceChannel (ChannelVoice {}) = True
isVoiceChannel _ = False
