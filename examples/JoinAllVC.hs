module Main where

import           Control.Monad              ( forM_
                                            )
import qualified Data.Text.IO as TIO
import           Discord
-- import           Discord.Voice              ( joinVoice
--                                             )
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
                          , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                          }
    putStrLn "Finished!"

eventHandler :: Event -> DiscordHandler ()
eventHandler event = pure ()

startHandler :: DiscordHandler ()
startHandler = pure ()
    -- Right partialGuilds <- restCall R.GetCurrentUserGuilds

    -- forM_ partialGuilds $ \pg -> do
    --     Right guild <- restCall $ R.GetGuild (partialGuildId pg)
    --     Right chans <- restCall $ R.GetGuildChannels (guildId guild)
    --     case filter isVoiceChannel chans of
    --         (c:_) -> do
    --             _ <- joinVoice (guildId guild) (channelId c) False False
    --             pure ()
    --         _ -> pure ()


isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

isVoiceChannel :: Channel -> Bool
isVoiceChannel (ChannelVoice {}) = True
isVoiceChannel _ = False
