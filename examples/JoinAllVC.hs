{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Conduit
import           Control.Concurrent         ( threadDelay
                                            )
import           Control.Exception.Safe     ( catch
                                            , SomeException
                                            )
import           Control.Monad              ( forM_
                                            , forever
                                            , void
                                            )
import           Control.Monad.Trans        ( lift )
import qualified Data.Text.IO as TIO
import           Data.List.NonEmpty         ( NonEmpty((:|)) )
import           Discord
import           Discord.Voice
import qualified Discord.Requests as R
import           Discord.Types
import           UnliftIO                   ( liftIO
                                            )

main :: IO ()
main = do
    tok <- TIO.readFile "./examples/production.secret"

    t <- runDiscord $ def
        { discordToken = tok
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
                (c:_) -> void $ join (guildId guild) (channelId c)
                _     -> pure ()

        -- play something, then sit around in silence for 30 seconds
        resource <- createYoutubeResource "https://www.youtube.com/watch?v=dQw4w9WgXcQ" $
            -- TODO: opus output with no transformation is not yet supported
            Just $ (Reverb 150 :| []) ::.: (awaitForever yield)
        case resource of
            Nothing -> liftIO $ print "whoops"
            Just re -> catch (play re UnknownCodec) (\(e :: SomeException) -> liftIO $ print e)

        liftIO $ threadDelay $ 30 * 1000 * 1000

    liftIO $ print result
    pure ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

isVoiceChannel :: Channel -> Bool
isVoiceChannel (ChannelVoice {}) = True
isVoiceChannel _ = False
