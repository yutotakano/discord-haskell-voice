module Discord.Internal.Voice where

import Control.Concurrent.Chan          ( dupChan
                                        )
import Control.Monad.Reader             ( ask
                                        )

import Discord.Internal.Types           ( GuildId
                                        , ChannelId
                                        )
import Discord.Handle                   ( discordHandleGateway
                                        )
import Discord                          ( DiscordHandler
                                        )

-- | Joins a voice channel and initialises all the threads, ready to stream.
joinVoice :: GuildId -> ChannelId -> Bool -> Bool -> DiscordHandler DiscordVoiceHandle
joinVoice gid cid mute deaf = do
    -- Duplicate the event channel first
    h <- ask
    (_events, sends, _) <- discordHandleGateway h

    events <- dupChan _events


