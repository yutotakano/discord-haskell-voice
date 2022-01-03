{-|
Module      : Discord.Voice
Description : Voice support for discord-haskell!
Copyright   : (c) Yuto Takano (2021)
License     : MIT
Maintainer  : moa17stock@email.com

Welcome to @discord-haskell-voice@! This library provides you with a high-level
interface for interacting with Discord's Voice API, building on top of the
@[discord-haskell](https://hackage.haskell.org/package/discord-haskell)@ library
by Karl.

For a quick intuitive introduction to what this library enables you to do, see
the following snippet of code:

@
rickroll :: 'Channel' -> 'DiscordHandler' ()
rickroll ChannelVoice {} = do
    result <- runVoice $ do
        join (channelGuild c) (channelId c)
        playYouTube \"https:\/\/www.youtube.com\/watch?v=dQw4w9WgXcQ\"

    case result of
        Left err -> liftIO . print (show err)
        Right _  -> pure ()
@

We can see that this library introduces a dedicated monad for voice operations,
which opaquely guarantees that you won't accidentally keep hold of a closed
voice connection, or try to use it after a network error had occurred.

You'll also see further down the docs, that you can use
@[conduit](https://hackage.haskell.org/package/conduit)@ to stream arbitrary
ByteString data as audio, as well as manipulate and transform streams using its
interface. This is quite a powerful feature!

Let's dive in :)
-}
module Discord.Voice
    ( 
      -- * Monad for Voice Operations
      Voice
    , runVoice
    , liftDiscord
      -- * Joining a Voice Channel
    , join
      -- * Functions for Playing Audio
    , play
    , playPCMFile
    , playPCMFile'
    , playFile
    , playFile'
    , playFileWith
    , playFileWith'
    , playYouTube
    , playYouTube'
    , defaultFFmpegArgs
    ) where

import Discord.Internal.Types.VoiceCommon
import Discord.Internal.Voice
