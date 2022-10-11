{-|
Module      : Discord.Voice
Description : Voice support for discord-haskell!
Copyright   : (c) 2021-2022 Yuto Takano
License     : MIT
Maintainer  : moa17stock@gmail.com

Welcome to @discord-haskell-voice@! This library provides you with a high-level
interface for interacting with Discord's Voice API, building on top of the
@[discord-haskell](https://hackage.haskell.org/package/discord-haskell)@ library
by Karl.

For a quick intuitive introduction to what this library enables you to do, see
the following snippet of code:

@
rickroll :: 'Channel' -> 'DiscordHandler' ()
rickroll c@(ChannelVoice {}) = do
    result <- runVoice $ do
        join (channelGuild c) (channelId c)
        playYouTube \"https:\/\/www.youtube.com\/watch?v=dQw4w9WgXcQ\"

    case result of
        Left err -> liftIO $ print err
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
      -- * Play Some Audio
    , play
    , createYoutubeResource
    , AudioTransformation(..)
    , AudioCodec(..)
    , FFmpegFilter(..)
    ) where

import Discord.Internal.Types.VoiceCommon
import Discord.Internal.Voice

{- $moreAccessibleVariants

While 'play' is the most fundamental way to play audio, it is often inconvenient
to write a Conduit, especially if you want to perform common actions like
streaming YouTube audio, or playing arbitrary audio files in arbitrary formats.
This is why we provide a number of more accessible variants of 'play', which
provide a more convenient interface to playing your favourite media.

Some of the functions in this section are marked with an apostrophe, which
indicate that they accept a Conduit processor as an argument to manipulate the
audio stream on the fly (such as changing volume).

The following table gives a comparative overview of all the functions provided
in this module for playing audio:

+-------------------------+--------------------+------------------+-------------------------------+-------------------------------------+
| Variant \\ Audio Source | ByteString Conduit | PCM Encoded File | Arbitrary Audio File          | YouTube Search/Video                |
+=========================+====================+==================+=============+=================+================+====================+
| Basic                   | 'play'             | 'playPCMFile'    | 'playFile'  | 'playFileWith'  | 'playYouTube'  | 'playYouTubeWith'  |
+-------------------------+--------------------+------------------+-------------+-----------------+----------------+--------------------+
| Post-process audio      | -                  | 'playPCMFile''   | 'playFile'' | 'playFileWith'' | 'playYouTube'' | 'playYouTubeWith'' |
+-------------------------+--------------------+------------------+-------------+-----------------+----------------+--------------------+

The functions that end with @-With@ accept arguments to specify executable names,
and in the case of FFmpeg, any arguments to FFmpeg.

-}
