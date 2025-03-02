{-|
Module      : Discord.Voice
Description : Voice support for discord-haskell!
Copyright   : (c) 2021-2022 Yuto Takano
              (c) 2025-PRESENT discord-haskell-voice Contributors
License     : MIT
Maintainer  : Yuto Takano <moa17stock@gmail.com>

Welcome to @discord-haskell-voice@! This library provides you with a high-level
interface for interacting with Discord's Voice API, building on top of the
@[discord-haskell](https://hackage.haskell.org/package/discord-haskell)@ library
by Karl.

For a quick intuitive introduction to what this library enables you to do, see
the following snippet of code:

@
rickroll :: 'Discord.Types.Channel' -> 'Discord.DiscordHandler' ()
rickroll c@(ChannelVoice {}) = runVoice $ do
    join (channelGuild c) (channelId c)
    res <- createYoutubeResource \"https:\/\/www.youtube.com\/watch?v=dQw4w9WgXcQ\" Nothing
    play res UnknownCodec
@

We can see that this library introduces a dedicated monad for voice operations,
which opaquely guarantees that you won't accidentally keep hold of a closed
voice connection, or try to use it after a network error had occurred. We also
see intuitive functions for creating and playing audio resources. If you were
worried about how we leave the voice call, it's done automatically as part of
the cleanup action in 'runVoice'.

You'll also see further down the docs, that you can use
@[conduit](https://hackage.haskell.org/package/conduit)@ to stream arbitrary
ByteString data as audio, as well as manipulate and transform streams using its
interface. This is quite a powerful feature!

Let's dive in :)

== Dependencies / Requirements

Our README contains the requirements for this library to operate as expected,
but we repeat it here as well for readers who are too lazy.

  [@libsodium@]: We depend on [saltine](https://github.com/tel/saltine) for
  encryption and decryption of audio packets. This binds to libsodium, a system
  package.
  An alternative to saltine is provided via a compile flag. That is to use
  @crypton@ as the encryption backend instead, which needs no system
  dependencies. The security of this library has not been vetted so be cautious.

  [@libopus@]: We require Opus libraries to be installed on your system. Please
  follow the README of the [Haskell Opus package](https://github.com/yutotakano/opus).

  [@ffmpeg@]: It is heavily recommended to have FFmpeg installed and available in
  PATH. Without FFmpeg, you will not be able to transcode any non-PCM non-Opus
  files, bytestrings, or YouTube media.

  [@yt-dlp@]: It is equally heavily recommended to have yt-dlp installed and
  available in PATH. Without yt-dlp, you will not be able to use
  'createYoutubeResource'.

  [@ffprobe@]: It is optional to have FFprobe installed and available in PATH.
  Without FFprobe, you will not be able to use 'ProbeCodec' to check if a given
  file, bytestream, or YouTube video can avoid transcoding via FFmpeg if it's
  already PCM or Opus-encoded.

In general, all three largest OSes (Windows, macOS, Ubuntu) are supported, but
each one has a different way of installing system dependencies for encryption
and encoding, so please be careful.

=== I want to hurry up and just test around

The following commands install all system dependencies for the three most major
OSes.

==== __Windows__

For ffmpeg, ffprobe, and yt-dlp:

> winget install --id=Gyan.FFmpeg -e
> winget install --id=yt-dlp.yt-dlp -e

For libopus and libsodium, if you know where the MSYS2 environment uesd by
your Haskell toolchain is and you are comfortable modifying it, run:

> pacman -S mingw64/mingw-w64-x86_64-pkg-config mingw64/mingw-w64-x86_64-opus mingw64/mingw-w64-x86_64-libsodium

Otherwise, assuming you installed your Haskell toolchain using GHCup, run:

> ghcup run -m -- pacman -S mingw64/mingw-w64-x86_64-pkg-config mingw64/mingw-w64-x86_64-opus mingw64/mingw-w64-x86_64-libsodium

All other scenarios are unsupported but there should be equivalences.

==== __macOS__
> brew install ffmpeg yt-dlp opus libsodium

==== __Ubuntu__

> sudo add-apt-repository ppa:tomtomtom/yt-dlp
> sudo apt update
> sudo apt-get install ffmpeg yt-dlp pkg-config libopus-dev

== Getting Started

We assume you've added this library to your Cabal file dependencies list, and
already have a basic skeleton of a Discord bot. Specifically, our 'Voice' monad
can only be run from within code in the 'Discord.DiscordHandler' monad. Whether
it be within an event handler or on join or some scheduled action, make sure you
find where you want the bot to join a voice call.

The first two functions to learn are 'runVoice' and 'join'. Scroll down!
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
    , createFileResource
    , createPCMResource
    , AudioTransformation(..)
    , AudioCodec(..)
    , AudioResource(..)
    , defaultFfmpegArgs
    ) where

import Discord.Internal.Types.VoiceCommon
import Discord.Internal.Voice

