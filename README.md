# discord-haskell-voice

[![hackage version](https://img.shields.io/hackage/v/discord-haskell-voice?color=7565a8)](https://hackage.haskell.org/package/discord-haskell-voice)
[![discord-haskell version dependency](https://img.shields.io/badge/discord--haskell-%3E=1.12.0%20%26%26%20%3C=1.17.1-677eab)](https://hackage.haskell.org/package/discord-haskell)
![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/yutotakano/discord-haskell-voice/build_cabal.yml)

Welcome to `discord-haskell-voice`! This library provides you with a high-level
interface for interacting with Discord's Voice API, building on top of the
[`discord-haskell`](https://hackage.haskell.org/package/discord-haskell) library.

For a quick intuitive introduction to what this library enables you to do, see
the following snippet of code:

```hs
rickroll :: Channel -> DiscordHandler ()
rickroll c@(ChannelVoice {}) = do
    runVoice $ do
        join (channelGuild c) (channelId c)
        res <- createYoutubeResource "https://www.youtube.com/watch?v=dQw4w9WgXcQ" Nothing
        play res UnknownCodec
```

The library actively uses and supports conduit, which enables you to write
something like the following as well! (Spoiler: it plays 'Never Gonna Give You
Up' by Rick Astley at half the volume then prints to stdout.)

```hs
rickrollHalfVolume :: Channel -> DiscordHandler ()
rickrollHalfVolume c@(ChannelVoice {}) = do
    runVoice $ do
        join (channelGuild c) (channelId c)
        let halfAmplitude = awaitForever $ \current ->
                yield $ round $ fromIntegral current * 0.5
        res <- createYoutubeResource "rickroll" $ HaskellTransformation $ packInt16C .| halfAmplitude .| unpackInt16C
        play res UnknownCodec
        liftIO $ print "finished playing!"
```

Scroll down for a more in-depth features list.

## Requirements

- `libsodium`: We depend on [`saltine`](https://github.com/tel/saltine) for
  encryption and decryption of audio packets. This is a NaCl binding and thus
  requires libsodium to be installed on your system. See their README for
  installation information.
  - An alternative is provided via a compile flag, which is to use `crypton` as
    a backend instead, which requires no native dependencies. The security of
    this library has not been vetted however, so use with caution.
- `libopus`: We require Opus libraries to be installed on your system. Please
  follow the README of the [Haskell Opus package](https://github.com/yutotakano/opus).
- `ffmpeg`: It is heavily recommended to have FFmpeg installed and available in
  PATH. Without FFmpeg, you will not be able to transcode any non-PCM non-Opus
  files, bytestrings, or YouTube media.
- `yt-dlp`: It is equally heavily recommended to have yt-dlp installed and
  available in PATH. Without yt-dlp, you will not be able to use
  `createYoutubeResource`.
- `ffprobe`: It is optional to have FFprobe installed and available in PATH.
  Without FFprobe, you will not be able to use `ProbeCodec` to check if a given
  file, bytestream, or YouTube video can avoid transcoding via FFmpeg if it's
  already PCM or Opus-encoded.

## Features

What is supported:

- Can join/leave Discord voice channels.
  - Can join multiple of them simultaneously (one per sever) and stream
    different content to each.
  - Can join many voice channels (across many servers) and simulcast the same
    content, radio/subscriber-style.
- Can play arbitrary PCM/Opus audio from a file or byte stream
- Can play arbitrary audio using FFmpeg to transcode
- Can intelligently skip transcoding based on source format using ffprobe
- Can play arbitrary internet audio using yt-dlp, including live streams
- Can transform audio arbitrarily using either FFmpeg flags or Conduits that
  operate on bytestreams
- As it streams content, the library should use constant memory (unverified)

Where possible, specific details like method of encryption, protocol handshakes,
packet encoding etc have been abstracted away. As a result, this library is able
to offer a high-level productive API interface similar to discord.js and
discord.py libraries.

What is not supported:

- Decrypting audio packets sent from Discord (other people's voices), and
  decoding them to PCM. This isn't particularly well-documented by Discord and
  will thus likely never be supported by this library.

## Installation

This library is available on Hackage, at https://hackage.haskell.org/package/discord-haskell-voice.

### Cabal

To use it in your Cabal-based project, add `discord-haskell-voice` as a
dependency in your `.cabal` file:

```yaml
# --- myproject.cabal <truncated>
 build-depends:
      base >=4.7 && <5
    , discord-haskell ==1.17.1
    , discord-haskell-voice ==3.0.0
```

### Stack

To use it in your Stack-based project, add `discord-haskell-voice` in both your
`package.yaml` and `stack.yaml` files (since this library is not available in
Stackage for the same reason `discord-haskell` is not on Stackage):

```yaml
# --- stack.yaml <truncated>
extra-deps:
- discord-haskell-1.17.1
- discord-haskell-voice-3.0.0
```

```yaml
# --- package.yaml <truncated>
dependencies:
- base >= 4.7 && < 5
- discord-haskell == 1.17.1
- discord-haskell-voice == 3.0.0
```

## Documentation

See the Haddock documentation on the Hackage page, at
https://hackage.haskell.org/package/discord-haskell-voice/docs/Discord-Voice.html.

## Examples

See `examples/BasicMusicBot.hs` for a bot that uses many advanced features of
the library, including dynamically adjusting the stream audio using a TVar
(and allowing users to change the TVar using a `/volume` command).
