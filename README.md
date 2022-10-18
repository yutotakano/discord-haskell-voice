# discord-haskell-voice

[![hackage version](https://img.shields.io/hackage/v/discord-haskell-voice?color=%235e5184)](https://hackage.haskell.org/package/discord-haskell-voice)
[![discord-haskell version dependency](https://img.shields.io/badge/discord--haskell-%3E=1.12.0%20%26%26%20%3C=1.15.3-lightblue)](https://hackage.haskell.org/package/discord-haskell)

Welcome to `discord-haskell-voice`! This library provides you with a high-level
interface for interacting with Discord's Voice API, building on top of the
[`discord-haskell`](https://hackage.haskell.org/package/discord-haskell) library
by Karl.

For a quick intuitive introduction to what this library enables you to do, see
the following snippet of code:

```hs
rickroll :: Channel -> DiscordHandler ()
rickroll c@(ChannelVoice {}) = do
    void $ runVoice $ do
        join (channelGuild c) (channelId c)
        playYouTube "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
```

The library actively uses and supports conduit, which enables you to write
something like the following as well!

```hs
rickrollHalfVolume :: Channel -> DiscordHandler ()
rickrollHalfVolume c@(ChannelVoice {}) = do
    void $ runVoice $ do
        join (channelGuild c) (channelId c)
        let halfAmplitude = awaitForever $ \current ->
                yield $ round $ fromIntegral current * 0.5
        playYouTube' "rickroll" $ packInt16C .| halfAmplitude .| unpackInt16C
        liftIO $ print "finished playing!"
```

## Requirements

- The library uses [`saltine`](https://github.com/tel/saltine) for encryption
and decryption of audio packets. This requires libsodium to be installed on
your system. See their README for information.
- The library requires Opus libraries to be installed on your system. The
`libopus-dev` package available on package repositories should be sufficient
on most \*nix systems. The `opus` brew package suffices on Mac. Windows
is unexplored yet (WSL works).
- If you are to use any variants of `playFile`, `playYouTube`, you will need
FFmpeg installed. To specify a custom executable name, see the `-With` function
variants.
- If you are to use any variants of `playYouTube`, you will additionally need
youtube-dl installed. This is used to get the stream URL to pass to FFmpeg. To
specify a custom executable name (such as yt-dlp), use `playYouTubeWith`.

## Features

What is supported:

- Can join/leave Discord voice channels. It is possible to join multiple of them
simultaneously (one per sever) and stream different contents to each.
- It is also possible to join many voice channels (across many servers) and play
the same content, radio/subscriber-style.
- You can play arbitrary PCM audio, arbitrary audio (with FFmpeg), and arbitrary
internet audio (with youtube-dl).
- You can transform audio arbitrarily using Conduit.
- As it streams content, the library /should/ use constant memory (unverified).
- OPUS encoding and specific implementation details such as handshakes and
encryption are done opaquely, and a nice abstraction layer is provided.

What is not supported:

- Decrypting audio packets sent from Discord (other people's voices), and
decoding them to PCM.

See `examples/BasicMusicBot.hs` for a bot that uses many advanced features of
the library, including dynamically adjusting the stream audio using a TVar
(and allowing users to change the TVar using a `/volume` command).

## Installation

This library is available on Hackage, at https://hackage.haskell.org/package/discord-haskell-voice.

### Cabal

To use it in your Cabal-based project, add `discord-haskell-voice` as a dependency in your `.cabal` file:

```yaml
# --- myproject.cabal <truncated>
 build-depends:
      base >=4.7 && <5
    , discord-haskell ==1.15.3
    , discord-haskell-voice ==2.3.1
```

### Stack

To use it in your Stack-based project, add `discord-haskell-voice` in both your `package.yaml` and `stack.yaml` files (since this library is not available in Stackage for the same reason `discord-haskell` is not on Stackage)

```yaml
# --- stack.yaml <truncated>
extra-deps:
- discord-haskell-1.15.3
- discord-haskell-voice-2.3.1
```

```yaml
# --- package.yaml <truncated>
dependencies:
- base >= 4.7 && < 5
- discord-haskell == 1.15.3
- discord-haskell-voice == 2.3.1
```

## Documentation

See the Haddock documentation on the Hackage page, at https://hackage.haskell.org/package/discord-haskell-voice/docs/Discord-Voice.html.

## Future Plans

- Use `stm-conduit` and `stm` for a safer Chan?
- Look into SubprocessException seemingly never been thrown (e.g. when SIGINT
is signalled to the libarry while FFmpeg is running)
- Consider, document, and improve the distinction of errors (VoiceError) vs
exceptions, and note down why any choices are made
