# discord-haskell-voice

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
and decryption of audio packets. This requires the appropriate libraries to be
installed on your system. See their README for information.
- If you are to use any variants of `playFile`, `playYouTube`, you will need
FFmpeg installed. To specify a custom executable name, see the `-With` function
variants.
- If you are to use any variants of `playYouTube`, you will additionally need
youtube-dl installed. This is used to get the stream URL to pass to FFmpeg. To
specify a custom executable name, use `playYouTubeWith`.

## Features

What is supported:

- Can join/leave Discord voice channels. It is possible to join multiple of them
simultaneously (one per sever) and stream different contents to each.
- It is also possible for many voice channels (across many servers) and play the
same content, radio/subscriber-style.
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

## Documentation

See the Haddock documentation.

## Future Plans

- Use `stm-conduit` and `stm` for a safer Chan?
