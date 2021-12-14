# discord-haskell-voice

Saltine (used for encrypting/decrypting audio packets) requires `sodium` on Windows, and `pkg-config` and `libsodium-dev` on other systems.
See more [here](https://github.com/tel/saltine).

Currently implemented experimentally:
- Joining voice calls (multiple simultaneously too), changing voice status (mute, deaf, speaking indicator)
- Playing PCM audio data through encoding as OPUS, and encrypting, then sending over UDP
- Playing other format audio data through calling ffmpeg as a subprocess, then piping its PCM-format stdout into the above procedure
- Playing arbitrary YouTube videos/search queries through calling youtube-dl as a subprocess, then using ffmpeg to pipe it to the above procedures in real-time
- Decrypting OPUS audio packet data sent from Discord (other people's voices), and decoding them to PCM

Stuff in my todo-list:
- Currently in-progress: rewrite the entire experimental dirty code into a more structured and planned code. (see the relevant branch)
- Handle unexpected errors better, like UDP connection dropping or Websocket dropping, or main Gateway dropping
- Dedicated monad to prevent unlawful use of voice connections, and handle websocket/udp errors transparently
- Use/Learn Conduit to implement clean packet sending through streams
- Use/Learn Lenses to implement a simpler interface to e.g. changing volume or applying filters to a stream of audio (as ADT?)
- see `more idea.txt` and `idea.txt` for personal notes and thoughts I had during showers

End goal:
```hs
-- on /join
searchQuery <- newMVar
runVoice $ do
    join 231525124124
    forever $ do
        q <- readMVar query
        play (YoutubeQuery q)
-- on /play
putMVar searchQuery "https://youtube.com/watch?v=random"
```
