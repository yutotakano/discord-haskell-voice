# ideas for 2.4.0

We want to return youtube-dl data or artist name, access it during playing or before to implement !np commands or confirmations

Current state of `play` functions block the thread -- can't use it for returns

Field Research:

- discord.js [Lib layer] -> has an AudioResource thing that's passed to a play function, for ffmpeg and creating streams.
- discord.js third party lib `discord-player` [App layer] -> has a .search() func to get a Track type/object, passed to a Queue object which handles creation of stream (incl. spotify resolution to youtube) and pass to discord.js
- discord.py [Lib layer] -> has an AudioSource baseclass thing that's passed to a play function, optionally wrapping it with VolumeTransformer if it's pcm
- discord.py ![example bot](https://gist.github.com/Lenart12/024222b63db38c65f68b57ae7e623d56) [App layer] -> create_source() gets all the info and sets up the stream and ffmpeg, wrapped in Song class with requestor name, and played explicitly
- dsharpplus -> couldn't find
- serenity -> a bit messy couldn't find

Ideas:

- introduce an AudioSource or AudioResource or whatever these names are, make the creation of these separate from `play`
- remove all `play~~` functions and replace with the creation vers., `play` should be a single play function for all sources
- the source type should contain track details if any (local file would be none, so an inner type with Maybe TrackInfo? or use ffmpeg to get these), the person who initiated the `play` if possible (how? lib doesn't know the command structure), stuff like filters? or should filters still be an argument in the `play` func?
- the source type ideally shouldn't hold e.g. file handlers open or contain short-lived urls, or processes open as we can't know how long in the future the source will be used with `play`
  - it should be able to exit the voice monad fine to use the info elsewhere

- allow 
