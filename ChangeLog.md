# Changelog for discord-haskell-voice

## Unreleased changes

## 2.1.0

- Removed `updateSpeakingStatus` from the publicly exported function list for `Discord.Voice`.

## 2.0.0

- Rewrite the entire library (see #1).
- Introduce the `Voice` monad, and all functions in it: `join`, `play`, and all other variants of `play`.
- Add `lens` as a dependency for internal library use.
- Add `conduit` as the main method of piping and transforming audio on the fly.
- Remove all previous functions: `joinVoice`, `leaveVoice`, `playPCM`, etc.
- Add package documentation to public modules, and make sure the abstraction layer is solid (don't export useless internals).
- Rename the JoinSpecificVC example to BasicMusicBot and add a `bot volume` command to change the volume.

## 0.0.1

- Initial release.
- Implement `joinVoice`, `leaveVoice`, etc and use `DiscordVoiceHandle` to maintain a reference to the voice handle.
- Add JoinAllVC and JoinSpecificVC as example usages of the library.
