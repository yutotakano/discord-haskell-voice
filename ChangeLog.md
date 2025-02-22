# Changelog for discord-haskell-voice

## Unreleased changes

- Add a compile flag to use a `cryptonite`-based encryption implementation, which removes the necessity for libsodium.
- Improve the lisp-ish-ness of the BasicMusicBot example.
- Relax package bounds for `wuss`, `conduit` and `stm`, and support GHC 9.0.2 and GHC 9.2.4 (CI present).
- Update `discord-haskell` dependency bounds to `>= 1.12.0 && <= 1.15.3`

## 2.3.1

- Update `discord-haskell` dependency bounds to `>= 1.12.0 && <= 1.14.0`.
- Use `UnliftIO.MVar` functions internally for MVar operations in `DiscordHandler`
- `IOException`s thrown by e.g. createProcess during `runVoice` are no longer caught and subdued - they are propagated to the user.

## 2.3.0

- Export `playYouTubeWith` and `playYouTubeWith'` from `Discord.Voice`.
- Update `discord-haskell` dependency bounds to `>= 1.12.0 && <= 1.13.0`.
- Migrate from `lens` to `microlens`, following the `opus` package doing the same.

## 2.2.2

- Update `discord-haskell` dependency to 1.12.0
- Bump copyright to 2022
- Fix incomplete pattern match crash in the example bot when using `bot --bash-completion-script`

## 2.2.1

- Patch README having incorrect non-published details after Hackage publication.

## 2.2.0

- Change the definition of `Voice` from a type alias exposing dangerous internal handles, to a newtype wrapper. This also changes the definition of `liftDiscord` to maintain identical behaviour.
- Update `discord-haskell` dependency to 1.11.0

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
