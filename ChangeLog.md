# Changelog for discord-haskell-voice

## Unreleased changes

- **Breaking API changes**
  - Replaced previously separate playing functions with a single `play` function
  - New `AudioResource` type represents a resource to be played, made with e.g. `createYoutubeResource`
  - New audio transformation abilities via FFmpeg flags and Haskell conduits
  - Use FFmpeg's direct Ogg/Opus output to skip the FFmpeg->PCM->Opus translation if there are no Haskell conduits that operate on PCM
  - Can now specify a codec in `play` to skip on using FFmpeg at all if input is already PCM or Opus
  - Can now use `ffprobe` to automatically detect if the input resource is already PCM or Opus and intelligently skip FFmpeg
  - New dependency on `typed-process` for safer external process
  - New dependency on `opus` from Hackage instead of direct git source
  - Remove ExceptT from the Voice monad stack

- Bug Fixes
  - Fix `OpusBufferTooSmall` when receiving Opus data due to buffer being half the size it should be
  - Fix the library crashing when joining a call with another user already in it, which triggers Opcodes 11/18/20 before Opcode 4
  - Fix `leave` causing MVar thread deadlock due to the BoundedChan consumer thread being killed

- Miscellaneous
  - Support GHC 8.10.7, 9.0.2, 9.2.4, and 9.6.8
  - Improve BasicMusicBot example to be less lisp-y in terms of brackets, and fix all warnings
  - Use `DerivingStrategies` in the library code to make explicit where deriving typeclasses are from
  - Add a compile flag to use a `crypton`-based encryption backend, which removes the necessity for libsodium
  - Remove `containers` dependency
  - Relax package bounds:
    - `aeson` from ==1.5.6.0 to <2.3
    - `bytestring` from <0.11 to <0.13
    - `conduit` from <=1.3.4.2 to <=1.4.0.0
    - `mtl` from ==2.2.2 to <2.4
    - `saltine` from <0.2 to <0.4
    - `stm` from <2.5.1 to <2.6
    - `text` from <2 to <3
    - `websockets` from <0.12.8 to <0.14
    - `wuss` from <=1.2 to <2.1
    - `discord-haskell` from <= 1.14.0 to <= 1.17.1
  - Add build CI for Cabal & Stack for all supported GHC versions
  - Renamed `master` branch to `main`
  - Updated copyright to current year and include contributors where applicable
  - Added link to GitHub Sponsors

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
