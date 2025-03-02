{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-|
Module      : Discord.Voice.Conduit
Description : Convenient Conduits for transforming voice data
Copyright   : (c) 2021-2022 Yuto Takano
              (c) 2025-PRESENT discord-haskell-voice Contributors
License     : MIT
Maintainer  : Yuto Takano <moa17stock@gmail.com>

This module provides convenient Conduits (see the @conduit@ package for an
introduction to conduits, but essentially streaming pipes) for transforming
audio data, to be used 'Discord.Voice.HaskellTransformation's when constructing
audio resources using functions like 'Discord.Voice.createYoutubeResource'.

Functions that create an audio resource can optionally take as argument a
'Discord.Voice.HaskellTransformation', which is a Conduit transformation to
apply to the 16 bit little endian representation of the audio stream. It takes a
Conduit of the following type.

@
ConduitT B.ByteString B.ByteString (ResourceT DiscordHandler) ()
@

That is, the Conduit's needs to take @ByteString@ values from the upstream and
give to the downstream @ByteString@s. This ByteString stream is rather useless
as-is because it interweaves both stereo channels and has bytes as elements
(when 16-bit signed little-endian PCM means each sample spans across two bytes).
To make this stream useful, this module provides conduits to pack every two
bytes into a single signed 16-bit 'Int16', and vice versa. See 'packInt16C' and
'unpackInt16C'.

Since the audio data is stereo, there are also conduits provided that pack to
and unpack from tuples of @(left, right) :: (Int16, Int16)@ values.

These enable us to have an easier type signature to work with when performing
arithmetics on audio data:

@
yourConduit :: ConduitT Int16 Int16 (ResourceT DiscordHandler) ()

play (createFileResource "myfile.mp3" $ HaskellTransformation $ packInt16C .| yourConduit .| unpackInt16C) UnknownCodec
@

Despite the pack/unpack being a common pattern, we have not chosen it to be the
default behaviour, since you may want to use your own algorithms for
transforming audio.

An example usage of these conduits is:

@
runVoice $ do
    join (read "123456789012345") (read "67890123456789012")
    res <- createYoutubeResource "Lost in the Woods" $ HaskellTransformation $ packTo16CT .| toMono .| packFrom16CT
    play res (ProbeCodec "ffprobe")
@
-}
module Discord.Voice.Conduit
    (
      -- * Conduits to transform ByteString into workable Int16 data
      packInt16C
    , packInt16CT
    , unpackInt16C
    , unpackInt16CT
      -- * Common audio operations exemplars (see source for inspiration)
    , toMono
    )
where

import Conduit
import Data.Bits ( shiftL, shiftR, (.|.) )
import Data.ByteString qualified as B
import Data.Int ( Int16 )
import Data.Word ( Word16, Word8 )
import Discord

-- | A conduit that transforms 16-bit signed little-endian PCM ByteString to
-- streams of Int16 values. The Int16 values are in the range [-32768, 32767]
-- and alternate between left and right channels (stereo audio). See
-- 'packInt16CT' for a version that produces tuples for both channels.
packInt16C :: ConduitT B.ByteString Int16 (ResourceT DiscordHandler) ()
packInt16C = chunksOfExactlyCE 2 .| loop
  where
    loop = awaitForever $ \bytes -> do
        let [b1, b2] = B.unpack bytes
        -- little-endian arrangement
        yield $ (fromIntegral $ (shiftL (fromIntegral b2 :: Word16) 8) .|. (fromIntegral b1 :: Word16) :: Int16)

-- | A conduit that transforms streams of Int16 values to a ByteString, laid
-- out in 16-bit little-endian PCM format. Alternating inputs to this conduit
-- will be taken as left and right channels. See 'unpackInt16CT' for a version
-- that takes a stream of tuples of both channels.
unpackInt16C :: ConduitT Int16 B.ByteString (ResourceT DiscordHandler) ()
unpackInt16C = awaitForever $ \i ->
    yield $ B.pack
        [ fromIntegral i :: Word8
        , fromIntegral $ shiftR (fromIntegral i :: Word16) 8 :: Word8
        ]

-- | A conduit that transforms 16-bit signed little-endian PCM ByteString to
-- streams of (left, right)-tupled Int16 values.
packInt16CT :: ConduitT B.ByteString (Int16, Int16) (ResourceT DiscordHandler) ()
packInt16CT = chunksOfExactlyCE 4 .| loop
  where
    loop = awaitForever $ \bytes -> do
        let [b1, b2, b3, b4] = B.unpack bytes
        -- little-endian arrangement
        let left = (fromIntegral $ (shiftL (fromIntegral b2 :: Word16) 8) .|. (fromIntegral b1 :: Word16) :: Int16)
        let right = (fromIntegral $ (shiftL (fromIntegral b4 :: Word16) 8) .|. (fromIntegral b3 :: Word16) :: Int16)
        yield (left, right)

-- | A conduit that transforms (left, right)-tupled Int16 values into 16-bit
-- signed little-endian PCM ByteString.
unpackInt16CT :: ConduitT (Int16, Int16) B.ByteString (ResourceT DiscordHandler) ()
unpackInt16CT = awaitForever $ \(l, r) ->
    yield $ B.pack
        [ fromIntegral l :: Word8
        , fromIntegral $ shiftR (fromIntegral l :: Word16) 8 :: Word8
        , fromIntegral r :: Word8
        , fromIntegral $ shiftR (fromIntegral r :: Word16) 8 :: Word8
        ]

-- | A conduit to transform stereo audio to mono audio by taking the average
-- of the left and right channel values.
toMono :: ConduitT (Int16, Int16) (Int16, Int16) (ResourceT DiscordHandler) ()
toMono = awaitForever $ \(l, r) -> do
    -- take the average of the left and right channels
    let avg = l `div` 2 + r `div` 2
    yield (avg, avg)
