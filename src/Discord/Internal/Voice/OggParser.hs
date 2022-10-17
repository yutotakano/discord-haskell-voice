{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Discord.Internal.Voice.OggParser
    ( unwrapOggPacketsC
    ) where

import Codec.Audio.Opus.Encoder
import Conduit
import Control.Monad ( forM_, unless, replicateM, forM )
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Foldable ( toList )
import Data.Int
import Data.Sequence as S ( empty, Seq, viewl, (|>) )
import Lens.Micro

-- | The Ogg Page Header, after the initial 4 byte pattern of "OggS".
-- As described here: https://xiph.org/ogg/doc/framing.html
-- and here: http://www33146ue.sakura.ne.jp/staff/iz/formats/ogg.html
data OggPageHeader = OggPageHeader
    { oggPageVersion :: Word8
    -- ^ The index 4 byte is "stream_structure_version", which should be set to
    -- 0x00.
    , oggPageFlags :: Word8
    -- ^ The index 5 byte is "header_type_flag", which can be a combination of
    -- 0x01 continued packet (if unset then fresh packet)
    -- 0x02 first page of bitstream (if unset then not first page)
    -- 0x04 last page of bitsream (if unset then not last page)
    , oggPageGranularPosition :: Word64
    -- ^ The index 6 to 13 bytes are the "absolute granule position", which is
    -- the "sample" number. "-1" in two's complement indicate that no packets
    -- finish on this page.
    , oggPageStreamSerial :: Word32
    -- ^ The index 14 to 17 bytes are the "stream serial number". Each logical
    -- stream has a unique serial number.
    , oggPageSeqNumber :: Word32
    -- ^ The index 18 to 21 bytes are the "page counter".
    , oggPageCRCChecksum :: Word32
    -- ^ The index 22 to 25 is the CRC checksum of the entire page, with the
    -- checksum field set to 0.
    , oggPageSegmentAmt :: Word8
    -- ^ The index 26 is "page_segments", which is the number of entries
    -- appearing in the segment table below.
    , oggPageSegmentLengths :: [Word8]
    -- ^ Index 27 onwards in the header contains the length of each segment that
    -- follows this header in the body, up to the number of segments specified
    -- in segmentAmt. These are also called the lacing values.
    }
    deriving stock (Eq, Show)

instance Binary OggPageHeader where
    put OggPageHeader{..} = do
        putByteString "OggS"
        -- Ogg is a little-endian format
        putWord8 oggPageVersion
        putWord8 oggPageFlags
        putWord64le oggPageGranularPosition
        putWord32le oggPageStreamSerial
        putWord32le oggPageSeqNumber
        putWord32le oggPageCRCChecksum
        putWord8 oggPageSegmentAmt
        forM_ oggPageSegmentLengths $ \len ->
            putWord8 len
    get = do
        -- Ignore the OggS pattern
        getByteString 4
        ver <- getWord8
        flags <- getWord8
        granularPosition <- getWord64le
        streamSerial <- getWord32le
        seqNumber <- getWord32le
        crcChecksum <- getWord32le
        segmentAmt <- getWord8
        segmentLengths <- replicateM (fromIntegral segmentAmt) getWord8
        pure $ OggPageHeader ver flags granularPosition streamSerial seqNumber crcChecksum segmentAmt segmentLengths

unwrapOggPacketsC :: ConduitT BS.ByteString BS.ByteString IO ()
unwrapOggPacketsC = loop BS.empty BS.empty
  where
    -- | Keep awaiting for new chunks of bytestrings, concat that with any
    -- previous leftover chunks, and try to parse it as an Ogg, yielding any
    -- parsed bytes.
    loop :: BS.ByteString -> BS.ByteString -> ConduitT BS.ByteString BS.ByteString IO ()
    loop unconsumedBytes inProgressOpusSegment = do
        -- Get the bytestring chunks that sourceHandle reads, which is
        -- defaultChunkSize, roughly 32k bytes.
        -- Prepend any previous bytes we didn't consume since it wasn't part of
        -- the page.
        mbChunk <- (fmap (unconsumedBytes <>)) <$> await
        case mbChunk of
            -- No more upstream content.
            Nothing -> do
                -- Send at least 5 blank frames (20ms * 5 = 100 ms)
                let enCfg = mkEncoderConfig opusSR48k True app_audio
                encoder <- opusEncoderCreate enCfg
                let frame = BS.pack $ concat $ replicate 1280 [0xF8, 0xFF, 0xFE]
                encoded <- opusEncode encoder (mkStreamConfig enCfg (48*20) 1276) frame
                forM_ [0..4] $ \_ -> do
                    yield encoded
            Just chunk -> do
                -- Get the segment beginning from OggS, which is an Ogg page.
                let page = dropUntilPageStart chunk
                case decodeOrFail (BL.fromStrict page) of
                    Left (_unconsumed, _consumedAmt, _err) -> do
                        -- TOOD: log warning
                        error "warning"
                        loop page inProgressOpusSegment
                    Right (unconsumed, consumedAmt, hdr) -> do
                        if (BL.take 8 unconsumed `elem` ["OpusHead", "OpusTags"]) then
                            loop BS.empty BS.empty
                        else do
                            let (pkts, unterminatedSeg, nextPage) = splitOggPackets (oggPageSegmentLengths hdr) (BL.toStrict unconsumed) inProgressOpusSegment
                            forM pkts yield
                            loop nextPage unterminatedSeg

-- Keep droping one byte at a time until the first four bytes say OggS.
dropUntilPageStart :: BS.ByteString -> BS.ByteString
dropUntilPageStart bsChunk
    | BS.length bsChunk < 4
    = BS.empty
    | BS.take 4 bsChunk == "OggS"
    = bsChunk
    | otherwise
    = dropUntilPageStart $ BS.tail bsChunk

-- | split according to the given lengths, return any segments that were not
-- properly finished, like if the last item in the segLengths was 255
splitOggPackets
    :: [Word8]
    -- ^ Length of each segment in bytes (0 - 255). This list can be up to 255
    -- long, and is taken from the segmentAmt header.
    -> BS.ByteString
    -- ^ The bytes. Unless the network packet itself was corrupt or lost halfway,
    -- this should have at least the total length of all segments.
    -> BS.ByteString
    -- ^ Any previous segment data that was not terminated in the previous page.
    -> ([BS.ByteString], BS.ByteString, BS.ByteString)
    -- ^ A tuple containing: (the list of parsed and concatted packets, any
    -- unterminated segments, any leftover bytes that were after the segments).
splitOggPackets sl bytes ps = loop sl bytes ps
  where
    loop
        :: [Word8]
        -> BS.ByteString
        -> BS.ByteString
        -> ([BS.ByteString], BS.ByteString, BS.ByteString)
    loop [] bytes previousSegment = ([], previousSegment, bytes)
    loop (segLen : segLengths) bytes previousSegment
        -- Handle the case when there is at least another segment after this in
        -- the same packet
        | (seg, rest) <- BS.splitAt (fromIntegral segLen) bytes
        , BS.length seg == fromIntegral segLen
        , segLen == 255
        = loop segLengths rest (previousSegment <> seg)
        -- Handle the case when this segment terminates a packet (or is self-
        -- conclusive)
        | (seg, rest) <- BS.splitAt (fromIntegral segLen) bytes
        , BS.length seg == fromIntegral segLen
        , segLen < 255
        = _1 %~ (previousSegment <> seg :) $ loop segLengths rest BS.empty
        -- Handle the case when the segment length doesn't match!! This only
        -- happens if the network packet was lost somewhere, or if the Ogg packet
        -- is simply corrupt.
        | BS.length bytes < fromIntegral segLen
        = error "TODO think of a better (non-crashing) way to handle this here"
