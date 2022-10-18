{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Discord.Internal.Voice.OggParser
    ( unwrapOggPacketsC
    ) where

import Codec.Audio.Opus.Encoder
import Conduit
import Control.Monad ( forM_, replicateM, void, unless )
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
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
    , oggPageSegmentAmt :: Int
    -- ^ The index 26 is "page_segments", which is the number of entries
    -- appearing in the segment table below.
    , oggPageSegmentLengths :: [Int]
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
        putWord8 $ fromIntegral oggPageSegmentAmt
        forM_ oggPageSegmentLengths $ \len ->
            putWord8 $ fromIntegral len
    get = do
        -- Ignore the OggS pattern
        void $ getByteString 4
        ver <- getWord8
        flags <- getWord8
        granularPosition <- getWord64le
        streamSerial <- getWord32le
        seqNumber <- getWord32le
        crcChecksum <- getWord32le
        segmentAmt <- fromIntegral <$> getWord8
        segmentLengths <- replicateM segmentAmt $ fromIntegral <$> getWord8
        pure $ OggPageHeader ver flags granularPosition streamSerial seqNumber crcChecksum segmentAmt segmentLengths

data OggPage = OggPage
    { oggPageHdr :: OggPageHeader
    , oggPageSegs :: BS.ByteString
    }
    deriving stock (Eq, Show)

unwrapOggPacketsC :: ConduitT BS.ByteString BS.ByteString IO ()
unwrapOggPacketsC = oggPageExtractC .| opusPacketExtractC .| filterOpusNonMetaC

oggPageExtractC :: (Monad m) => ConduitT BS.ByteString OggPage m ()
oggPageExtractC = loop BL.empty
  where
    -- | Keep awaiting for new chunks of bytestrings, concat that with any
    -- previous leftover chunks, and try to parse it as an Ogg, yielding any
    -- parsed pages.
    -- Since we perform concatenation between large bytestrings repeatedly,
    -- we use lazy bytestrings for the intermediate unconsumedBytes variable.
    loop :: (Monad m) => BL.ByteString -> ConduitT BS.ByteString OggPage m ()
    loop unconsumedBytes = do
        -- Get the bytestring chunks that sourceHandle reads, which is
        -- defaultChunkSize, roughly 32k bytes.
        -- Prepend any previous bytes we didn't consume since it wasn't part of
        -- the page.
        -- We cannot use 'leftover' to put back the unconsumed bytes at the end
        -- of each iteration, since conduit does not concatenate those bytes with
        -- the next item, and it'll just loop forever on the same failing input.
        -- See: https://stackoverflow.com/a/26872574
        mbChunk <- await
        -- Return early and prevent the bytestring append operation if neither
        -- the unconsumed bytes nor the conduit data exist. Both BL.null and
        -- equality check are O(1).
        if BL.null unconsumedBytes && mbChunk == Nothing then
            pure ()
        else do
            -- Since these are lazy bytestrings, appending is only dependent
            -- on the length of the spine in @unconsumedBytes@. Furthermore,
            -- conversion of strict to lazy with BL.fromStrict is O(1).
            -- This is better than using strict bytestrings, where appending
            -- incurs two memory copies.
            -- https://hackage.haskell.org/package/bytestring-0.11.3.1/docs/src/Data.ByteString.Internal.html#append
            let chunk = unconsumedBytes <> maybe BL.empty BL.fromStrict mbChunk
            let page = dropUntilPageStart chunk
            case decodeOrFail page of
                Left (_unconsumed, _consumedAmt, _err) -> do
                    -- TOOD: log warning
                    void $ error "warning"
                    loop page
                Right (unconsumed, _consumedAmt, hdr) -> do
                    let (page, rest) = BL.splitAt (fromIntegral $ sum $ oggPageSegmentLengths hdr) unconsumed
                    yield $ OggPage hdr (BL.toStrict page)
                    loop rest

-- | Conduit to extract the Opus bytes from an Ogg Page. This also handles the
-- addition of empty frames when there is no audio. The output of this conduit
-- is a strict bytestring since there is no longer any need for laziness, and
-- it's better to make it strict when we can guarantee properties of it (like
-- that each packet will only be made strict once) than later down when we know
-- for sure we need a strict bytestring anyway to send to Discord.
opusPacketExtractC :: ConduitT OggPage BS.ByteString IO ()
opusPacketExtractC = loop BL.empty
  where
    loop :: BL.ByteString -> ConduitT OggPage BS.ByteString IO ()
    loop opusSegment = do
        mbPage <- await
        case mbPage of
            Nothing -> do
                -- Send any incomplete leftovers. The toStrict here is
                -- run on the remaining unterminated Opus packet, which was never
                -- called toStrict upon previously. This is in sync with the
                -- runtime explanation given in splitOggPackets (that toStrict
                -- is only called maximally once for each segment).
                if BL.null opusSegment then pure () else yield (BL.toStrict opusSegment)

                -- Send at least 5 blank frames (20ms * 5 = 100 ms)
                let enCfg = mkEncoderConfig opusSR48k True app_audio
                encoder <- opusEncoderCreate enCfg
                let frame = BS.pack $ concat $ replicate 1280 [0xF8, 0xFF, 0xFE]
                encoded <- opusEncode encoder (mkStreamConfig enCfg (48*20) 1276) frame
                forM_ [0..4] $ \_ -> do
                    yield encoded
            Just (OggPage hdr segsBytes) -> do
                let (pkts, untermSeg, _) = splitOggPackets (oggPageSegmentLengths hdr) segsBytes opusSegment
                forM_ pkts $ yield . BL.toStrict
                loop untermSeg

-- | Conduit to filter out any packets beginning with OpusHead or OpusTag.
filterOpusNonMetaC :: (Monad m) => ConduitT BS.ByteString BS.ByteString m ()
filterOpusNonMetaC = do
    mbPkt <- await
    case mbPkt of
        Nothing -> pure ()
        Just pkt -> do
            unless (BS.take 8 pkt `elem` ["OpusHead", "OpusTags"]) $
                yield pkt

-- Keep droping one byte at a time until the first four bytes say OggS.
dropUntilPageStart :: BL.ByteString -> BL.ByteString
dropUntilPageStart bsChunk
    | BL.length bsChunk < 4
    = BL.empty
    | BL.take 4 bsChunk == "OggS"
    = bsChunk
    | otherwise
    = dropUntilPageStart $ BL.tail bsChunk

-- | split according to the given lengths, return any segments that were not
-- properly finished, like if the last item in the segLengths was 255
splitOggPackets
    :: [Int]
    -- ^ Length of each segment in bytes (0 - 255). This list can be up to 255
    -- long, and is taken from the segmentAmt header.
    -> BS.ByteString
    -- ^ The bytes. Unless the network packet itself was corrupt or lost halfway,
    -- this should have at least the total length of all segments.
    -> BL.ByteString
    -- ^ Any previous segment data that was not terminated in the previous page.
    -> ([BL.ByteString], BL.ByteString, BS.ByteString)
    -- ^ A tuple containing: (the list of parsed and concatted packets, any
    -- unterminated segments, any leftover bytes that were after the segments).
splitOggPackets sl bytes ps = loop sl bytes ps
  where
    loop
        :: [Int]
        -- ^ Length of remaining segments in bytes (0-255).
        -> BS.ByteString
        -- ^ The remaining bytes.
        -> BL.ByteString
        -- ^ Any previous unterminated segments that need to be prepended with
        -- this one to construct a complete Opus packet. This argument is lazy
        -- since it can undergo an append operation on each loop.
        --
        -- To be precise:
        --
        -- 1. Both Lazy and Strict bytestring variants would need to be made
        --    strict in its entirety once: O(n). The lazy variant will be made
        --    strict at the very end, while the strict one will be made strict
        --    before calling splitOggPackets.
        -- 2. Both Lazy and Strict bytestring variants have O(1) operation on
        --    self-contained segments (i.e. Opus packets that don't span multiple
        --    segments). For Lazy, we use constant-time strict splitAt: O(1);
        --    then convert the strict bytestring to lazy: O(1); and prepend that
        --    to the result list: O(1). For Strict, we use the same splitAt
        --    operation: O(1); And just prepend it to the result list: O(1).
        -- 3. Both Lazy and Strict bytestring variants have
        --    O(255 * (n * (n + 1) / 2)) for n segments that continue onto the
        --    next. In the case of Lazy, the discovery of a new nth continuation
        --    segment n will perform an append operation walking through the
        --    spine of previous (n - 1) * 255 bytes. The discovery of a
        --    termination segment at n+1 will walk through n * 255 bytes. This
        --    totals to (n * (n + 1) / 2) times walkthrough of 255 bytes.
        --    In the case of Strict, the discovery of a new nth continuation
        --    segment n will perform an append operation walking through both
        --    the previous (n - 1) * 255 bytes and the new 255 bytes, so n * 255.
        --    This totals to (n * (n + 1) / 2) times walkthrough of 255 bytes,
        --    although we have yet to consider the termination segment.
        -- 4. The discovery of a termination segment at n+1 in the Strict variant
        --    incurs a further walkthrough of the previous 255 * n bytes, plus
        --    however many bytes are in the last segment (<255).
        --
        -- Because 4 introduces an additional factor of 255 * n + lastBytes, the
        -- Lazy variant is preferred.
        -> ([BL.ByteString], BL.ByteString, BS.ByteString)
    loop [] bytes previousSegment = ([], previousSegment, bytes) -- O(255i)
    loop (segLen : segLengths) bytes previousSegment
        -- Handle the case when this is the first segment in a series of more
        -- than one segments.
        | (seg, rest) <- BS.splitAt segLen bytes -- O(1)
        , BS.length seg == segLen -- O(1)
        , segLen == 255 -- O(1)
        , BL.null previousSegment -- O(1)
        = loop segLengths rest $ BL.fromStrict seg -- O(1)
        | (seg, rest) <- BS.splitAt segLen bytes -- O(1)
        , BS.length seg == segLen -- O(1)
        , segLen == 255 -- O(1)
        = loop segLengths rest (previousSegment <> BL.fromStrict seg) -- O(255 * m), where m is number of previous segments
        -- Handle the case when this segment is self-conclusive
        | (seg, rest) <- BS.splitAt segLen bytes -- O(1)
        , BS.length seg == segLen -- O(1)
        , segLen < 255 -- O(1)
        , BL.null previousSegment -- O(1)
        = _1 %~ (BL.fromStrict seg :) $ loop segLengths rest BL.empty -- O(1)
        -- Handle the case when this segment terminates a packet
        | (seg, rest) <- BS.splitAt segLen bytes -- O(1)
        , BS.length seg == segLen -- O(1)
        , segLen < 255 -- O(1)
        = _1 %~ (previousSegment <> BL.fromStrict seg :) $ loop segLengths rest BL.empty -- O(255 * m) + recursion, where m is number of previous segments
        -- Handle the case when the segment length doesn't match!! This only
        -- happens if the network packet was lost somewhere, or if the Ogg packet
        -- is simply corrupt.
        | otherwise
        = error "TODO think of a better (non-crashing) way to handle this here"
