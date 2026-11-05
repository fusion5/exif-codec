module Codec.Exif.Encode (encodeExif, encodeExifRaw) where

import Codec.Exif.Types
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as B
import Data.ByteString.Lazy qualified as BL
-- import Data.Int (Int32)
import Data.List (sortBy)
import Data.Maybe (isJust)
import Data.Ord (comparing)
import Data.Word

-- | Encodes an entire Exif envelope including the "Exif\NUL\NUL" prefix.
encodeExif :: ExifEnvelope -> BS.ByteString
encodeExif (Envelope header) = exifPrefix <> encodeExifRaw header

-- | Encodes just the TIFF layout (header + payload) without the APP1 prefix.
encodeExifRaw :: TiffHeader -> BS.ByteString
encodeExifRaw header = BL.toStrict $
  B.toLazyByteString $
    case header of
      IntelOrder exif -> buildTiff Intel exif
      MotorolaOrder exif -> buildTiff Motorola exif

putW16 :: ByteAlign -> Word16 -> B.Builder
putW16 Intel = B.word16LE
putW16 Motorola = B.word16BE

putW32 :: ByteAlign -> Word32 -> B.Builder
putW32 Intel = B.word32LE
putW32 Motorola = B.word32BE

-- putI32 :: ByteAlign -> Int32 -> B.Builder
-- putI32 Intel = B.int32LE
-- putI32 Motorola = B.int32BE

-- Constructs the binary block for an IFD & its associated extra data
buildIFDBlock :: ByteAlign -> Word32 -> [DirEntry] -> Word32 -> B.Builder
buildIFDBlock align startOffset entries nextOffset =
  let sortedEntries = sortBy (comparing dirEntryTag) entries
      count = fromIntegral (length sortedEntries) :: Word16
      ifdSize = 2 + 12 * fromIntegral count + 4
      extraStartOffset = startOffset + ifdSize
      (entriesBuilder, extraBuilder, _) =
        foldl' (buildDirEntry align extraStartOffset) (mempty, mempty, 0) sortedEntries
   in putW16 align count <> entriesBuilder <> putW32 align nextOffset <> extraBuilder

buildDirEntry ::
  ByteAlign ->
  Word32 ->
  (B.Builder, B.Builder, Word32) ->
  DirEntry ->
  (B.Builder, B.Builder, Word32)
buildDirEntry align extraBase (eb, xb, xsz) (DirEntry tag fmt cnt dat) =
  let datLen = fromIntegral (BS.length dat)
      fmtCode = formatCode fmt
   in if datLen <= 4
        then
          -- Left-justify <= 4 byte inline payloads via padding zeros (per spec, regardless of
          -- endianness)
          let paddedDat = dat <> BS.replicate (4 - BS.length dat) 0
              entryB =
                putW16 align tag
                  <> putW16 align fmtCode
                  <> putW32 align cnt
                  <> B.byteString paddedDat
           in (eb <> entryB, xb, xsz)
        else
          let offset = extraBase + xsz
              entryB =
                putW16 align tag
                  <> putW16 align fmtCode
                  <> putW32 align cnt
                  <> putW32 align offset
              padLen = if even datLen then 0 else 1
              paddedExtra = B.byteString dat <> B.byteString (BS.replicate padLen 0)
           in (eb <> entryB, xb <> paddedExtra, xsz + datLen + fromIntegral padLen)

-- | Internal pure builder for the TIFF structure.
buildTiff :: ByteAlign -> ExifData -> B.Builder
buildTiff align exif = headerBlock <> ifd0B <> exifB <> gpsB <> interopB <> ifd1B <> thumbDataB
  where
    enc32 w = BL.toStrict $ B.toLazyByteString $ putW32 align w

    headerBlock = case align of
      Intel -> B.byteString "II\x2A\x00" <> putW32 align 8
      Motorola -> B.byteString "MM\x00\x2A" <> putW32 align 8

    -- Calculates the total block size of an IFD (directories + out-of-line padded extra data)
    calcIFDTotalSize :: [DirEntry] -> Word32 -> Word32
    calcIFDTotalSize baseEntries addedCount =
      let count = fromIntegral (length baseEntries) + addedCount :: Word32
          ifdSize = 2 + 12 * count + 4
          extraSize =
            sum
              [ let l = fromIntegral (BS.length dat) :: Word32
                 in if l <= 4 then 0 else l + (if even l then 0 else 1)
              | DirEntry _ _ _ dat <- baseEntries
              ]
       in ifdSize + extraSize

    -- Purify base entries of any pre-existing pointers so we don't duplicate tags
    ifd0BaseEntries =
      filter
        (\e -> dirEntryTag e `notElem` [exifOffsetTag, gpsInfoTag])
        (ifdEntries (exifIFD0 exif))
    exifBaseEntries =
      maybe
        []
        (filter (\e -> dirEntryTag e /= interopOffsetTag) . ifdEntries)
        (exifSubIFD exif)
    gpsBaseEntries = maybe [] ifdEntries (exifGpsSubIFD exif)
    interopBaseEntries = maybe [] ifdEntries (exifInteropSubIFD exif)
    ifd1BaseEntries = case exifIFD1 exif of
      Just ifd ->
        filter
          (\e -> dirEntryTag e `notElem` [0x0201, 0x0202, 0x0111, 0x0117])
          (ifdEntries ifd)
      Nothing -> []

    hasExif = isJust (exifSubIFD exif)
    hasGps = isJust (exifGpsSubIFD exif)
    hasInterop = isJust (exifInteropSubIFD exif)
    hasThumb = isJust (exifThumbnail exif)
    hasIFD1 = isJust (exifIFD1 exif) || hasThumb

    -- Calculate total layout footprint sequentially
    ifd0Total = calcIFDTotalSize ifd0BaseEntries ((if hasExif then 1 else 0) + (if hasGps then 1 else 0))
    exifTotal = if hasExif then calcIFDTotalSize exifBaseEntries (if hasInterop then 1 else 0) else 0
    gpsTotal = if hasGps then calcIFDTotalSize gpsBaseEntries 0 else 0
    interopTotal = if hasInterop then calcIFDTotalSize interopBaseEntries 0 else 0
    ifd1Total = if hasIFD1 then calcIFDTotalSize ifd1BaseEntries (if hasThumb then 2 else 0) else 0

    thumbSize = case exifThumbnail exif of
      Just (JpegThumbnail bs) -> BS.length bs
      Just (TiffThumbnail bs) -> BS.length bs
      Nothing -> 0
    thumbPadLen = if even thumbSize then 0 else 1

    -- Exact sequential byte offsets (Header is exactly 8 bytes)
    offIFD0 = 8
    offExif = offIFD0 + ifd0Total
    offGps = offExif + exifTotal
    offInterop = offGps + gpsTotal
    offIFD1 = if hasIFD1 then offInterop + interopTotal else 0
    offThumb = if hasThumb then offIFD1 + ifd1Total else 0

    -- Compile specific blocks with dynamically injected pointer tags
    ifd0Entries =
      ifd0BaseEntries
        ++ [DirEntry exifOffsetTag FormatUnsignedLong 1 (enc32 offExif) | hasExif]
        ++ [DirEntry gpsInfoTag FormatUnsignedLong 1 (enc32 offGps) | hasGps]
    ifd0B = buildIFDBlock align offIFD0 ifd0Entries offIFD1

    exifEntries =
      exifBaseEntries
        ++ [DirEntry interopOffsetTag FormatUnsignedLong 1 (enc32 offInterop) | hasInterop]
    exifB = if hasExif then buildIFDBlock align offExif exifEntries 0 else mempty

    gpsB = if hasGps then buildIFDBlock align offGps gpsBaseEntries 0 else mempty

    interopB = if hasInterop then buildIFDBlock align offInterop interopBaseEntries 0 else mempty

    thumbEntries = case exifThumbnail exif of
      Just (JpegThumbnail bs) ->
        [ DirEntry 0x0201 FormatUnsignedLong 1 (enc32 offThumb),
          DirEntry 0x0202 FormatUnsignedLong 1 (enc32 (fromIntegral $ BS.length bs))
        ]
      Just (TiffThumbnail bs) ->
        [ DirEntry 0x0111 FormatUnsignedLong 1 (enc32 offThumb),
          DirEntry 0x0117 FormatUnsignedLong 1 (enc32 (fromIntegral $ BS.length bs))
        ]
      Nothing -> []

    ifd1B = if hasIFD1 then buildIFDBlock align offIFD1 (ifd1BaseEntries ++ thumbEntries) 0 else mempty
    thumbDataB = case exifThumbnail exif of
      Just (JpegThumbnail bs) -> B.byteString bs <> B.byteString (BS.replicate thumbPadLen 0)
      Just (TiffThumbnail bs) -> B.byteString bs <> B.byteString (BS.replicate thumbPadLen 0)
      Nothing -> mempty
