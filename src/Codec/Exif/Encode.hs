module Codec.Exif.Encode
  ( encodeExif,
  )
where

import Codec.Exif.Types (ByteAlign (..))
import Control.Monad (forM_, when)
import Data.Binary.Put
  ( Put,
    putByteString,
    putInt32be,
    putInt32le,
    putWord16be,
    putWord16le,
    putWord32be,
    putWord32le,
    putWord8,
    runPut,
  )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int32)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Word (Word16, Word32)
import Graphics.HsExif
  ( ExifTag (..),
    ExifValue (..),
    TagLocation (..),
  )
import Prelude

-- ---------------------------------------------------------------------------
-- Endian-aware put helpers

putW16P :: ByteAlign -> Word16 -> Put
putW16P Intel = putWord16le
putW16P Motorola = putWord16be

putW32P :: ByteAlign -> Word32 -> Put
putW32P Intel = putWord32le
putW32P Motorola = putWord32be

putI32P :: ByteAlign -> Int32 -> Put
putI32P Intel = putInt32le
putI32P Motorola = putInt32be

-- ---------------------------------------------------------------------------
-- IFD entry encoding

data EncodedEntry = EncodedEntry
  { entryTagKey :: Word16,
    entryFormat :: Word16,
    entryCount :: Word32,
    entryData :: BS.ByteString
  }

encodeValue :: ByteAlign -> Word16 -> ExifValue -> EncodedEntry
encodeValue ba tagKey = \case
  ExifNumber n ->
    if n < 0
      then EncodedEntry tagKey 9 1 $ LBS.toStrict $ runPut $ putI32P ba (fromIntegral n)
      else
        if n <= 65535
          then EncodedEntry tagKey 3 1 $ LBS.toStrict $ runPut $ putW16P ba (fromIntegral n)
          else EncodedEntry tagKey 4 1 $ LBS.toStrict $ runPut $ putW32P ba (fromIntegral n)
  ExifText str ->
    let raw = BS.pack (map (fromIntegral . fromEnum) str) <> BS.singleton 0
     in EncodedEntry tagKey 2 (fromIntegral (BS.length raw)) raw
  ExifRational num den ->
    let isSigned = num < 0 || den < 0
        fmt = if isSigned then 10 else 5
        bs =
          LBS.toStrict $
            runPut $
              if isSigned
                then putI32P ba (fromIntegral num) >> putI32P ba (fromIntegral den)
                else putW32P ba (fromIntegral num) >> putW32P ba (fromIntegral den)
     in EncodedEntry tagKey fmt 1 bs
  ExifNumberList xs ->
    let anyNeg = any (< 0) xs
        allShort = all (\x -> x >= 0 && x <= 65535) xs
        bs
          | anyNeg = LBS.toStrict $ runPut $ mapM_ (putI32P ba . (fromIntegral :: Int -> Int32)) xs
          | allShort = LBS.toStrict $ runPut $ mapM_ (putW16P ba . (fromIntegral :: Int -> Word16)) xs
          | otherwise = LBS.toStrict $ runPut $ mapM_ (putW32P ba . (fromIntegral :: Int -> Word32)) xs
        fmt
          | anyNeg = 9
          | allShort = 3
          | otherwise = 4
     in EncodedEntry tagKey fmt (fromIntegral (length xs)) bs
  ExifRationalList rs ->
    let anySigned = any (\(n, d) -> n < 0 || d < 0) rs
        fmt = if anySigned then 10 else 5
        bs = LBS.toStrict $ runPut $ forM_ rs $ \(n, d) ->
          if anySigned
            then putI32P ba (fromIntegral n) >> putI32P ba (fromIntegral d)
            else putW32P ba (fromIntegral n) >> putW32P ba (fromIntegral d)
     in EncodedEntry tagKey fmt (fromIntegral (length rs)) bs
  ExifUndefined bs ->
    EncodedEntry tagKey 7 (fromIntegral (BS.length bs)) bs
  ExifUnknown fmt count val ->
    EncodedEntry tagKey fmt (fromIntegral count) (LBS.toStrict $ runPut $ putW32P ba (fromIntegral val))

-- ---------------------------------------------------------------------------
-- Top-level encoder

-- | Encode a 'Map' of EXIF tags and values into a raw TIFF-structured byte
-- string. The 'ByteAlign' argument controls whether the output uses
-- little-endian ('Intel') or big-endian ('Motorola') byte order.
encodeExif :: ByteAlign -> Map ExifTag ExifValue -> BS.ByteString
encodeExif ba m = LBS.toStrict $ runPut $ do
  -- TIFF header
  putByteString $ case ba of Intel -> "II"; Motorola -> "MM"
  putW16P ba 42
  putW32P ba 8

  -- IFD0 (including pointers to sub-IFDs)
  putIFD ifd0WithOffsets

  -- ExifSubIFD
  when hasExifSub $ putIFD exifSubWithOffsets

  -- GpsSubIFD
  when hasGpsSub $ putIFD gpsSubWithOffsets

  -- Out-of-line value data
  putDataSection
  where
    entries = Map.toList m

    ifd0Raw =
      [ (tag, val)
      | (tag, val) <- entries,
        tagLocation tag == IFD0,
        tagKey tag /= 0x8769,
        tagKey tag /= 0x8825
      ]
    exifSubRaw = [(tag, val) | (tag, val) <- entries, tagLocation tag == ExifSubIFD]
    gpsSubRaw = [(tag, val) | (tag, val) <- entries, tagLocation tag == GpsSubIFD]

    ifd0Entries = [encodeValue ba (tagKey tag) val | (tag, val) <- ifd0Raw]
    exifSubEntries = [encodeValue ba (tagKey tag) val | (tag, val) <- exifSubRaw]
    gpsSubEntries = [encodeValue ba (tagKey tag) val | (tag, val) <- gpsSubRaw]

    hasExifSub = not (null exifSubEntries)
    hasGpsSub = not (null gpsSubEntries)

    numIFD0 = length ifd0Entries + (if hasExifSub then 1 else 0) + (if hasGpsSub then 1 else 0)
    lenIFD0 = 2 + fromIntegral numIFD0 * 12 + 4 :: Word32

    numExifSub = length exifSubEntries
    lenExifSub = if hasExifSub then 2 + fromIntegral numExifSub * 12 + 4 else 0 :: Word32
    offsetExifSub = 8 + lenIFD0

    numGpsSub = length gpsSubEntries
    lenGpsSub = if hasGpsSub then 2 + fromIntegral numGpsSub * 12 + 4 else 0 :: Word32
    offsetGpsSub = 8 + lenIFD0 + lenExifSub

    dataStartOffset = 8 + lenIFD0 + lenExifSub + lenGpsSub

    exifSubPtr = EncodedEntry 0x8769 4 1 (LBS.toStrict $ runPut $ putW32P ba offsetExifSub)
    gpsSubPtr = EncodedEntry 0x8825 4 1 (LBS.toStrict $ runPut $ putW32P ba offsetGpsSub)

    allIFD0 =
      sortBy (comparing entryTagKey) $
        ifd0Entries ++ [exifSubPtr | hasExifSub] ++ [gpsSubPtr | hasGpsSub]
    allExifSub = sortBy (comparing entryTagKey) exifSubEntries
    allGpsSub = sortBy (comparing entryTagKey) gpsSubEntries

    isInline e = BS.length (entryData e) <= 4

    assignOffsets :: Word32 -> [EncodedEntry] -> [(EncodedEntry, Maybe Word32)]
    assignOffsets _ [] = []
    assignOffsets curOff (e : es)
      | isInline e = (e, Nothing) : assignOffsets curOff es
      | otherwise =
          let len = fromIntegral (BS.length (entryData e)) :: Word32
              paddedLen = if odd len then len + 1 else len
           in (e, Just curOff) : assignOffsets (curOff + paddedLen) es

    allEntriesWithOffsets =
      assignOffsets dataStartOffset (allIFD0 ++ allExifSub ++ allGpsSub)

    n0 = length allIFD0
    nExif = length allExifSub
    (ifd0WithOffsets, rest1) = splitAt n0 allEntriesWithOffsets
    (exifSubWithOffsets, gpsSubWithOffsets) = splitAt nExif rest1

    outOfLineEntries = [(e, off) | (e, Just off) <- allEntriesWithOffsets]

    putIFD entriesWithOffsets = do
      putW16P ba (fromIntegral (length entriesWithOffsets))
      forM_ entriesWithOffsets $ \(e, mOffset) -> do
        putW16P ba (entryTagKey e)
        putW16P ba (entryFormat e)
        putW32P ba (entryCount e)
        case mOffset of
          Nothing -> putByteString $ entryData e <> BS.replicate (4 - BS.length (entryData e)) 0
          Just off -> putW32P ba off
      putW32P ba 0

    putDataSection =
      forM_ outOfLineEntries $ \(e, _) -> do
        putByteString (entryData e)
        when (odd (BS.length (entryData e))) $ putWord8 0
