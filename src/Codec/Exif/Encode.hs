module Codec.Exif.Encode
  ( encodeExif,
  )
where

import Control.Monad (forM_, when)
import Data.Binary.Put
  ( putByteString,
    putInt32le,
    putWord16le,
    putWord32le,
    putWord8,
    runPut,
  )
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
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

data EncodedEntry = EncodedEntry
  { entryTagKey :: Word16,
    entryFormat :: Word16,
    entryCount :: Word32,
    entryData :: BS.ByteString
  }

encodeValue :: Word16 -> ExifValue -> EncodedEntry
encodeValue tagKey = \case
  ExifNumber n ->
    if n < 0
      then
        let bs = LBS.toStrict $ runPut $ putInt32le (fromIntegral n :: Int32)
         in EncodedEntry tagKey 9 1 bs
      else
        if n <= 65535
          then
            let bs = LBS.toStrict $ runPut $ putWord16le (fromIntegral n :: Word16)
             in EncodedEntry tagKey 3 1 bs
          else
            let bs = LBS.toStrict $ runPut $ putWord32le (fromIntegral n :: Word32)
             in EncodedEntry tagKey 4 1 bs
  ExifText str ->
    let raw = Char8.pack str <> BS.singleton 0
     in EncodedEntry tagKey 2 (fromIntegral (BS.length raw)) raw
  ExifRational num den ->
    let isSigned = num < 0 || den < 0
        fmt = if isSigned then 10 else 5
        bs =
          LBS.toStrict $
            runPut $
              if isSigned
                then do
                  putInt32le (fromIntegral num :: Int32)
                  putInt32le (fromIntegral den :: Int32)
                else do
                  putWord32le (fromIntegral num :: Word32)
                  putWord32le (fromIntegral den :: Word32)
     in EncodedEntry tagKey fmt 1 bs
  ExifNumberList xs ->
    let anyNeg = any (< 0) xs
        allShort = all (\x -> x >= 0 && x <= 65535) xs
     in if anyNeg
          then
            let bs = LBS.toStrict $ runPut $ mapM_ (putInt32le . (fromIntegral :: Int -> Int32)) xs
             in EncodedEntry tagKey 9 (fromIntegral (length xs)) bs
          else
            if allShort
              then
                let bs = LBS.toStrict $ runPut $ mapM_ (putWord16le . (fromIntegral :: Int -> Word16)) xs
                 in EncodedEntry tagKey 3 (fromIntegral (length xs)) bs
              else
                let bs = LBS.toStrict $ runPut $ mapM_ (putWord32le . (fromIntegral :: Int -> Word32)) xs
                 in EncodedEntry tagKey 4 (fromIntegral (length xs)) bs
  ExifRationalList rs ->
    let anySigned = any (\(n, d) -> n < 0 || d < 0) rs
        fmt = if anySigned then 10 else 5
        bs = LBS.toStrict $ runPut $ forM_ rs $ \(n, d) ->
          if anySigned
            then do
              putInt32le (fromIntegral n :: Int32)
              putInt32le (fromIntegral d :: Int32)
            else do
              putWord32le (fromIntegral n :: Word32)
              putWord32le (fromIntegral d :: Word32)
     in EncodedEntry tagKey fmt (fromIntegral (length rs)) bs
  ExifUndefined bs ->
    EncodedEntry tagKey 7 (fromIntegral (BS.length bs)) bs
  ExifUnknown fmt count val ->
    let bs = LBS.toStrict $ runPut $ putWord32le (fromIntegral val :: Word32)
     in EncodedEntry tagKey fmt (fromIntegral count) bs

-- | Encode a Map of EXIF tags and values into a standard TIFF-structured EXIF byte string.
encodeExif :: Map ExifTag ExifValue -> BS.ByteString
encodeExif m = LBS.toStrict $ runPut $ do
  -- 1. TIFF Header
  putByteString "II"
  putWord16le 42
  putWord32le 8

  -- 2. IFD0
  putIFD ifd0WithOffsets

  -- 3. ExifSubIFD (if present)
  when hasExifSub $ putIFD exifSubWithOffsets

  -- 4. GpsSubIFD (if present)
  when hasGpsSub $ putIFD gpsSubWithOffsets

  -- 5. Data Section
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

    ifd0Entries = [encodeValue (tagKey tag) val | (tag, val) <- ifd0Raw]
    exifSubEntries = [encodeValue (tagKey tag) val | (tag, val) <- exifSubRaw]
    gpsSubEntries = [encodeValue (tagKey tag) val | (tag, val) <- gpsSubRaw]

    hasExifSub = not (null exifSubEntries)
    hasGpsSub = not (null gpsSubEntries)

    numIFD0 = length ifd0Entries + (if hasExifSub then 1 else 0) + (if hasGpsSub then 1 else 0)
    lenIFD0 = 2 + fromIntegral numIFD0 * 12 + 4

    numExifSub = length exifSubEntries
    lenExifSub = if hasExifSub then 2 + fromIntegral numExifSub * 12 + 4 else 0
    offsetExifSub = 8 + lenIFD0

    numGpsSub = length gpsSubEntries
    lenGpsSub = if hasGpsSub then 2 + fromIntegral numGpsSub * 12 + 4 else 0
    offsetGpsSub = 8 + lenIFD0 + lenExifSub

    dataStartOffset = 8 + lenIFD0 + lenExifSub + lenGpsSub

    exifSubPtr =
      EncodedEntry 0x8769 4 1 (LBS.toStrict $ runPut $ putWord32le (offsetExifSub :: Word32))
    gpsSubPtr =
      EncodedEntry 0x8825 4 1 (LBS.toStrict $ runPut $ putWord32le (offsetGpsSub :: Word32))

    allIFD0 =
      sortBy
        (comparing entryTagKey)
        (ifd0Entries ++ [exifSubPtr | hasExifSub] ++ [gpsSubPtr | hasGpsSub])
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
      putWord16le (fromIntegral (length entriesWithOffsets))
      forM_ entriesWithOffsets $ \(e, mOffset) -> do
        putWord16le (entryTagKey e)
        putWord16le (entryFormat e)
        putWord32le (entryCount e)
        case mOffset of
          Nothing ->
            putByteString (entryData e <> BS.replicate (4 - BS.length (entryData e)) 0)
          Just off ->
            putWord32le off
      putWord32le 0

    putDataSection =
      forM_ outOfLineEntries $ \(e, _) -> do
        putByteString (entryData e)
        when (odd (BS.length (entryData e))) $
          putWord8 0
