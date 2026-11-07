module Codec.Exif.Decode
  ( decodeExif,
    decodeTiff,
    ParseError (..),
  )
where

import Codec.Exif.Types
import Control.Monad (replicateM, unless)
import Data.Binary.Get
  ( Get,
    getByteString,
    getDoublebe,
    getDoublele,
    getFloatbe,
    getFloatle,
    getInt16be,
    getInt16le,
    getInt32be,
    getInt32le,
    getInt64be,
    getInt64le,
    getInt8,
    getWord16be,
    getWord16le,
    getWord32be,
    getWord32le,
    getWord64be,
    getWord64le,
    getWord8,
    runGetOrFail,
  )
import Data.Binary.Put (putWord32be, putWord32le, runPut)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Functor ((<&>))
import Data.Int (Int16, Int32, Int64)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Word (Word16, Word32, Word64)
import Prelude

data ParseError
  = ParseError String
  | MissingExifPrefix
  | IFDOffsetOutOfBounds Word32
  | IFDChainCycle
  deriving (Show, Eq)

-- | Decodes raw EXIF bytestring into the high-level generic AST
decodeExif :: BS.ByteString -> Either ParseError Exif
decodeExif bs = decodeRawExif bs <&> Exif . convertRawTiff (BS.drop 6 bs) . rePayload

-- | Decodes just the structural Tiff block
decodeTiff :: BS.ByteString -> Either ParseError Tiff
decodeTiff bs = decodeRawTiff bs <&> convertRawTiff bs

-- ---------------------------------------------------------------------------
-- Conversion: Raw AST -> High-Level AST

convertRawTiff :: BS.ByteString -> RawTiff -> Tiff
convertRawTiff tiffBs (RawTiff order (RawExifData rawIfds)) =
  let exifData = case rawIfds of
        [] -> ExifData (IFD0 M.empty Nothing Nothing) []
        (raw0 : rest) ->
          let ifd0 = convertRawIFD0 order tiffBs raw0
              chained = map (convertRawIFD order tiffBs) rest
           in ExifData ifd0 chained
   in case order of
        Intel -> IntelOrder exifData
        Motorola -> MotorolaOrder exifData

-- | Looks up an offset tag, parses its IFD chain, and applies a conversion function.
lookupSubIFD ::
  ByteOrder ->
  BS.ByteString ->
  Tag ->
  M.Map Tag RawDirectoryEntry ->
  (RawIFD -> a) ->
  Maybe a
lookupSubIFD order tiffBs offsetTag entries convertSub = do
  rde <- M.lookup offsetTag entries
  case parseIFDChain order tiffBs (rdeOffsetOrValue rde) S.empty of
    Right (subRaw : _) -> Just (convertSub subRaw)
    _ -> Nothing

convertRawIFD0 :: ByteOrder -> BS.ByteString -> RawIFD -> IFD0
convertRawIFD0 order tiffBs rawIFD = IFD0 entries mExif mGps
  where
    allEntries = rifdDirectoryEntries rawIFD
    mExif = lookupSubIFD order tiffBs exifSubIFDOffsetTag allEntries (convertRawExifSubIFD order tiffBs)
    mGps = lookupSubIFD order tiffBs gpsInfoIFDOffsetTag allEntries (convertRawIFD order tiffBs)
    cleanEntries = M.delete exifSubIFDOffsetTag $ M.delete gpsInfoIFDOffsetTag allEntries
    entries = M.mapWithKey (convertEntry order tiffBs) cleanEntries

convertRawExifSubIFD :: ByteOrder -> BS.ByteString -> RawIFD -> SubIFD
convertRawExifSubIFD order tiffBs rawIFD = SubIFD entries mInterop
  where
    allEntries = rifdDirectoryEntries rawIFD
    mInterop = lookupSubIFD order tiffBs interopIFDOffsetTag allEntries (convertRawIFD order tiffBs)
    cleanEntries = M.filterWithKey (\k _ -> k /= interopIFDOffsetTag) allEntries
    entries = M.mapWithKey (convertEntry order tiffBs) cleanEntries

convertRawIFD :: ByteOrder -> BS.ByteString -> RawIFD -> IFD
convertRawIFD order tiffBs rawIFD =
  IFD $ M.mapWithKey (convertEntry order tiffBs) (rifdDirectoryEntries rawIFD)

convertEntry :: ByteOrder -> BS.ByteString -> Tag -> RawDirectoryEntry -> DirectoryEntryValue
convertEntry order tiffBs _tag rde =
  let typ = rdeType rde
      cnt = rdeCount rde
      typeSize = getTypeSize typ
      totalSize = cnt * typeSize

      -- Per TIFF specification, if total payload size is <= 4 bytes,
      -- it resides in the offset field padded to 4 bytes.
      bytes =
        if totalSize <= 4
          then BS.take (fromIntegral totalSize) (word32ToBytes order (rdeOffsetOrValue rde))
          else BS.take (fromIntegral totalSize) (BS.drop (fromIntegral (rdeOffsetOrValue rde)) tiffBs)
   in case runGetEither (getValues order typ cnt) bytes of
        Right val -> val
        Left _ -> Unknown typ cnt bytes

-- | Maps standard EXIF Directory Entry Types to byte counts
getTypeSize :: Word16 -> Word32
getTypeSize 1 = 1 -- BYTE
getTypeSize 2 = 1 -- ASCII
getTypeSize 3 = 2 -- SHORT
getTypeSize 4 = 4 -- LONG
getTypeSize 5 = 8 -- RATIONAL
getTypeSize 6 = 1 -- SBYTE
getTypeSize 7 = 1 -- UNDEFINED
getTypeSize 8 = 2 -- SSHORT
getTypeSize 9 = 4 -- SLONG
getTypeSize 10 = 8 -- SRATIONAL
getTypeSize 11 = 4 -- FLOAT
getTypeSize 12 = 8 -- DOUBLE
getTypeSize 16 = 8 -- LONG8 (BigTIFF)
getTypeSize 17 = 8 -- SLONG8 (BigTIFF)
getTypeSize _ = 1 -- Default/Unknown Fallback

-- | Extracts the proper Typed array using binary primitives
getValues :: ByteOrder -> Word16 -> Word32 -> Get DirectoryEntryValue
getValues order typ count = do
  let n = fromIntegral count
  case typ of
    1 -> UnsignedByte <$> replicateM n getWord8
    2 -> ASCII <$> getByteString n
    3 -> UnsignedShort <$> replicateM n (getW16 order)
    4 -> UnsignedLong <$> replicateM n (getW32 order)
    5 -> UnsignedRational <$> replicateM n (getURational order)
    6 -> SignedByte <$> replicateM n getInt8
    7 -> Undefined <$> getByteString n
    8 -> SignedShort <$> replicateM n (getI16 order)
    9 -> SignedLong <$> replicateM n (getI32 order)
    10 -> SignedRational <$> replicateM n (getSRational order)
    11 -> SingleFloat <$> replicateM n (getFloat32 order)
    12 -> DoubleFloat <$> replicateM n (getFloat64 order)
    16 -> UnsignedLong8 <$> replicateM n (getW64 order)
    17 -> SignedLong8 <$> replicateM n (getI64 order)
    _ -> fail $ "Unknown EXIF type: " ++ show typ

-- ---------------------------------------------------------------------------
-- Decoding Raw Pipelines

-- | Helper to bridge strict ByteStrings to binary's lazy Get runner,
-- mapping the 3-tuple error/success into a clean Either.
runGetEither :: Get a -> BS.ByteString -> Either ParseError a
runGetEither getBs bs =
  case runGetOrFail getBs (BL.fromStrict bs) of
    Left (_, _, err) -> Left $ ParseError err
    Right (_, _, val) -> Right val

decodeRawExif :: BS.ByteString -> Either ParseError RawExif
decodeRawExif bs = do
  unless (exifPrefix `BS.isPrefixOf` bs) $ Left MissingExifPrefix
  -- Everything from here on is relative to the start of the TIFF header.
  RawExif <$> decodeRawTiff (BS.drop 6 bs)

decodeRawTiff :: BS.ByteString -> Either ParseError RawTiff
decodeRawTiff tiffBS = do
  -- Read the endianness and the offset to the first IFD
  (order, ifd0Offset) <- runGetEither parseTiffHeader tiffBS
  -- Follow the linked list of IFDs using an empty Set for cycle detection
  ifds <- parseIFDChain order tiffBS ifd0Offset S.empty
  Right $ RawTiff order (RawExifData ifds)

-- | 2. Sub-IFD Parser (Exposed for Step 2)
parseRawIFD :: ByteOrder -> BS.ByteString -> Word32 -> Either ParseError RawIFD
parseRawIFD order tiffBs offset
  | fromIntegral offset >= BS.length tiffBs = Left $ IFDOffsetOutOfBounds offset
  | otherwise = runGetEither (getIFD order) (BS.drop (fromIntegral offset) tiffBs)

-- ---------------------------------------------------------------------------
-- Internal Parsing Logic

-- | Parses the 8-byte TIFF header: ByteOrder (2), tiff constant 42 (2), IFD0 Offset (4)
parseTiffHeader :: Get (ByteOrder, Word32)
parseTiffHeader = do
  endianBytes <- getByteString 2
  order <- case endianBytes of
    "II" -> return Intel
    "MM" -> return Motorola
    _ -> fail $ "Invalid byte order mark: " ++ show endianBytes
  tiffConstant <- getW16 order
  if tiffConstant /= 42
    then fail $ "Invalid TIFF constant (expected 42): " ++ show tiffConstant
    else do
      offset <- getW32 order
      return (order, offset)

-- | Recursively parses the linked list of IFDs.
-- Keeps track of 'seen' offsets in a Set to prevent infinite loops in malformed EXIF data.
parseIFDChain :: ByteOrder -> BS.ByteString -> Word32 -> S.Set Word32 -> Either ParseError [RawIFD]
parseIFDChain _ _ 0 _ = Right []
parseIFDChain order tiffBs offset seen
  | fromIntegral offset >= BS.length tiffBs = Right []
  | S.member offset seen = Left IFDChainCycle
  | otherwise = do
      ifd <- parseRawIFD order tiffBs offset
      rest <- parseIFDChain order tiffBs (rifdNextOffset ifd) (S.insert offset seen)
      return (ifd : rest)

-- | Binary Get monad for a single IFD block
getIFD :: ByteOrder -> Get RawIFD
getIFD order = do
  numEntries <- getW16 order
  entries <- replicateM (fromIntegral numEntries) (getEntry order)
  nextOffset <- getW32 order
  return
    RawIFD
      { rifdNumberOfEntries = numEntries,
        rifdDirectoryEntries = M.fromList entries,
        rifdNextOffset = nextOffset
      }

-- | Parses a 12-byte directory entry and returns the Tag alongside it
getEntry :: ByteOrder -> Get (Tag, RawDirectoryEntry)
getEntry order = do
  tag <- getW16 order
  typ <- getW16 order
  count <- getW32 order
  offsetOrVal <- getW32 order
  return (tag, RawDirectoryEntry typ count offsetOrVal)

-- ---------------------------------------------------------------------------
-- Endianness / Decoding Helpers

-- Helper: Re-serializes the Word32 representing literal values <= 4 bytes properly
word32ToBytes :: ByteOrder -> Word32 -> BS.ByteString
word32ToBytes Intel w = BL.toStrict $ runPut $ putWord32le w
word32ToBytes Motorola w = BL.toStrict $ runPut $ putWord32be w

getW16 :: ByteOrder -> Get Word16
getW16 Intel = getWord16le
getW16 Motorola = getWord16be

getW32 :: ByteOrder -> Get Word32
getW32 Intel = getWord32le
getW32 Motorola = getWord32be

getW64 :: ByteOrder -> Get Word64
getW64 Intel = getWord64le
getW64 Motorola = getWord64be

getI16 :: ByteOrder -> Get Int16
getI16 Intel = getInt16le
getI16 Motorola = getInt16be

getI32 :: ByteOrder -> Get Int32
getI32 Intel = getInt32le
getI32 Motorola = getInt32be

getI64 :: ByteOrder -> Get Int64
getI64 Intel = getInt64le
getI64 Motorola = getInt64be

getFloat32 :: ByteOrder -> Get Float
getFloat32 Intel = getFloatle
getFloat32 Motorola = getFloatbe

getFloat64 :: ByteOrder -> Get Double
getFloat64 Intel = getDoublele
getFloat64 Motorola = getDoublebe

getURational :: ByteOrder -> Get URational
getURational order = URational <$> getW32 order <*> getW32 order

getSRational :: ByteOrder -> Get SRational
getSRational order = SRational <$> getI32 order <*> getI32 order
