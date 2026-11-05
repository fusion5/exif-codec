module Codec.Exif.Decode
  ( decodeExif,
    decodeExifRaw,
    ParseError (..),
  )
where

import Codec.Exif.Types
import Control.Monad (replicateM)
import Data.Bifunctor
import Data.Binary.Get
import Data.Binary.Get
  ( Get,
    getByteString,
    getWord16be,
    getWord16le,
    getWord32be,
    getWord32le,
    runGetOrFail,
  )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Word (Word16, Word32)
import Prelude

data ParseError
  = ParseError String
  | MissingExifPrefix
  | IFDOffsetOutOfBounds Word32
  | IFDChainCycle
  deriving (Show, Eq)

decodeExif :: BS.ByteString -> Exif
decodeExif = error "TODO"

decodeExifRaw :: BS.ByteString -> Tiff
decodeExifRaw = error "TODO"

-- | Helper to bridge strict ByteStrings to binary's lazy Get runner,
-- mapping the 3-tuple error/success into a clean Either.
runGetEither :: Get a -> BS.ByteString -> Either ParseError a
runGetEither getBs bs =
  case runGetOrFail getBs (BL.fromStrict bs) of
    Left (_, _, err) -> Left $ ParseError err
    Right (_, _, val) -> Right val

-- | 1. Main decoding pipeline
decodeRawExif :: BS.ByteString -> Either ParseError RawExif
decodeRawExif bs = do
  let prefix = "Exif\NUL\NUL"

  if not (prefix `BS.isPrefixOf` bs)
    then Left MissingExifPrefix
    else do
      -- Everything from here on is relative to the start of the TIFF header.
      let tiffBs = BS.drop 6 bs
      -- Read the endianness and the offset to the first IFD
      (order, ifd0Offset) <- runGetEither parseTiffHeader tiffBs
      -- Follow the linked list of IFDs using an empty Set for cycle detection
      ifds <- parseIFDChain order tiffBs ifd0Offset S.empty
      Right $ RawExif (RawTiff order (RawExifData ifds))

-- | 2. Sub-IFD Parser (Exposed for Step 2)
parseRawIFD :: ByteOrder -> BS.ByteString -> Word32 -> Either ParseError RawIFD
parseRawIFD order tiffBs offset
  | fromIntegral offset >= BS.length tiffBs = Left $ IFDOffsetOutOfBounds offset
  | otherwise = runGetEither (getIFD order) (BS.drop (fromIntegral offset) tiffBs)

-- ---------------------------------------------------------------------------
-- Internal Parsing Logic

-- | Parses the 8-byte TIFF header: ByteOrder (2), Magic 42 (2), IFD0 Offset (4)
parseTiffHeader :: Get (ByteOrder, Word32)
parseTiffHeader = do
  -- Note: getByteString returns a strict ByteString, matching our literal
  endianBytes <- getByteString 2
  order <- case endianBytes of
    "II" -> return Intel
    "MM" -> return Motorola
    _ -> fail $ "Invalid byte order mark: " ++ show endianBytes
  magic <- getW16 order
  if magic /= 42
    then fail $ "Invalid TIFF magic number (expected 42): " ++ show magic
    else do
      offset <- getW32 order
      return (order, offset)

-- | Recursively parses the linked list of IFDs.
-- Keeps track of 'seen' offsets in a Set to prevent infinite loops in malformed EXIF data.
parseIFDChain :: ByteOrder -> BS.ByteString -> Word32 -> S.Set Word32 -> Either ParseError [RawIFD]
parseIFDChain _ _ 0 _ = Right []
parseIFDChain order tiffBs offset seen
  | S.member offset seen = Left IFDChainCycle
  | otherwise = do
      ifd <- parseRawIFD order tiffBs offset
      -- Insert current offset and recurse
      rest <- parseIFDChain order tiffBs (rifdNextOffset ifd) (S.insert offset seen)
      return (ifd : rest)

-- | Binary Get monad for a single IFD block
getIFD :: ByteOrder -> Get RawIFD
getIFD order = do
  numEntries <- getW16 order

  -- Each directory entry is exactly 12 bytes.
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
-- Endianness Helpers

getW16 :: ByteOrder -> Get Word16
getW16 Intel = getWord16le
getW16 Motorola = getWord16be

getW32 :: ByteOrder -> Get Word32
getW32 Intel = getWord32le
getW32 Motorola = getWord32be
