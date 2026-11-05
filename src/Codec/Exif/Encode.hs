module Codec.Exif.Encode
  ( encodeExif,
    lowerExif,
    encodeRawExif,
  )
where

import Codec.Exif.Types
import Control.Monad (forM, forM_)
import Control.Monad.State.Strict
import Data.Binary.Get (Get, getWord32be, getWord32le, runGet)
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int
import Data.Map qualified as M
import Data.Word
import Prelude

-- | Complete encoding pipeline
encodeExif :: Exif -> BS.ByteString
encodeExif exif =
  let (rawExif, dataBlocks) = lowerExif exif
   in exifPrefix <> encodeRawExif rawExif dataBlocks

-- ---------------------------------------------------------------------------
-- Phase 1: AST Lowering & Memory Layout with State Monad

-- | State tracking both the linear byte offset and the accumulated external data blocks
data LayoutState = LayoutState
  { lsOffset :: !Word32,
    lsBlocks :: [BS.ByteString] -- Stored in reverse order for O(1) prepending
  }
  deriving (Show)

type LayoutM = State LayoutState

-- | Lowers the high-level AST into structural TIFF headers and collects external payloads
lowerExif :: Exif -> (RawExif, [[BS.ByteString]])
lowerExif (Exif tiff) =
  let (order, ExifData ifds) = case tiff of
        IntelOrder ed -> (Intel, ed)
        MotorolaOrder ed -> (Motorola, ed)

      initialState = LayoutState {lsOffset = 8, lsBlocks = []}
      (rawIfds, finalState) = runState (lowerIFDChain order ifds) initialState
      -- Reverse blocks to restore correct layout order
      dataBlocks = [reverse (lsBlocks finalState)]
   in (RawExif (RawTiff order (RawExifData rawIfds)), dataBlocks)

-- | Layouts a chain of IFDs sequentially using the State monad
lowerIFDChain :: ByteOrder -> [IFD] -> LayoutM [RawIFD]
lowerIFDChain _ [] = return []
lowerIFDChain order (ifd : ifds) = do
  let numEntries = M.size (ifdEntries ifd)
      dirSize = 2 + 12 * fromIntegral numEntries + 4

  -- Pre-allocate space for the directory itself
  modify $ \st -> st {lsOffset = lsOffset st + dirSize}

  -- Process all entries (external data blocks are automatically pushed to state)
  rawAndData <- forM (M.toList $ ifdEntries ifd) $ uncurry $ processEntry order

  let rawEntries = M.fromList rawAndData
  nextIfdOffset <- if null ifds then return 0 else gets lsOffset
  let rawIfd = RawIFD (fromIntegral numEntries) rawEntries nextIfdOffset

  restIfds <- lowerIFDChain order ifds
  return (rawIfd : restIfds)

-- | Lowers a single entry
processEntry :: ByteOrder -> Tag -> DirectoryEntryValue -> LayoutM (Tag, RawDirectoryEntry)
processEntry order tag val = do
  pending <- evaluateEntry order tag val
  rawEntry <- layoutPendingEntry order pending
  return (tag, rawEntry)

-- | Evaluates entry values, handling recursive sub-IFDs
evaluateEntry :: ByteOrder -> Tag -> DirectoryEntryValue -> LayoutM PendingDirectoryEntry
evaluateEntry order tag val =
  case val of -- Sub-IFDs are recursively compiled into a local blob and treated as external bytes
    IFDPointer subIfd -> do
      currentOff <- gets lsOffset
      -- Run a localized state computation for the sub-IFD chain
      let initialState = LayoutState {lsOffset = currentOff, lsBlocks = []}
          (rawSubIfds, finalSubState) = runState (lowerIFDChain order [subIfd]) initialState
          subBytes = serializeChain order rawSubIfds [reverse (lsBlocks finalSubState)]
      return $ PendingDirectoryEntry tag 4 1 subBytes
    _ -> do
      let (typ, count, rawBytes) = encodeValue order val
      return $ PendingDirectoryEntry tag typ count rawBytes

-- | Decides if entry bytes fit in the 4-byte pointer or require external state allocation
layoutPendingEntry :: ByteOrder -> PendingDirectoryEntry -> LayoutM RawDirectoryEntry
layoutPendingEntry order pe =
  if len <= 4
    then do
      let valOrOff = packToWord32 order bytes
      return $ RawDirectoryEntry (peType pe) (peCount pe) valOrOff
    else do
      let alignedBytes = if odd len then bytes `BS.snoc` 0 else bytes
          payloadLen = fromIntegral (BS.length alignedBytes)

      currentOff <- gets lsOffset
      -- Update offset and prepend the block to state
      modify $ \st ->
        st
          { lsOffset = lsOffset st + payloadLen,
            lsBlocks = alignedBytes : lsBlocks st
          }
      return $ RawDirectoryEntry (peType pe) (peCount pe) currentOff
  where
    bytes = peData pe
    len = BS.length bytes

-- ---------------------------------------------------------------------------
-- Phase 2: Binary Serialization (Pure)

encodeRawExif :: RawExif -> [[BS.ByteString]] -> BS.ByteString
encodeRawExif (RawExif (RawTiff order (RawExifData rawIfds))) blocks =
  let tiffHeader = BL.toStrict $ runPut $ do
        case order of
          Intel -> putByteString "II"
          Motorola -> putByteString "MM"
        putW16 order 42
        putW32 order 8
   in tiffHeader <> serializeChain order rawIfds blocks

serializeChain :: ByteOrder -> [RawIFD] -> [[BS.ByteString]] -> BS.ByteString
serializeChain order rawIfds blocks = BS.concat $ do
  (rawIfd, ifdDataBlocks) <- zip rawIfds blocks
  return $ putRawIFD order rawIfd <> BS.concat ifdDataBlocks

putRawIFD :: ByteOrder -> RawIFD -> BS.ByteString
putRawIFD order rawIfd = BL.toStrict $ runPut $ do
  putW16 order (rifdNumberOfEntries rawIfd)
  forM_ (M.toList $ rifdDirectoryEntries rawIfd) $ \(tag, rde) -> do
    putW16 order tag
    putW16 order (rdeType rde)
    putW32 order (rdeCount rde)
    putW32 order (rdeOffsetOrValue rde)
  putW32 order (rifdNextOffset rawIfd)

-- ---------------------------------------------------------------------------
-- Payload Encoding Helpers

encodeValue :: ByteOrder -> DirectoryEntryValue -> (Word16, Word32, BS.ByteString)
encodeValue order val = case val of
  UnsignedByte ws -> (1, fromIntegral $ length ws, BL.toStrict $ runPut $ mapM_ putWord8 ws)
  Ascii bs -> (2, fromIntegral $ BS.length bs, bs)
  UnsignedShort ws -> (3, fromIntegral $ length ws, BL.toStrict $ runPut $ mapM_ (putW16 order) ws)
  UnsignedLong ws -> (4, fromIntegral $ length ws, BL.toStrict $ runPut $ mapM_ (putW32 order) ws)
  UnsignedRational rs -> (5, fromIntegral $ length rs, BL.toStrict $ runPut $ mapM_ (putURational order) rs)
  SignedByte is -> (6, fromIntegral $ length is, BL.toStrict $ runPut $ mapM_ putInt8 is)
  Undefined bs -> (7, fromIntegral $ BS.length bs, bs)
  SignedShort is -> (8, fromIntegral $ length is, BL.toStrict $ runPut $ mapM_ (putI16 order) is)
  SignedLong is -> (9, fromIntegral $ length is, BL.toStrict $ runPut $ mapM_ (putI32 order) is)
  SignedRational rs -> (10, fromIntegral $ length rs, BL.toStrict $ runPut $ mapM_ (putSRational order) rs)
  SingleFloat fs -> (11, fromIntegral $ length fs, BL.toStrict $ runPut $ mapM_ (putFloat32 order) fs)
  DoubleFloat ds -> (12, fromIntegral $ length ds, BL.toStrict $ runPut $ mapM_ (putFloat64 order) ds)
  UnsignedLong8 ws -> (16, fromIntegral $ length ws, BL.toStrict $ runPut $ mapM_ (putW64 order) ws)
  SignedLong8 is -> (17, fromIntegral $ length is, BL.toStrict $ runPut $ mapM_ (putI64 order) is)
  Unknown typ count bs -> (typ, count, bs)
  IFDPointer _ -> error "IFDPointer is handled structurally by evaluateEntry"

packToWord32 :: ByteOrder -> BS.ByteString -> Word32
packToWord32 order bytes =
  let padded = BL.fromStrict bytes <> BL.replicate (4 - fromIntegral (BS.length bytes)) 0
   in runGet (getW32 order) padded

-- ---------------------------------------------------------------------------
-- Endian-Aware Putters/Getters
getW32 :: ByteOrder -> Get Word32
getW32 Intel = getWord32le
getW32 Motorola = getWord32be

putW16 :: ByteOrder -> Word16 -> Put
putW16 Intel = putWord16le
putW16 Motorola = putWord16be

putW32 :: ByteOrder -> Word32 -> Put
putW32 Intel = putWord32le
putW32 Motorola = putWord32be

putW64 :: ByteOrder -> Word64 -> Put
putW64 Intel = putWord64le
putW64 Motorola = putWord64be

putI16 :: ByteOrder -> Int16 -> Put
putI16 Intel = putInt16le
putI16 Motorola = putInt16be

putI32 :: ByteOrder -> Int32 -> Put
putI32 Intel = putInt32le
putI32 Motorola = putInt32be

putI64 :: ByteOrder -> Int64 -> Put
putI64 Intel = putInt64le
putI64 Motorola = putInt64be

putFloat32 :: ByteOrder -> Float -> Put
putFloat32 Intel = putFloatle
putFloat32 Motorola = putFloatbe

putFloat64 :: ByteOrder -> Double -> Put
putFloat64 Intel = putDoublele
putFloat64 Motorola = putDoublebe

putURational :: ByteOrder -> URational -> Put
putURational order (URational n d) = putW32 order n >> putW32 order d

putSRational :: ByteOrder -> SRational -> Put
putSRational order (SRational n d) = putI32 order n >> putI32 order d
