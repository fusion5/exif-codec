module Codec.Exif.Encode
  ( encodeExif,
    lowerExif,
    encodeRawExif,
  )
where

import Codec.Exif.Patterns
import Codec.Exif.Types
import Control.Monad (forM, forM_)
import Control.Monad.State.Strict
import Data.Binary.Get (Get, getWord32be, getWord32le, runGet)
import Data.Binary.Put
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int
import Data.Map qualified as M
import Data.Maybe (isJust)
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
lowerExif exif = (RawExif (RawTiff order (RawExifData rawIfds)), dataBlocks)
  where
    (order, ExifData ifd0 chained) = getExifData exif
    initialState = LayoutState {lsOffset = 8, lsBlocks = []}
    ((raw0, blocks0, rawChained), _) = runState (lowerAll order ifd0 chained) initialState
    rawIfds = raw0 : map fst rawChained
    dataBlocks = blocks0 : map snd rawChained

lowerAll ::
  ByteOrder ->
  IFD0 ->
  [IFD] ->
  LayoutM (RawIFD, [BS.ByteString], [(RawIFD, [BS.ByteString])])
lowerAll order ifd0 chained = do
  (raw0WithoutNext, blocks0) <- lowerIFD0 order ifd0
  if null chained
    then return (raw0WithoutNext 0, blocks0, [])
    else do
      ifd1Offset <- gets lsOffset
      chainedPairs <- lowerIFDChain order chained
      return (raw0WithoutNext ifd1Offset, blocks0, chainedPairs)

lowerIFD0 :: ByteOrder -> IFD0 -> LayoutM (Word32 -> RawIFD, [BS.ByteString])
lowerIFD0 order (IFD0 entries mExif mGps) = do
  let extraCount = (if isJust mExif then 1 else 0) + (if isJust mGps then 1 else 0)
      numEntries = M.size entries + extraCount
      dirSize = 2 + 12 * fromIntegral numEntries + 4

  modify $ \st -> st {lsOffset = lsOffset st + dirSize, lsBlocks = []}

  rawEntriesList <- forM (M.toList entries) $ uncurry $ processEntry order

  rawExifEntry <- case mExif of
    Nothing -> return []
    Just exifSub -> do
      pending <- lowerExifSubPending order exifSub
      rawEntry <- layoutPendingEntry order pending
      return [(exifSubIFDOffsetTag, rawEntry)]

  rawGpsEntry <- case mGps of
    Nothing -> return []
    Just gps -> do
      pending <- lowerIFDPending order gps
      rawEntry <- layoutPendingEntry order pending
      return [(ImageGPSTag, rawEntry)]

  stAfter <- get
  let ifdBlocks = reverse (lsBlocks stAfter)
  modify $ \st -> st {lsBlocks = []}

  let allEntries = M.fromList (rawEntriesList ++ rawExifEntry ++ rawGpsEntry)
  return (RawIFD (fromIntegral numEntries) allEntries, ifdBlocks)

lowerSubIFDPending ::
  ByteOrder -> Tag -> LayoutM (RawIFD, [BS.ByteString]) -> LayoutM PendingDirectoryEntry
lowerSubIFDPending order (Tag tag) lowerSub = do
  currentOff <- gets lsOffset
  let initialState = LayoutState {lsOffset = currentOff, lsBlocks = []}
      ((rawIfd, ifdBlocks), _) = runState lowerSub initialState
      subBytes = serializeChain order [rawIfd] [ifdBlocks]
  return $ PendingDirectoryEntry tag 4 1 subBytes

lowerExifSubPending :: ByteOrder -> SubIFD -> LayoutM PendingDirectoryEntry
lowerExifSubPending order exifSub =
  lowerSubIFDPending order exifSubIFDOffsetTag (lowerExifSubIFD order exifSub)

lowerExifSubIFD :: ByteOrder -> SubIFD -> LayoutM (RawIFD, [BS.ByteString])
lowerExifSubIFD order (SubIFD entries mInterop) = do
  let extraCount = if isJust mInterop then 1 else 0
      numEntries = M.size entries + extraCount
      dirSize = 2 + 12 * fromIntegral numEntries + 4

  modify $ \st -> st {lsOffset = lsOffset st + dirSize, lsBlocks = []}

  rawEntriesList <- forM (M.toList entries) $ uncurry $ processEntry order

  rawInteropEntry <- case mInterop of
    Nothing -> return []
    Just interop -> do
      pending <- lowerIFDPending order interop
      rawEntry <- layoutPendingEntry order pending
      return [(PhotoInteroperabilityTag, rawEntry)]

  stAfter <- get
  let ifdBlocks = reverse (lsBlocks stAfter)
  modify $ \st -> st {lsBlocks = []}

  let allEntries = M.fromList (rawEntriesList ++ rawInteropEntry)
  return (RawIFD (fromIntegral numEntries) allEntries 0, ifdBlocks)

lowerIFDPending :: ByteOrder -> IFD -> LayoutM PendingDirectoryEntry
lowerIFDPending order (IFD entries) =
  lowerSubIFDPending order PhotoInteroperabilityTag (lowerLeafEntries order entries)

lowerLeafEntries :: ByteOrder -> M.Map Tag DirectoryEntryValue -> LayoutM (RawIFD, [BS.ByteString])
lowerLeafEntries order entries = do
  let numEntries = M.size entries
      dirSize = 2 + 12 * fromIntegral numEntries + 4

  modify $ \st -> st {lsOffset = lsOffset st + dirSize, lsBlocks = []}
  rawEntriesList <- forM (M.toList entries) $ uncurry $ processEntry order

  stAfter <- get
  let ifdBlocks = reverse (lsBlocks stAfter)
  modify $ \st -> st {lsBlocks = []}

  let allEntries = M.fromList rawEntriesList
  return (RawIFD (fromIntegral numEntries) allEntries 0, ifdBlocks)

-- | Layouts a chain of IFDs sequentially using the State monad
lowerIFDChain :: ByteOrder -> [IFD] -> LayoutM [(RawIFD, [BS.ByteString])]
lowerIFDChain _ [] = return []
lowerIFDChain order (ifd : ifds) = do
  let numEntries = M.size (ifdEntries ifd)
      dirSize = 2 + 12 * fromIntegral numEntries + 4

  -- Pre-allocate space for the directory and reset block buffer for this IFD
  modify $ \st -> st {lsOffset = lsOffset st + dirSize, lsBlocks = []}

  rawAndData <- forM (M.toList $ ifdEntries ifd) $ uncurry $ processEntry order

  -- Capture data blocks allocated specifically for this IFD
  stAfter <- get
  let ifdBlocks = reverse (lsBlocks stAfter)
  modify $ \st -> st {lsBlocks = []}

  let rawEntries = M.fromList rawAndData
  nextIfdOffset <- if null ifds then return 0 else gets lsOffset
  let rawIfd = RawIFD (fromIntegral numEntries) rawEntries nextIfdOffset

  rest <- lowerIFDChain order ifds
  return ((rawIfd, ifdBlocks) : rest)

-- | Lowers a single entry
processEntry :: ByteOrder -> Tag -> DirectoryEntryValue -> LayoutM (Tag, RawDirectoryEntry)
processEntry order tag val = do
  let pending = evaluateEntry order tag val
  rawEntry <- layoutPendingEntry order pending
  return (tag, rawEntry)

-- | Evaluates entry values
evaluateEntry :: ByteOrder -> Tag -> DirectoryEntryValue -> PendingDirectoryEntry
evaluateEntry order (Tag tag) val =
  let (typ, count, rawBytes) = encodeValue order val
   in PendingDirectoryEntry tag typ count rawBytes

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
  forM_ (M.toList $ rifdDirectoryEntries rawIfd) $ \(Tag tag, rde) -> do
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
  ASCII bs -> (2, fromIntegral $ BS.length bs, bs)
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
