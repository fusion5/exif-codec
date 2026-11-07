module Codec.Exif.Types
  ( module Codec.Exif.Types,
  )
where

import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Map qualified as M
import Data.Maybe (listToMaybe)
import Data.Text ()
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)
import Prelude

type Tag = Word16

data Directory = Directory0 | Directory1 | Sub | GPS | Interoperability

-- | Tag 0x8769: Offset to Exif SubIFD
exifSubIFDOffsetTag :: Tag
exifSubIFDOffsetTag = 0x8769

-- | Tag 0x8825: Offset to GPS SubIFD
gpsInfoIFDOffsetTag :: Tag
gpsInfoIFDOffsetTag = 0x8825

-- | Tag 0xA005: Offset to Interoperability SubIFD
interopIFDOffsetTag :: Tag
interopIFDOffsetTag = 0xA005

-- | Abstractly represents the "Exif\0\0" identifier prefix wrapping a payload.
newtype Exif = Exif
  { exifPayload :: Tiff
  }
  deriving (Show, Eq, Generic)

newtype RawExif = RawExif
  { rePayload :: RawTiff
  }
  deriving (Show, Eq, Generic)

-- | The TIFF header and structure, with constructors for Intel (little-endian)
-- and Motorola (big-endian) byte order, nesting the payload (such as 'ExifData').
data Tiff
  = IntelOrder ExifData
  | MotorolaOrder ExifData
  deriving (Eq, Show, Generic)

data RawTiff = RawTiff
  { rtHeader :: ByteOrder,
    rtExifData :: RawExifData
  }
  deriving (Show, Eq, Generic)

-- | Byte order (endianness) of the TIFF/EXIF payload.
data ByteOrder
  = -- | Little-endian ("II")
    Intel
  | -- | Big-endian ("MM")
    Motorola
  deriving (Eq, Show, Generic)

exifPrefix :: BS.ByteString
exifPrefix = "Exif\NUL\NUL"

-- | Unsigned rational number (numerator, denominator).
data URational = URational
  { urNumerator :: Word32,
    urDenominator :: Word32
  }
  deriving (Eq, Show, Generic)

-- | Signed rational number (numerator, denominator).
data SRational = SRational
  { srNumerator :: Int32,
    srDenominator :: Int32
  }
  deriving (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- Typed entry values

-- Low level directory entry
data RawDirectoryEntry = RawDirectoryEntry
  { -- | The data type (byte, ascii, short, long...)
    rdeType :: Word16,
    -- | The number of values present
    rdeCount :: Word32,
    -- | If the value is <= 4 bytes then it's a literal, otherwise it's an offset form
    -- the beginning of the Exif data.
    -- In the case of sub-IFDs then it's an offset to the sub-IFD, obviously.
    rdeOffsetOrValue :: Word32
  }
  deriving (Show, Eq, Generic)

-- | Representing an entry during the encoding pipeline:
data PendingDirectoryEntry = PendingDirectoryEntry
  { peTag :: Word16,
    peType :: Word16,
    peCount :: Word32,
    -- | Always holds the raw payload bytes (whether 2, 4, or 5000 bytes)
    peData :: BS.ByteString
  }

data DirectoryEntryValue
  = UnsignedByte [Word8]
  | ASCII BS.ByteString
  | UnsignedShort [Word16]
  | UnsignedLong [Word32]
  | UnsignedRational [URational]
  | SignedByte [Int8]
  | Undefined BS.ByteString
  | SignedShort [Int16]
  | SignedLong [Int32]
  | SignedRational [SRational]
  | SingleFloat [Float]
  | DoubleFloat [Double]
  | -- | Technically part of the BigTIFF specification, not standard EXIF/TIFF (which limits offsets
    -- and sizes to 32-bit).
    UnsignedLong8 [Word64]
  | -- | Same as above
    SignedLong8 [Int64]
  | Unknown Word16 Word32 BS.ByteString
  deriving (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- Image File Directory (IFD)

data RawIFD = RawIFD
  { rifdNumberOfEntries :: Word16,
    -- | The key is the tag and the value is the entry value
    rifdDirectoryEntries :: M.Map Word16 RawDirectoryEntry,
    rifdNextOffset :: Word32
  }
  deriving (Show, Eq, Generic)

-- | Leaf directories that cannot branch to any sub-IFDs
newtype IFD = IFD
  { ifdEntries :: M.Map Tag DirectoryEntryValue
  }
  deriving (Eq, Show, Generic)

-- | Exif SubIFD: contains camera settings and can ONLY branch to Interop
data SubIFD = SubIFD
  { ifdSubEntries :: M.Map Tag DirectoryEntryValue,
    ifdSubInterop :: Maybe IFD
  }
  deriving (Eq, Show, Generic)

-- | IFD0: primary image directory; can branch to SubIFD and GPSIFD
data IFD0 = IFD0
  { ifd0Entries :: M.Map Tag DirectoryEntryValue,
    ifd0SubIFD :: Maybe SubIFD,
    ifd0GPSIFD :: Maybe IFD
  }
  deriving (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- Exif Abstract Syntax Tree

newtype RawExifData = RawExifData
  { rawIFDs :: [RawIFD]
  }
  deriving (Show, Eq, Generic)

-- | The Exif data structure (AST), reflecting the layout described in the Exif specification:
-- primary IFD0 (main image), optional Exif SubIFD (camera settings),
-- optional GPS SubIFD, optional Interoperability SubIFD, and optional chained IFDs (IFD1, etc.).
data ExifData = ExifData
  { exifIFD0 :: IFD0,
    exifChainedIFDs :: [IFD]
  }
  deriving (Eq, Show, Generic)

getExifData :: Exif -> (ByteOrder, ExifData)
getExifData (Exif (IntelOrder exifData)) = (Intel, exifData)
getExifData (Exif (MotorolaOrder exifData)) = (Motorola, exifData)

getDirectoryTag :: Exif -> Directory -> Tag -> Maybe DirectoryEntryValue
getDirectoryTag exif directory tag =
  case directory of
    Directory0 -> M.lookup tag (ifd0Entries ifd0)
    Directory1 -> do
      ifd1 <- listToMaybe ifds
      M.lookup tag (ifdEntries ifd1)
    Sub -> do
      subIFD <- ifd0SubIFD ifd0
      M.lookup tag $ ifdSubEntries subIFD
    GPS -> do
      gpsIFD <- ifd0SubIFD ifd0
      M.lookup tag $ ifdSubEntries gpsIFD
    Interoperability -> do
      subIFD <- ifd0SubIFD ifd0
      interopIFD <- ifdSubInterop subIFD
      M.lookup tag $ ifdEntries interopIFD
  where
    (_, ExifData ifd0 ifds) = getExifData exif

setDirectoryTag :: Exif -> Directory -> Tag -> Maybe DirectoryEntryValue
setDirectoryTag = error "TODO"
