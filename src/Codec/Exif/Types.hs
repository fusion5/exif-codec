module Codec.Exif.Types
  ( -- * TIFF Header & Envelope
    TiffHeader (..),
    Exif (..),
    tiffByteAlign,
    tiffData,
    ByteAlign (..),
    exifPrefix,

    -- * Exif AST: Core data structure
    ExifData (..),
    IFD (..),
    DirEntry (..),
    Thumbnail (..),

    -- * Exif formats and values
    ExifFormat (..),
    formatCode,
    codeToFormat,
    formatBytesPerComponent,
    URational (..),
    SRational (..),
    TagValue (..),

    -- * Well-known Exif pointer tags
    exifOffsetTag,
    gpsInfoTag,
    interopOffsetTag,
  )
where

import Data.ByteString qualified as BS
import Data.Int (Int16, Int32, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)
import Prelude

-- | Tag 0x8769: Offset to Exif SubIFD
exifOffsetTag :: Word16
exifOffsetTag = 0x8769

-- | Tag 0x8825: Offset to GPS SubIFD
gpsInfoTag :: Word16
gpsInfoTag = 0x8825

-- | Tag 0xA005: Offset to Interoperability SubIFD
interopOffsetTag :: Word16
interopOffsetTag = 0xA005

-- | Abstractly represents the "Exif\0\0" identifier prefix wrapping a payload.
newtype Exif = Envelope
  { envelopePayload :: TiffHeader
  }
  deriving (Show, Eq, Generic)

-- | The TIFF header and structure, with constructors for Intel (little-endian)
-- and Motorola (big-endian) byte order, nesting the payload (such as 'ExifData').
data TiffHeader
  = IntelOrder ExifData
  | MotorolaOrder ExifData
  deriving (Eq, Show, Generic)

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
  { -- | Determines what the entry is, e.g. original date time, flash, focal length, rotation...
    rdeTag :: Word16,
    -- | The data type (byte, ascii, short, long...)
    rdeType :: Word16,
    -- | The number of values present
    rdeCount :: Word32,
    -- | If the value is <= 4 bytes then it's a literal, otherwise it's an offset form
    -- the beginning of the Exif data.
    rdeOffsetOrValue :: Word32
  }

-- | High-level typed representation of an Exif entry's payload values.
data DirectoryEntry
  = UnsignedByte [Word8]
  | Ascii BS.ByteString
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
  | UnsignedLong8 [Word64]
  | SignedLong8 [Word64]
  | Unknown Word16 Word32 BS.ByteString
  deriving (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- Image File Directory (IFD)

data RawIFD = RawIFD
  { rifdNumberOfEntries :: Word16,
    rifdDirectoryEntries :: [RawDirectoryEntry],
    rifdValues :: [BS.ByteString],
    rifdNext :: RawIFD
  }

-- | An Image File Directory (IFD) containing a list of entries and
-- the offset pointing to the next IFD (0 if none).
newtype IFD = IFD
  { ifdEntries :: [DirectoryEntry]
  }
  deriving (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- Thumbnail

-- | Thumbnail image optionally embedded in IFD1.
data Thumbnail
  = JpegThumbnail BS.ByteString
  | TiffThumbnail BS.ByteString
  deriving (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- Exif Abstract Syntax Tree

data RawExifData = RawExifData
  { reIFD0 :: RawIFD
  , reSubIFD :: Maybe RawIFD
  }

-- | The Exif data structure (AST), reflecting the layout described in the Exif specification:
-- primary IFD0 (main image), optional Exif SubIFD (camera settings),
-- optional GPS SubIFD, optional Interoperability SubIFD, optional IFD1 (thumbnail directory),
-- and optional thumbnail image. Byte alignment is determined by the enclosing 'TiffHeader'.
data ExifData = ExifData
  { exifIFD0 :: IFD,
    exifSubIFD :: Maybe IFD,
    exifGpsSubIFD :: Maybe IFD,
    exifInteropSubIFD :: Maybe IFD,
    exifIFD1 :: Maybe IFD,
    exifThumbnail :: Maybe Thumbnail
  }
  deriving (Eq, Show, Generic)
