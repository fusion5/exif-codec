module Codec.Exif.Types
  ( -- * Existing representations
    ByteAlign (..),
    Header (..),
    Envelope (..),
    exifPrefix,

    -- * Exif AST: Core data structure
    ExifData (..),
    TiffHeader (..),
    IFD (..),
    DirEntry (..),
    RawDirEntry (..),
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
import Data.Map (Map)
import Data.Word (Word16, Word32, Word8)
import GHC.Generics (Generic)
import Graphics.HsExif
  ( ExifTag (..),
    ExifValue (..),
  )
import Prelude

-- ---------------------------------------------------------------------------
-- Existing legacy types

-- | Abstractly represents the "Exif\0\0" identifier
newtype Envelope = Envelope Header
  deriving (Show, Eq)

data Header
  = IntelOrder (Map ExifTag ExifValue)
  | MotorolaOrder (Map ExifTag ExifValue)
  deriving (Show, Eq)

-- | Byte alignment (endianness) of the TIFF/EXIF payload.
data ByteAlign
  = -- | Little-endian ("II")
    Intel
  | -- | Big-endian ("MM")
    Motorola
  deriving (Eq, Show, Generic)

exifPrefix :: BS.ByteString
exifPrefix = "Exif\NUL\NUL"

-- ---------------------------------------------------------------------------
-- Well-known pointer tags

-- | Tag 0x8769: Offset to Exif SubIFD
exifOffsetTag :: Word16
exifOffsetTag = 0x8769

-- | Tag 0x8825: Offset to GPS SubIFD
gpsInfoTag :: Word16
gpsInfoTag = 0x8825

-- | Tag 0xA005: Offset to Interoperability SubIFD
interopOffsetTag :: Word16
interopOffsetTag = 0xA005

-- ---------------------------------------------------------------------------
-- TIFF Header

-- | The 8-byte TIFF header at the beginning of the TIFF stream.
data TiffHeader = TiffHeader
  { tiffByteAlign :: ByteAlign,
    tiffMagic :: Word16,
    tiffFirstIfdOffset :: Word32
  }
  deriving (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- Data Formats (TIFF 6.0 / Exif Specification Types 1-12)

-- | TIFF/Exif data format (field type code).
data ExifFormat
  = FormatUnsignedByte -- ^ 1: 8-bit unsigned integer (BYTE)
  | FormatAsciiString -- ^ 2: Null-terminated ASCII string (ASCII)
  | FormatUnsignedShort -- ^ 3: 16-bit unsigned integer (SHORT)
  | FormatUnsignedLong -- ^ 4: 32-bit unsigned integer (LONG)
  | FormatUnsignedRational -- ^ 5: Two 32-bit unsigned integers: numerator, denominator (RATIONAL)
  | FormatSignedByte -- ^ 6: 8-bit signed integer (SBYTE)
  | FormatUndefined -- ^ 7: 8-bit uninterpreted byte (UNDEFINED)
  | FormatSignedShort -- ^ 8: 16-bit signed integer (SSHORT)
  | FormatSignedLong -- ^ 9: 32-bit signed integer (SLONG)
  | FormatSignedRational -- ^ 10: Two 32-bit signed integers: numerator, denominator (SRATIONAL)
  | FormatSingleFloat -- ^ 11: 32-bit IEEE float (FLOAT)
  | FormatDoubleFloat -- ^ 12: 64-bit IEEE double float (DOUBLE)
  | FormatUnknown Word16 -- ^ Unknown or non-standard format code
  deriving (Eq, Show, Generic)

-- | Convert an 'ExifFormat' to its corresponding 16-bit integer code.
formatCode :: ExifFormat -> Word16
formatCode = \case
  FormatUnsignedByte -> 1
  FormatAsciiString -> 2
  FormatUnsignedShort -> 3
  FormatUnsignedLong -> 4
  FormatUnsignedRational -> 5
  FormatSignedByte -> 6
  FormatUndefined -> 7
  FormatSignedShort -> 8
  FormatSignedLong -> 9
  FormatSignedRational -> 10
  FormatSingleFloat -> 11
  FormatDoubleFloat -> 12
  FormatUnknown w -> w

-- | Parse a 16-bit integer code into an 'ExifFormat'.
codeToFormat :: Word16 -> ExifFormat
codeToFormat = \case
  1 -> FormatUnsignedByte
  2 -> FormatAsciiString
  3 -> FormatUnsignedShort
  4 -> FormatUnsignedLong
  5 -> FormatUnsignedRational
  6 -> FormatSignedByte
  7 -> FormatUndefined
  8 -> FormatSignedShort
  9 -> FormatSignedLong
  10 -> FormatSignedRational
  11 -> FormatSingleFloat
  12 -> FormatDoubleFloat
  w -> FormatUnknown w

-- | Byte size per component for each format.
formatBytesPerComponent :: ExifFormat -> Int
formatBytesPerComponent = \case
  FormatUnsignedByte -> 1
  FormatAsciiString -> 1
  FormatUnsignedShort -> 2
  FormatUnsignedLong -> 4
  FormatUnsignedRational -> 8
  FormatSignedByte -> 1
  FormatUndefined -> 1
  FormatSignedShort -> 2
  FormatSignedLong -> 4
  FormatSignedRational -> 8
  FormatSingleFloat -> 4
  FormatDoubleFloat -> 8
  FormatUnknown _ -> 1

-- ---------------------------------------------------------------------------
-- Rational numbers

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

-- | High-level typed representation of an Exif entry's payload value.
data TagValue
  = ValUnsignedByte [Word8]
  | ValAscii BS.ByteString
  | ValUnsignedShort [Word16]
  | ValUnsignedLong [Word32]
  | ValUnsignedRational [URational]
  | ValSignedByte [Int8]
  | ValUndefined BS.ByteString
  | ValSignedShort [Int16]
  | ValSignedLong [Int32]
  | ValSignedRational [SRational]
  | ValSingleFloat [Float]
  | ValDoubleFloat [Double]
  | ValUnknown ExifFormat Word32 BS.ByteString
  deriving (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- Directory Entries

-- | On-wire 12-byte TIFF directory entry.
data RawDirEntry = RawDirEntry
  { rawEntryTag :: Word16,
    rawEntryFormat :: Word16,
    rawEntryCount :: Word32,
    rawEntryOffsetOrInline :: Word32
  }
  deriving (Eq, Show, Generic)

-- | An IFD directory entry holding its tag key, format, component count,
-- and byte payload.
data DirEntry = DirEntry
  { entryTagKey :: Word16,
    entryFormat :: ExifFormat,
    entryCount :: Word32,
    entryData :: BS.ByteString
  }
  deriving (Eq, Show, Generic)

-- ---------------------------------------------------------------------------
-- Image File Directory (IFD)

-- | An Image File Directory (IFD) containing a list of entries and
-- the offset pointing to the next IFD (0 if none).
data IFD = IFD
  { ifdEntries :: [DirEntry],
    ifdNextOffset :: Word32
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

-- | The Exif data structure (AST), reflecting the layout described in the Exif specification:
-- TIFF header endianness, primary IFD0 (main image), optional Exif SubIFD (camera settings),
-- optional GPS SubIFD, optional Interoperability SubIFD, optional IFD1 (thumbnail directory),
-- and optional thumbnail image.
data ExifData = ExifData
  { exifByteAlign :: ByteAlign,
    exifIFD0 :: IFD,
    exifSubIFD :: Maybe IFD,
    exifGpsSubIFD :: Maybe IFD,
    exifInteropSubIFD :: Maybe IFD,
    exifIFD1 :: Maybe IFD,
    exifThumbnail :: Maybe Thumbnail
  }
  deriving (Eq, Show, Generic)
