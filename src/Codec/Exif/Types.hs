module Codec.Exif.Types
  ( ByteAlign (..),
    Header (..),
    Envelope (..),
    exifPrefix,
  )
where

import Data.ByteString qualified as BS
import Data.Map
import Graphics.HsExif
  ( ExifTag (..),
    ExifValue (..),
  )
import Prelude

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
  deriving (Eq, Show)

exifPrefix :: BS.ByteString
exifPrefix = "Exif\NUL\NUL"
