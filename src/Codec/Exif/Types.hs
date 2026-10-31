module Codec.Exif.Types
  ( ByteAlign (..)
  ) where

import Prelude

-- | Byte alignment (endianness) of the TIFF/EXIF payload.
data ByteAlign = Intel   -- ^ Little-endian ("II")
               | Motorola -- ^ Big-endian ("MM")
  deriving (Eq, Show)
