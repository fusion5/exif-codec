module Codec.Exif
  ( decodeExif,
    decodeExifRaw,
    ParseError (..),
    Envelope (..),
    Header (..),
    allTags,
    Codec.Exif.Encode.encodeExif,
    getExifMap,
    module Exports,
    module Graphics.HsExif,
  )
where

import Codec.Exif.Decode (ParseError (..), allTags, decodeExif, decodeExifRaw)
import Codec.Exif.Encode (encodeExif)
import Codec.Exif.Types
import Codec.Exif.Types as Exports (Envelope (..), Header (..))
import Data.Map (Map)
import Graphics.HsExif hiding (parseExif, parseFileExif)

getExifMap :: Envelope -> Map ExifTag ExifValue
getExifMap (Envelope (IntelOrder exifMap)) = exifMap
getExifMap (Envelope (MotorolaOrder exifMap)) = exifMap
