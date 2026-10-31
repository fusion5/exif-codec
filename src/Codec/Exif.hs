module Codec.Exif (encodeExif, decodeExif) where

import Prelude
import Graphics.HsExif
import Data.ByteString
import Data.Map

data ParseError

decodeExif :: ByteString -> Either ParseError (Map ExifTag ExifValue)
decodeExif = error "TODO"

encodeExif :: (Map ExifTag ExifValue) -> ByteString
encodeExif = error "TODO"
