module Codec.Exif
  ( -- * Decoding
    decodeExif,
    decodeTiff,
    ParseError (..),

    -- * Encoding
    encodeExif,

    -- * Types
    Tag,
    Directory (..),
    DirectoryEntryValue (..),
    Exif (..),
    ByteOrder (..),
    alterDirectoryTag,
    emptyExif,
    getDirectoryTag,
    getDirectoryTags,
  )
where

import Codec.Exif.Decode (ParseError (..), decodeExif, decodeTiff)
import Codec.Exif.Encode (encodeExif)
import Codec.Exif.Types
  ( ByteOrder (..),
    Directory (..),
    DirectoryEntryValue (..),
    Exif (..),
    Tag,
    alterDirectoryTag,
    emptyExif,
    getDirectoryTag,
    getDirectoryTags,
  )
