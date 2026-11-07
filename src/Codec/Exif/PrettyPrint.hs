module Codec.Exif.PrettyPrint (prettyPrint) where

import Codec.Exif.Types
import Data.Text
import Codec.Exif.Patterns ()

prettyPrint :: Directory -> Tag -> DirectoryEntryValue -> Text
prettyPrint = error "TODO"
