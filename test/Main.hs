module Main (main) where

import Codec.Exif qualified as Exif ()
import Prelude
import Test.Hspec (describe, hspec, it, pendingWith)

main :: IO ()
main = hspec $ do
  describe "decodeExif" $ do
    it "decodes EXIF metadata from raw TIFF payload" $ do
      pendingWith "implementation pending review"
