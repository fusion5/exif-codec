module Main (main) where

import Codec.Exif qualified as Exif
import Data.ByteString qualified as BS
import Prelude

main :: IO ()
main = do
  case Exif.decodeExif BS.empty of
    Left _ -> putStrLn "Test suite stub for decodeExif: initialized."
    Right _ -> putStrLn "Test suite stub for decodeExif: initialized."
