module Main (main) where

import Arbitrary ()
import Codec.Exif
import Control.Monad
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.Either
import Data.Map qualified as Map
import JPEGParser (extractExifBlob)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, (</>))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Prelude

helloWorldExif :: BS.ByteString
helloWorldExif =
  BS.pack
    [ -- 1. EXIF IDENTIFIER
      0x45,
      0x78,
      0x69,
      0x66,
      0x00,
      0x00, -- "Exif\0\0"

      -- ==============================================
      -- TIFF HEADER STARTS HERE (This is "Offset 0")
      -- ==============================================

      -- 2. Byte Order: "II" (Little Endian format)
      0x49,
      0x49,
      -- 3. TIFF Magic Number (Always 42)
      0x2A,
      0x00,
      -- 4. Offset to first IFD (Image File Directory) - 8 bytes from TIFF start
      0x08,
      0x00,
      0x00,
      0x00,
      -- ==============================================
      -- IFD0 (DIRECTORY) STARTS HERE (Offset 8)
      -- ==============================================

      -- 5. Number of directory entries (We only have 1)
      0x01,
      0x00,
      -- 6. Entry 1: ImageDescription Tag (0x010E in Little Endian)
      0x0E,
      0x01,
      --    Data Type: ASCII String (Type 2)
      0x02,
      0x00,
      --    Component Count: 12 bytes ("hello world" + null terminator)
      0x0C,
      0x00,
      0x00,
      0x00,
      --    Value Offset: The string lives 26 bytes from the TIFF start (0x1A)
      0x1A,
      0x00,
      0x00,
      0x00,
      -- 7. Offset to next IFD (0 means no more directories)
      0x00,
      0x00,
      0x00,
      0x00,
      -- ==============================================
      -- DATA BLOCK STARTS HERE (Offset 26)
      -- ==============================================

      -- 8. The raw ASCII string: "hello world\0"
      0x68,
      0x65,
      0x6C,
      0x6C,
      0x6F,
      0x20,
      0x77,
      0x6F,
      0x72,
      0x6C,
      0x64,
      0x00
    ]

findJpegs :: FilePath -> IO [FilePath]
findJpegs dir = do
  entries <- listDirectory dir
  let paths = map (dir </>) entries
  foldM categorize [] paths
  where
    categorize :: [FilePath] -> FilePath -> IO [FilePath]
    categorize acc path = do
      isDir <- doesDirectoryExist path
      if isDir
        then do
          subFiles <- findJpegs path
          return (acc <> subFiles)
        else do
          let ext = map toLower (takeExtension path)
          if ext `elem` [".jpg", ".jpeg"]
            then return (path : acc)
            else return acc

getSamplesDir :: IO FilePath
getSamplesDir = do
  mDir <- lookupEnv "EXIF_SAMPLES_DIR"
  case mDir of
    Just dir -> pure dir
    Nothing -> error "EXIF_SAMPLES_DIR environment variable is not set. Run via 'nix develop'."

main :: IO ()
main = do
  samples <- getSamplesDir
  jpegs <- findJpegs samples
  print jpegs
  hspec $ do
    describe "decodeExif" $ do
      it "decodes EXIF metadata from raw TIFF payload" $ do
        let result = decodeExif helloWorldExif
        result `shouldBe` Right (Map.singleton imageDescription (ExifText "hello world"))

    describe "QuickCheck Arbitrary Exif Map" $ do
      prop "exif encode decode roundtrip Intel" $ \(m :: Map.Map ExifTag ExifValue) ->
        decodeExif (encodeExif Intel m) `shouldBe` Right m
      prop "exif encode decode roundtrip Motorola" $ \(m :: Map.Map ExifTag ExifValue) ->
        decodeExif (encodeExif Motorola m) `shouldBe` Right m

    describe "Parse exif from sample files" $ do
      forM_ jpegs $ \sample ->
        it ("parse file " <> sample) $ do
          blob <- extractExifBlob sample
          case blob of
            Left e -> do
              print e
              pending
            Right exifBS -> decodeExif exifBS `shouldSatisfy` isRight
