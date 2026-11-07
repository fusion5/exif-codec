module Main (main) where

import Arbitrary ()
import Codec.Exif
import Control.Monad
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.Either
import Data.Map (traverseWithKey)
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

findEXIFs :: FilePath -> IO [FilePath]
findEXIFs dir = do
  entries <- listDirectory dir
  let paths = map (dir </>) entries
  foldM categorize [] paths
  where
    categorize :: [FilePath] -> FilePath -> IO [FilePath]
    categorize acc path = do
      isDir <- doesDirectoryExist path
      if isDir
        then do
          subFiles <- findEXIFs path
          pure $ acc <> subFiles
        else do
          case map toLower (takeExtension path) of
            ".exif" -> pure $ path : acc
            _ -> pure acc

getSamplesDir :: IO (Maybe FilePath)
getSamplesDir = lookupEnv "EXIF_SAMPLES_DIR"

main :: IO ()
main = do
  samples <- getSamplesDir
  maybeExifs <- mapM findEXIFs samples
  hspec $ do
    describe "decodeExif" $ do
      it "decodes EXIF metadata from raw payload" $ do
        case decodeExif helloWorldExif of
          Left err ->
            expectationFailure $ "Failed to decode EXIF: " <> show err
          Right exif -> do
            getDirectoryTag exif Directory0 270 `shouldBe` Just (ASCII "hello world\NUL")
      it "alter EXIF metadata" $ do
        let exif1 = emptyExif Motorola
            exif2 = alterDirectoryTag exif1 Interoperability 270 (Just (ASCII "HI\NUL"))
            exif3 = alterDirectoryTag exif2 Interoperability 270 Nothing
        exif3 `shouldBe` exif1
      it "pretty printing works" $ do
        case decodeExif helloWorldExif of
          Left err ->
            expectationFailure $ "Failed to decode EXIF: " <> show err
          Right exif -> do
            forM_ [Directory0, Directory1, Sub, GPS, Interoperability] $ \dir -> do
              let tags = getDirectoryTags exif dir
              flip traverseWithKey tags $ \tag value -> do
                prettyPrintTag dir tag `shouldBe` "ImageImageDescription"
                prettyPrint dir tag value `shouldBe` "hello world"

    describe "QuickCheck Arbitrary Exif Map" $ do
      prop "exif encode decode roundtrip Intel" $ \exif -> do
        decodeExif (encodeExif exif) `shouldBe` Right exif

    describe "Parse EXIF from sample files with roundtrip" $ do
      forM_ maybeExifs $ \exifs ->
        forM_ exifs $ \sample ->
          it ("parse file " <> sample) $ do
            tiffBS <- BS.readFile sample
            case decodeTiff tiffBS of
              Left err ->
                expectationFailure $ "Failed to decode original camera EXIF: " <> show err
              Right tiff -> do
                decodeExif (encodeExif $ Exif tiff) `shouldBe` Right (Exif tiff)
