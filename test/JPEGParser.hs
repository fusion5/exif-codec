module JPEGParser (extractExifBlob, ParseError (..)) where

import Data.Binary.Get
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

data ParseError
  = ParseError LBS.ByteString ByteOffset String
  | NoExifDataFound ByteOffset
  deriving (Show)

extractExifBlob :: FilePath -> IO (Either ParseError BS.ByteString)
extractExifBlob path = do
  lazyBytes <- LBS.readFile path
  case runGetOrFail getExif lazyBytes of
    Right (_, _, Just exifData) -> pure $ Right exifData
    Right (_, byteOffset, Nothing) -> pure $ Left $ NoExifDataFound byteOffset
    Left (byteString, byteOffset, errorMessage) ->
      pure $ Left $ ParseError byteString byteOffset errorMessage

getExif :: Get (Maybe BS.ByteString)
getExif = do
  soi <- getWord16be
  if soi /= 0xFFD8 -- 0xFFD8 is the Start Of Image (SOI) marker
    then pure Nothing
    else parseSegments

parseSegments :: Get (Maybe BS.ByteString)
parseSegments = do
  empty <- isEmpty
  if empty
    then pure Nothing
    else do
      marker <- getWord16be

      -- 0xFFDA is Start Of Scan (SOS) - image data starts here, no more metadata
      if marker == 0xFFDA
        then pure Nothing
        -- Check if it's the APP1 marker (where EXIF lives)
        else
          if marker == 0xFFE1
            then do
              -- The length includes the 2 bytes of the length indicator itself
              segLen <- getWord16be
              payload <- getByteString (fromIntegral segLen - 2)

              -- Verify it's actually an EXIF APP1 block and not XMP or something else
              if BS.isPrefixOf "Exif\0\0" payload
                then pure (Just payload)
                else parseSegments

            -- Standalone markers with no length payload (e.g. RST markers)
            else
              if marker >= 0xFFD0 && marker <= 0xFFD9
                then parseSegments
                -- Any other marker: read its length and skip its payload
                else do
                  segLen <- getWord16be
                  skip (fromIntegral segLen - 2)
                  parseSegments
