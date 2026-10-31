module Codec.Exif
  ( encodeExif
  , decodeExif
  , ParseError(..)
  , allTags
  , module Graphics.HsExif
  ) where

import Control.Monad (replicateM, unless)
import Data.Binary.Get
  ( Get
  , getByteString
  , getWord16be
  , getWord16le
  , getWord32be
  , getWord32le
  , getWord8
  , lookAhead
  , runGetOrFail
  , skip
  )
import Data.Binary.Put (Put, putWord32be, putWord32le, runPut)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Internal (w2c)
import Data.ByteString.Lazy qualified as LBS
import Data.Int (Int16, Int32, Int8)
import Data.List (find)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Word (Word16, Word32, Word8)
import Graphics.HsExif hiding (parseExif, parseFileExif)
import Prelude

newtype ParseError = ParseError String
  deriving (Show, Eq)

data ByteAlign = Intel | Motorola
  deriving (Eq)

getWord16 :: ByteAlign -> Get Word16
getWord16 Intel = getWord16le
getWord16 Motorola = getWord16be

getWord32 :: ByteAlign -> Get Word32
getWord32 Intel = getWord32le
getWord32 Motorola = getWord32be

putWord32 :: ByteAlign -> Word32 -> Put
putWord32 Intel = putWord32le
putWord32 Motorola = putWord32be

-- | Read EXIF data directly from an EXIF payload (without container image headers).
-- Handles both raw TIFF headers and payloads with an "Exif\0\0" prefix.
decodeExif :: BS.ByteString -> Either ParseError (Map ExifTag ExifValue)
decodeExif bs =
  let tiffBytes =
        if "Exif\NUL\NUL" `BS.isPrefixOf` bs
          then BS.drop 6 bs
          else bs
      lazyBytes = LBS.fromStrict tiffBytes
  in case runGetOrFail parseTiff lazyBytes of
       Left (_, _, err) -> Left (ParseError err)
       Right (_, _, tags) -> Right tags

encodeExif :: Map ExifTag ExifValue -> BS.ByteString
encodeExif = error "TODO: counterpart Put functions to write encodeExif"

parseTiff :: Get (Map ExifTag ExifValue)
parseTiff = do
  (byteAlign, ifdOffset) <- lookAhead parseTiffHeader
  tags <- parseIfd byteAlign IFD0 ifdOffset
  return $ Map.fromList tags

parseTiffHeader :: Get (ByteAlign, Int)
parseTiffHeader = do
  byteAlignV <- Char8.unpack <$> getByteString 2
  byteAlign <- case byteAlignV of
    "II" -> return Intel
    "MM" -> return Motorola
    _ -> fail $ "Unknown byte alignment: " ++ byteAlignV
  alignControl <- toInteger <$> getWord16 byteAlign
  unless (alignControl == 0x2a || (byteAlign == Intel && (alignControl == 0x55 || alignControl == 0x4f52)))
    $ fail "exif byte alignment mismatch"
  ifdOffset <- fromIntegral . toInteger <$> getWord32 byteAlign
  return (byteAlign, ifdOffset)

parseIfd :: ByteAlign -> TagLocation -> Int -> Get [(ExifTag, ExifValue)]
parseIfd byteAlign ifdId offset = do
  entries <- lookAhead $ do
    skip offset
    dirEntriesCount <- fromIntegral <$> getWord16 byteAlign
    replicateM dirEntriesCount (parseIfEntry byteAlign ifdId)
  concat <$> mapM (entryTags byteAlign) entries

data IfEntry = Tag ExifTag (Get ExifValue) | SubIFD TagLocation Int

entryTags :: ByteAlign -> IfEntry -> Get [(ExifTag, ExifValue)]
entryTags _ (Tag tag parseValue) = parseValue >>= \value -> pure [(tag, value)]
entryTags byteAlign (SubIFD ifdId offset) = lookAhead (parseIfd byteAlign ifdId offset)

parseIfEntry :: ByteAlign -> TagLocation -> Get IfEntry
parseIfEntry byteAlign ifdId = do
  tagNumber <- getWord16 byteAlign
  format <- getWord16 byteAlign
  numComponents <- fromIntegral <$> getWord32 byteAlign
  content <- getWord32 byteAlign
  return $ case (ifdId, tagNumber) of
    (IFD0, 0x8769) -> SubIFD ExifSubIFD (fromIntegral content)
    (IFD0, 0x8825) -> SubIFD GpsSubIFD (fromIntegral content)
    (_, tagId)     -> Tag (getExifTag ifdId tagId) (decodeEntry byteAlign format numComponents content)

getExifTag :: TagLocation -> Word16 -> ExifTag
getExifTag l v = fromMaybe (ExifTag l Nothing v (T.pack . show)) $ find (isSameTag l v) allTags
  where
    isSameTag l1 v1 (ExifTag l2 _ v2 _) = l1 == l2 && v1 == v2

data ValueHandler = ValueHandler
  { dataTypeId :: Word16
  , dataLength :: Int
  , readSingle :: ByteAlign -> Get ExifValue
  , readMany   :: ByteAlign -> Int -> Get ExifValue
  }

readNumberList :: (Integral a) => (ByteAlign -> Get a) -> ByteAlign -> Int -> Get ExifValue
readNumberList decoder byteAlign components =
  ExifNumberList . fmap fromIntegral <$> replicateM components (decoder byteAlign)

decodeTextByteString :: BS.ByteString -> String
decodeTextByteString bs = w2c <$> strippedWords
  where
    strippedWords =
      if not (null bsWords) && last bsWords == 0
        then init bsWords
        else bsWords
    bsWords = BS.unpack bs

unsignedByteValueHandler :: ValueHandler
unsignedByteValueHandler = ValueHandler
  { dataTypeId = 1
  , dataLength = 1
  , readSingle = \_ -> ExifNumber . fromIntegral <$> getWord8
  , readMany   = readNumberList $ const getWord8
  }

asciiStringValueHandler :: ValueHandler
asciiStringValueHandler = ValueHandler
  { dataTypeId = 2
  , dataLength = 1
  , readSingle = \ba -> readMany asciiStringValueHandler ba 1
  , readMany   = \_ components -> ExifText . decodeTextByteString <$> getByteString components
  }

unsignedShortValueHandler :: ValueHandler
unsignedShortValueHandler = ValueHandler
  { dataTypeId = 3
  , dataLength = 2
  , readSingle = fmap (ExifNumber . fromIntegral) . getWord16
  , readMany   = readNumberList getWord16
  }

unsignedLongValueHandler :: ValueHandler
unsignedLongValueHandler = ValueHandler
  { dataTypeId = 4
  , dataLength = 4
  , readSingle = fmap (ExifNumber . fromIntegral) . getWord32
  , readMany   = readNumberList getWord32
  }

readRationalContents :: (Int -> Int -> a) -> ByteAlign -> Get a
readRationalContents c byteAlign = do
  numerator   <- fromIntegral <$> getWord32 byteAlign
  denominator <- fromIntegral <$> getWord32 byteAlign
  return $ c numerator denominator

unsignedRationalValueHandler :: ValueHandler
unsignedRationalValueHandler = ValueHandler
  { dataTypeId = 5
  , dataLength = 8
  , readSingle = readRationalContents ExifRational
  , readMany   = \byteAlign components -> ExifRationalList <$> replicateM components (readRationalContents (,) byteAlign)
  }

signedByteValueHandler :: ValueHandler
signedByteValueHandler = ValueHandler
  { dataTypeId = 6
  , dataLength = 1
  , readSingle = \_ -> ExifNumber . signedInt8ToInt <$> getWord8
  , readMany   = readNumberList (fmap signedInt8ToInt . const getWord8)
  }

undefinedValueHandler :: ValueHandler
undefinedValueHandler = ValueHandler
  { dataTypeId = 7
  , dataLength = 1
  , readSingle = \ba -> readMany undefinedValueHandler ba 1
  , readMany   = \_ components -> ExifUndefined <$> getByteString components
  }

signedShortValueHandler :: ValueHandler
signedShortValueHandler = ValueHandler
  { dataTypeId = 8
  , dataLength = 2
  , readSingle = fmap (ExifNumber . signedInt16ToInt) . getWord16
  , readMany   = readNumberList (fmap signedInt16ToInt . getWord16)
  }

signedLongValueHandler :: ValueHandler
signedLongValueHandler = ValueHandler
  { dataTypeId = 9
  , dataLength = 4
  , readSingle = fmap (ExifNumber . signedInt32ToInt) . getWord32
  , readMany   = readNumberList (fmap signedInt32ToInt . getWord32)
  }

readSignedRationalContents :: (Int -> Int -> a) -> ByteAlign -> Get a
readSignedRationalContents c byteAlign = do
  numerator   <- signedInt32ToInt <$> getWord32 byteAlign
  denominator <- signedInt32ToInt <$> getWord32 byteAlign
  return $ c numerator denominator

signedRationalValueHandler :: ValueHandler
signedRationalValueHandler = ValueHandler
  { dataTypeId = 10
  , dataLength = 8
  , readSingle = readSignedRationalContents ExifRational
  , readMany   = \byteAlign components -> ExifRationalList <$>
      replicateM components (readSignedRationalContents (,) byteAlign)
  }

valueHandlers :: [ValueHandler]
valueHandlers =
  [ unsignedByteValueHandler
  , asciiStringValueHandler
  , unsignedShortValueHandler
  , unsignedLongValueHandler
  , unsignedRationalValueHandler
  , signedByteValueHandler
  , signedShortValueHandler
  , signedLongValueHandler
  , signedRationalValueHandler
  , undefinedValueHandler
  ]

decodeEntry :: ByteAlign -> Word16 -> Int -> Word32 -> Get ExifValue
decodeEntry byteAlign format amount payload =
  case getHandler format of
    Just handler | isInline handler -> return $ parseInline byteAlign handler amount (LBS.toStrict $ runPut $ putWord32 byteAlign payload)
    Just handler                    -> parseOffset byteAlign handler amount payload
    Nothing                         -> return $ ExifUnknown format amount (fromIntegral payload)
  where
    isInline handler = dataLength handler * amount <= 4

getHandler :: Word16 -> Maybe ValueHandler
getHandler typeId = find ((== typeId) . dataTypeId) valueHandlers

parseInline :: ByteAlign -> ValueHandler -> Int -> BS.ByteString -> ExifValue
parseInline byteAlign handler amount bs =
  let lazyBs = LBS.fromStrict bs
      getter = case amount of
        1 -> readSingle handler byteAlign
        _ -> readMany handler byteAlign amount
  in case runGetOrFail getter lazyBs of
       Right (_, _, val) -> val
       Left (_, _, err)  -> error ("Failed to parse inline EXIF entry: " ++ err)

parseOffset :: ByteAlign -> ValueHandler -> Int -> Word32 -> Get ExifValue
parseOffset byteAlign handler amount offset =
  lookAhead $ do
    skip (fromIntegral offset)
    let bsLength = amount * dataLength handler
    bs <- getByteString bsLength
    return (parseInline byteAlign handler amount bs)

signedInt32ToInt :: Word32 -> Int
signedInt32ToInt w = fromIntegral (fromIntegral w :: Int32)

signedInt16ToInt :: Word16 -> Int
signedInt16ToInt w = fromIntegral (fromIntegral w :: Int16)

signedInt8ToInt :: Word8 -> Int
signedInt8ToInt w = fromIntegral (fromIntegral w :: Int8)

allTags :: [ExifTag]
allTags =
  [ exposureTime, fnumber, exposureProgram, isoSpeedRatings
  , exifVersion, dateTimeOriginal, dateTimeDigitized, componentConfiguration
  , compressedBitsPerPixel, shutterSpeedValue, apertureValue, brightnessValue
  , exposureBiasValue, maxApertureValue, subjectDistance, meteringMode
  , lightSource, flash, focalLength, makerNote, userComment
  , exifImageWidth, exifImageHeight, relatedSoundFile, focalPlaneXResolution
  , focalPlaneYResolution, focalPlaneResolutionUnit, sensingMethod, fileSource
  , sceneType, orientation, make, model, software, copyright
  , spectralSensitivity, oecf, subjectArea, subSecTime, subSecTimeOriginal
  , subSecTimeDigitized, flashPixVersion, colorSpace, flashEnergy
  , spatialFrequencyResponse, subjectLocation, exposureIndex, cfaPattern
  , customRendered, exposureMode, whiteBalance, digitalZoomRatio
  , focalLengthIn35mmFilm, sceneCaptureType, gainControl, contrast
  , saturation, sharpness, deviceSettingDescription, subjectDistanceRange
  , imageUniqueId, exifInteroperabilityOffset, imageDescription
  , xResolution, yResolution, resolutionUnit, dateTime, whitePoint
  , primaryChromaticities, yCbCrPositioning, yCbCrCoefficients
  , referenceBlackWhite, printImageMatching, artist
  , gpsVersionID, gpsLatitudeRef, gpsLatitude, gpsLongitudeRef, gpsLongitude
  , gpsAltitudeRef, gpsAltitude, gpsTimeStamp, gpsSatellites, gpsStatus
  , gpsMeasureMode, gpsDop, gpsSpeedRef, gpsSpeed, gpsTrackRef, gpsTrack
  , gpsImgDirectionRef, gpsImgDirection, gpsMapDatum, gpsDestLatitudeRef
  , gpsDestLatitude, gpsDestLongitudeRef, gpsDestLongitude, gpsDestBearingRef
  , gpsDestBearing, gpsDestDistanceRef, gpsDestDistance, gpsProcessingMethod
  , gpsAreaInformation, gpsDateStamp, gpsDifferential
  ]
