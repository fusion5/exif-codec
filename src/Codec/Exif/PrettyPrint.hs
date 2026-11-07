module Codec.Exif.PrettyPrint (prettyPrint) where

import Codec.Exif.Patterns
import Codec.Exif.Types
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.Int
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Word
import Text.Printf

-- | Maps a Directory, Tag, and Value to a pretty-printed Text representation.
-- GPS
-- Interoperability
prettyPrint :: Directory -> Tag -> DirectoryEntryValue -> T.Text
prettyPrint Directory1 tag value = prettyPrint Directory0 tag value
prettyPrint Directory0 ImageProcessingSoftware (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageNewSubfileType (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageSubfileType (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageImageWidth (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageImageLength (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageBitsPerSample (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageCompression (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImagePhotometricInterpretation (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageThresholding (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageCellWidth (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageCellLength (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageFillOrder (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageDocumentName (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageImageDescription (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageMake (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageModel (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageStripOffsets (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageOrientation value = ppOrientation value
prettyPrint Directory0 ImageSamplesPerPixel (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageRowsPerStrip (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageStripByteCounts (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageXResolution (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageYResolution (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImagePlanarConfiguration (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImagePageName (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageXPosition (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageYPosition (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageGrayResponseUnit (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageGrayResponseCurve (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageT4Options (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageT6Options (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageResolutionUnit val@(UnsignedShort {}) = ppResolutionUnit val
prettyPrint Directory0 ImagePageNumber (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageTransferFunction (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageSoftware (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageDateTime (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageArtist (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageHostComputer (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImagePredictor (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageWhitePoint (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImagePrimaryChromaticities (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageColorMap (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageHalftoneHints (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageTileWidth (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageTileLength (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageTileOffsets (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageTileByteCounts (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageSubIFDs (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageInkSet (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageInkNames (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageNumberOfInks (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageDotRange (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageTargetPrinter (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageExtraSamples (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageSampleFormat (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageSMinSampleValue (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageSMaxSampleValue (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageTransferRange (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageClipPath (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageXClipPathUnits (SignedShort xs) = ppSignedShorts xs
prettyPrint Directory0 ImageYClipPathUnits (SignedShort xs) = ppSignedShorts xs
prettyPrint Directory0 ImageIndexed (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageJPEGTables (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageOPIProxy (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageJPEGProc (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageJPEGInterchangeFormat (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageJPEGInterchangeFormatLength (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageJPEGRestartInterval (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageJPEGLosslessPredictors (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageJPEGPointTransforms (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageJPEGQTables (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageJPEGDCTables (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageJPEGACTables (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageYCbCrCoefficients (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageYCbCrSubSampling (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageYCbCrPositioning xs = ppYCbCrPositioning xs
prettyPrint Directory0 ImageReferenceBlackWhite (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageXMLPacket (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageRating (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageRatingPercent (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageVignettingCorrParams (SignedShort xs) = ppSignedShorts xs
prettyPrint Directory0 ImageChromaticAberrationCorrParams (SignedShort xs) = ppSignedShorts xs
prettyPrint Directory0 ImageDistortionCorrParams (SignedShort xs) = ppSignedShorts xs
prettyPrint Directory0 ImageImageID (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageCFARepeatPatternDim (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageCFAPattern (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageBatteryLevel (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageCopyright (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageExposureTime (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageFNumber (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageIPTCNAA (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageImageResources (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageExifTag (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageInterColorProfile (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageExposureProgram (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageSpectralSensitivity (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageGPSTag (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageISOSpeedRatings (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageOECF (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageInterlace (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageTimeZoneOffset (SignedShort xs) = ppSignedShorts xs
prettyPrint Directory0 ImageSelfTimerMode (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageDateTimeOriginal (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageCompressedBitsPerPixel (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageShutterSpeedValue (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageApertureValue (UnsignedRational xs) = T.unwords $ map ppApertureUnsigned xs
prettyPrint Directory0 ImageApertureValue (SignedRational xs) = T.unwords $ map ppApertureSigned xs
prettyPrint Directory0 ImageBrightnessValue (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageExposureBiasValue (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageMaxApertureValue (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageSubjectDistance (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageMeteringMode (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageLightSource (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageFlash (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageFocalLength (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageFlashEnergy (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageSpatialFrequencyResponse (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageNoise (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageFocalPlaneXResolution (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageFocalPlaneYResolution (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageFocalPlaneResolutionUnit value = ppFocalPlaneResolutionUnit value
prettyPrint Directory0 ImageImageNumber (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageSecurityClassification (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageImageHistory (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageSubjectLocation (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageExposureIndex (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageTIFFEPStandardID (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageSensingMethod (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageXPTitle (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageXPComment (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageXPAuthor (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageXPKeywords (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageXPSubject (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImagePrintImageMatching (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageDNGVersion (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageDNGBackwardVersion (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageUniqueCameraModel (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageLocalizedCameraModel (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageCFAPlaneColor (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageCFALayout (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageLinearizationTable (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageBlackLevelRepeatDim (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageBlackLevel (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageBlackLevelDeltaH (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageBlackLevelDeltaV (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageWhiteLevel (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageDefaultScale (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageDefaultCropOrigin (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageDefaultCropSize (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageColorMatrix1 (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageColorMatrix2 (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageCameraCalibration1 (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageCameraCalibration2 (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageReductionMatrix1 (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageReductionMatrix2 (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageAnalogBalance (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageAsShotNeutral (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageAsShotWhiteXY (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageBaselineExposure (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageBaselineNoise (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageBaselineSharpness (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageBayerGreenSplit (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageLinearResponseLimit (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageCameraSerialNumber (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageLensInfo (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageChromaBlurRadius (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageAntiAliasStrength (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageShadowScale (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageDNGPrivateData (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageMakerNoteSafety (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageCalibrationIlluminant1 (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageCalibrationIlluminant2 (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageBestQualityScale (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageRawDataUniqueID (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageOriginalRawFileName (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageOriginalRawFileData (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageActiveArea (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageMaskedAreas (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageAsShotICCProfile (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageAsShotPreProfileMatrix (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageCurrentICCProfile (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageCurrentPreProfileMatrix (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageColorimetricReference (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageCameraCalibrationSignature (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageProfileCalibrationSignature (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageExtraCameraProfiles (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageAsShotProfileName (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageNoiseReductionApplied (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageProfileName (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageProfileHueSatMapDims (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageProfileHueSatMapData1 (SingleFloat xs) = ppSingleFloats xs
prettyPrint Directory0 ImageProfileHueSatMapData2 (SingleFloat xs) = ppSingleFloats xs
prettyPrint Directory0 ImageProfileToneCurve (SingleFloat xs) = ppSingleFloats xs
prettyPrint Directory0 ImageProfileEmbedPolicy (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageProfileCopyright (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageForwardMatrix1 (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageForwardMatrix2 (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImagePreviewApplicationName (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImagePreviewApplicationVersion (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImagePreviewSettingsName (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImagePreviewSettingsDigest (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImagePreviewColorSpace (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImagePreviewDateTime (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageRawImageDigest (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageOriginalRawFileDigest (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageSubTileBlockSize (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageRowInterleaveFactor (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageProfileLookTableDims (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageProfileLookTableData (SingleFloat xs) = ppSingleFloats xs
prettyPrint Directory0 ImageOpcodeList1 (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageOpcodeList2 (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageOpcodeList3 (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageNoiseProfile (DoubleFloat xs) = ppDoubleFloats xs
prettyPrint Directory0 ImageTimeCodes (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageFrameRate (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageTStop (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageReelName (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageCameraLabel (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageOriginalDefaultFinalSize (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageOriginalBestQualityFinalSize (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageOriginalDefaultCropSize (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageProfileHueSatMapEncoding (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageProfileLookTableEncoding (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageBaselineExposureOffset (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageDefaultBlackRender (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageNewRawImageDigest (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint Directory0 ImageRawToPreviewGain (DoubleFloat xs) = ppDoubleFloats xs
prettyPrint Directory0 ImageDefaultUserCrop (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageDepthFormat (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageDepthNear (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageDepthFar (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Directory0 ImageDepthUnits (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageDepthMeasureType (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageEnhanceParams (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageProfileGainTableMap (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageSemanticName (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageSemanticInstanceID (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageCalibrationIlluminant3 (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Directory0 ImageCameraCalibration3 (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageColorMatrix3 (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageForwardMatrix3 (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageIlluminantData1 (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageIlluminantData2 (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageIlluminantData3 (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageMaskSubArea (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageProfileHueSatMapData3 (SingleFloat xs) = ppSingleFloats xs
prettyPrint Directory0 ImageReductionMatrix3 (SignedRational xs) = ppSignedRationals xs
prettyPrint Directory0 ImageRGBTables (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageProfileGainTableMap2 (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageColumnInterleaveFactor (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageImageSequenceInfo (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageImageStats (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageProfileDynamicRange (Undefined bs) = ppUndefined bs
prettyPrint Directory0 ImageProfileGroupName (ASCII bs) = ppASCII bs
prettyPrint Directory0 ImageJXLDistance (SingleFloat xs) = ppSingleFloats xs
prettyPrint Directory0 ImageJXLEffort (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Directory0 ImageJXLDecodeSpeed (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Sub PhotoExposureTime (UnsignedRational (x : _)) = ppExposureTime x
prettyPrint Sub PhotoFNumber (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Sub PhotoExposureProgram value = ppExposureProgram value
prettyPrint Sub PhotoSpectralSensitivity (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoISOSpeedRatings (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Sub PhotoOECF (Undefined bs) = ppUndefined bs
prettyPrint Sub PhotoSensitivityType (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Sub PhotoStandardOutputSensitivity (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Sub PhotoRecommendedExposureIndex (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Sub PhotoISOSpeed (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Sub PhotoISOSpeedLatitudeyyy (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Sub PhotoISOSpeedLatitudezzz (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Sub PhotoExifVersion value = ppExifVersion value
prettyPrint Sub PhotoDateTimeOriginal (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoDateTimeDigitized (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoOffsetTime (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoOffsetTimeOriginal (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoOffsetTimeDigitized (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoComponentsConfiguration value = ppComponentConfiguration value
prettyPrint Sub PhotoCompressedBitsPerPixel (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Sub PhotoShutterSpeedValue (SignedRational xs) = ppSignedRationals xs
prettyPrint Sub PhotoApertureValue (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Sub PhotoBrightnessValue (SignedRational xs) = ppSignedRationals xs
prettyPrint Sub PhotoExposureBiasValue (SignedRational xs) = ppSignedRationals xs
prettyPrint Sub PhotoMaxApertureValue (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Sub PhotoSubjectDistance (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Sub PhotoMeteringMode value = ppMeteringMode value
prettyPrint Sub PhotoLightSource value = ppLightSource value
prettyPrint Sub PhotoFlash value = ppFlash value
prettyPrint Sub PhotoFocalLength (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Sub PhotoSubjectArea (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Sub PhotoMakerNote (Undefined bs) = ppUndefined bs
prettyPrint Sub PhotoUserComment val = ppUserComment val
prettyPrint Sub PhotoLearningOptOutIn (Undefined bs) = ppUndefined bs
prettyPrint Sub PhotoSubSecTime (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoSubSecTimeOriginal (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoSubSecTimeDigitized (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoTemperature (SignedRational xs) = ppSignedRationals xs
prettyPrint Sub PhotoHumidity (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Sub PhotoPressure (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Sub PhotoWaterDepth (SignedRational xs) = ppSignedRationals xs
prettyPrint Sub PhotoAcceleration (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Sub PhotoCameraElevationAngle (SignedRational xs) = ppSignedRationals xs
prettyPrint Sub PhotoFlashpixVersion value = ppFlashPixVersion value
prettyPrint Sub PhotoColorSpace value = ppColorSpace value
prettyPrint Sub PhotoPixelXDimension (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Sub PhotoPixelYDimension (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Sub PhotoRelatedSoundFile (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoInteroperabilityTag (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Sub PhotoFlashEnergy (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Sub PhotoSpatialFrequencyResponse (Undefined bs) = ppUndefined bs
prettyPrint Sub PhotoFocalPlaneXResolution (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Sub PhotoFocalPlaneYResolution (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Sub PhotoFocalPlaneResolutionUnit (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Sub PhotoSubjectLocation (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Sub PhotoExposureIndex (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Sub PhotoSensingMethod value = ppSensingMethod value
prettyPrint Sub PhotoFileSource val = ppFileSource val
prettyPrint Sub PhotoSceneType val = ppSceneType val
prettyPrint Sub PhotoCFAPattern (Undefined bs) = ppUndefined bs
prettyPrint Sub PhotoCustomRendered value = ppCustomRendered value
prettyPrint Sub PhotoExposureMode value = ppExposureMode value
prettyPrint Sub PhotoWhiteBalance value = ppWhiteBalance value
prettyPrint Sub PhotoDigitalZoomRatio (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Sub PhotoFocalLengthIn35mmFilm (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Sub PhotoSceneCaptureType value = ppSceneCaptureType value
prettyPrint Sub PhotoGainControl value = ppGainControl value
prettyPrint Sub PhotoContrast value = ppContrastSharpness value
prettyPrint Sub PhotoSaturation value = ppSaturation value
prettyPrint Sub PhotoSharpness (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Sub PhotoDeviceSettingDescription (Undefined bs) = ppUndefined bs
prettyPrint Sub PhotoSubjectDistanceRange value = ppSubjectDistanceRange value
prettyPrint Sub PhotoDevelopmentType (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Sub PhotoDevelopmentTypeDescription (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoDistortionCorrection (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Sub PhotoChromaticAberrationCorrection (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Sub PhotoShadingCorrection (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Sub PhotoNoiseReduction (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Sub PhotoImageUniqueID (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoCameraOwnerName (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoBodySerialNumber (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoLensSpecification (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Sub PhotoLensMake (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoLensModel (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoLensSerialNumber (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoImageTitle (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoPhotographer (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoImageEditor (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoCameraFirmware (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoRAWDevelopingSoftware (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoImageEditingSoftware (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoMetadataEditingSoftware (ASCII bs) = ppASCII bs
prettyPrint Sub PhotoCompositeImage (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Sub PhotoSourceImageNumberOfCompositeImage (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint Sub PhotoSourceExposureTimesOfCompositeImage (Undefined bs) = ppUndefined bs
prettyPrint Sub PhotoGamma (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint Interoperability IopInteroperabilityIndex (ASCII bs) = ppASCII bs
prettyPrint Interoperability IopInteroperabilityVersion (Undefined bs) = ppUndefined bs
prettyPrint Interoperability IopRelatedImageFileFormat (ASCII bs) = ppASCII bs
prettyPrint Interoperability IopRelatedImageWidth (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint Interoperability IopRelatedImageLength (UnsignedLong xs) = ppUnsignedLongs xs
prettyPrint GPS GPSInfoGPSVersionID (UnsignedByte xs) = ppUnsignedBytes xs
prettyPrint GPS GPSInfoGPSLatitudeRef (ASCII bs) = ppASCII bs
prettyPrint GPS GPSInfoGPSLatitude (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint GPS GPSInfoGPSLongitudeRef (ASCII bs) = ppASCII bs
prettyPrint GPS GPSInfoGPSLongitude (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint GPS GPSInfoGPSAltitudeRef value = ppGPSAltitudeRef value
prettyPrint GPS GPSInfoGPSAltitude (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint GPS GPSInfoGPSTimeStamp (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint GPS GPSInfoGPSSatellites (ASCII bs) = ppASCII bs
prettyPrint GPS GPSInfoGPSStatus (ASCII bs) = ppASCII bs
prettyPrint GPS GPSInfoGPSMeasureMode (ASCII bs) = ppASCII bs
prettyPrint GPS GPSInfoGPSDOP (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint GPS GPSInfoGPSSpeedRef (ASCII bs) = ppASCII bs
prettyPrint GPS GPSInfoGPSSpeed (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint GPS GPSInfoGPSTrackRef (ASCII bs) = ppASCII bs
prettyPrint GPS GPSInfoGPSTrack (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint GPS GPSInfoGPSImgDirectionRef (ASCII bs) = ppASCII bs
prettyPrint GPS GPSInfoGPSImgDirection (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint GPS GPSInfoGPSMapDatum (ASCII bs) = ppASCII bs
prettyPrint GPS GPSInfoGPSDestLatitudeRef (ASCII bs) = ppASCII bs
prettyPrint GPS GPSInfoGPSDestLatitude (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint GPS GPSInfoGPSDestLongitudeRef (ASCII bs) = ppASCII bs
prettyPrint GPS GPSInfoGPSDestLongitude (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint GPS GPSInfoGPSDestBearingRef (ASCII bs) = ppASCII bs
prettyPrint GPS GPSInfoGPSDestBearing (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint GPS GPSInfoGPSDestDistanceRef (ASCII bs) = ppASCII bs
prettyPrint GPS GPSInfoGPSDestDistance (UnsignedRational xs) = ppUnsignedRationals xs
prettyPrint GPS GPSInfoGPSProcessingMethod (Undefined bs) = ppUndefined bs
prettyPrint GPS GPSInfoGPSAreaInformation (Undefined bs) = ppUndefined bs
prettyPrint GPS GPSInfoGPSDateStamp (ASCII bs) = ppASCII bs
prettyPrint GPS GPSInfoGPSDifferential (UnsignedShort xs) = ppUnsignedShorts xs
prettyPrint GPS GPSInfoGPSHPositioningError (UnsignedRational xs) = ppUnsignedRationals xs
-- =====================================================================
-- SAFETY FALLBACK CATCH-ALL
-- =====================================================================
-- Handles cases where a camera wrote the correct Tag but used a wildly
-- incorrect EXIF Type (e.g., Short instead of Long) so it avoids crashing.
prettyPrint _dir _tag val = T.pack (show val)

ppASCII :: BS.ByteString -> T.Text
ppASCII bs = TE.decodeUtf8With TEE.lenientDecode $ BS.takeWhile (/= 0) bs

ppUnsignedBytes :: [Word8] -> T.Text
ppUnsignedBytes = T.unwords . Prelude.map ppUnsignedByte

ppUnsignedByte :: Word8 -> T.Text
ppUnsignedByte = T.pack . show

ppUnsignedShorts :: [Word16] -> T.Text
ppUnsignedShorts = T.unwords . Prelude.map ppUnsignedShort

ppUnsignedShort :: Word16 -> T.Text
ppUnsignedShort = T.pack . show

ppUnsignedLongs :: [Word32] -> T.Text
ppUnsignedLongs = T.unwords . Prelude.map ppUnsignedLong

ppUnsignedLong :: Word32 -> T.Text
ppUnsignedLong = T.pack . show

ppUnsignedRationals :: [URational] -> T.Text
ppUnsignedRationals xs = T.pack (show xs)

ppSignedShorts :: [Int16] -> T.Text
ppSignedShorts xs = T.pack (show xs)

ppSignedRationals :: [SRational] -> T.Text
ppSignedRationals xs = T.pack (show xs)

ppSingleFloats :: [Float] -> T.Text
ppSingleFloats xs = T.pack (show xs)

ppDoubleFloats :: [Double] -> T.Text
ppDoubleFloats xs = T.pack (show xs)

ppUndefined :: BS.ByteString -> T.Text
ppUndefined bs = "Binary data (" <> T.pack (show (BS.length bs)) <> " bytes)"

-- Copied from
-- https://hackage.haskell.org/package/hsexif-0.6.1.10/docs/src/Graphics.PrettyPrinters.html
-- Copyright (c) 2014, Emmanuel Touzery

ppResolutionUnit :: DirectoryEntryValue -> T.Text
ppResolutionUnit =
  fromNumberMap
    [ (1, "No absolute unit"),
      (2, "Inch"),
      (3, "Centimeter")
    ]

ppYCbCrPositioning :: DirectoryEntryValue -> T.Text
ppYCbCrPositioning = fromNumberMap [(1, "Centered"), (2, "Co-sited")]

-- ppAperture :: DirectoryEntryValue -> Text
-- ppAperture = T.pack . printf "f/%s" . formatAsFloatingPoint 1

-- ! Pretty-print aperture stored as APEX value.
-- See: https://photo.stackexchange.com/questions/19143/how-can-the-aperture-value-written-in-exif-be-larger-than-the-nominal-limit-of-t
ppApertureUnsigned :: URational -> Text
ppApertureUnsigned (URational n d) = T.pack $ printf "f/%.1f" fnumber
  where
    apex = fromIntegral @Word32 @Double n / fromIntegral d
    fnumber = 2 ** (apex / 2)

ppApertureSigned :: SRational -> Text
ppApertureSigned (SRational n d) = T.pack $ printf "f/%.1f" fnumber
  where
    apex = fromIntegral @Int32 @Double n / fromIntegral d
    fnumber = 2 ** (apex / 2)

-- ! Pretty print exposure time.
-- Formats times slower than 1/4 with one decimal place and
-- faster times as fractions.
-- Examples: "4 sec.", "1/125 sec.", "0.8 sec."
ppExposureTime :: URational -> Text
ppExposureTime (URational {..}) =
  let seconds = fromIntegral urNumerator / (fromIntegral urDenominator :: Double)
      value
        | seconds <= 0.25 && seconds > 0 = "1/" ++ show (round (1 / seconds) :: Int)
        | otherwise = show seconds
   in T.append (T.pack value) " sec."

ppExposureProgram :: DirectoryEntryValue -> Text
ppExposureProgram =
  fromNumberMap
    [ (0, "Not defined"),
      (1, "Manual"),
      (2, "Normal program"),
      (3, "Aperture priority"),
      (4, "Shutter priority"),
      (5, "Creative program (biased toward depth of field)"),
      (6, "Action program (biased toward fast shutter speed)"),
      (7, "Portrait mode (for closeup photos with the background out of focus)"),
      (8, "Landscape mode (for landscape photos with the background in focus)")
    ]

ppMeteringMode :: DirectoryEntryValue -> Text
ppMeteringMode =
  fromNumberMap
    [ (0, "Unknown"),
      (1, "Average"),
      (2, "Center-weighted average"),
      (3, "Spot"),
      (4, "MultiSpot"),
      (5, "Pattern"),
      (6, "Partial"),
      (255, "other")
    ]

ppLightSource :: DirectoryEntryValue -> Text
ppLightSource =
  fromNumberMap
    [ (0, "Unknown"),
      (1, "Daylight"),
      (2, "Fluorescent"),
      (3, "Tungsten (incandescent light)"),
      (4, "Flash"),
      (9, "Fine weather"),
      (10, "Cloudy weather"),
      (11, "Shade"),
      (12, "Daylight fluorescent (D 5700 - 7100K)"),
      (13, "Day white fluorescent (N 4600 - 5400K)"),
      (14, "Cool white fluorescent (W 3900 - 4500K)"),
      (15, "White fluorescent (WW 3200 - 3700K)"),
      (17, "Standard light A"),
      (18, "Standard light B"),
      (19, "Standard light C"),
      (20, "D55"),
      (21, "D65"),
      (22, "D75"),
      (23, "D50"),
      (24, "ISO studio tungsten"),
      (255, "Other light source")
    ]

ppFlash :: DirectoryEntryValue -> Text
ppFlash =
  fromNumberMap
    [ (0, "Flash did not fire"),
      (1, "Flash fired"),
      (5, "Strobe return light not detected"),
      (7, "Strobe return light detected"),
      (9, "Flash fired, compulsory flash mode"),
      (0x0D, "Flash fired, compulsory flash mode, return light not detected"),
      (0x0F, "Flash fired, compulsory flash mode, return light detected"),
      (0x10, "Flash did not fire, compulsory flash mode"),
      (0x18, "Flash did not fire, auto mode"),
      (0x19, "Flash fired, auto mode"),
      (0x1D, "Flash fired, auto mode, return light not detected"),
      (0x1F, "Flash fired, auto mode, return light detected"),
      (0x20, "No flash function"),
      (0x41, "Flash fired, red-eye reduction mode"),
      (0x45, "Flash fired, red-eye reduction mode, return light not detected"),
      (0x47, "Flash fired, red-eye reduction mode, return light detected"),
      (0x49, "Flash fired, compulsory flash mode, red-eye reduction mode"),
      (0x4D, "Flash fired, compulsory flash mode, red-eye reduction mode, return light not detected"),
      (0x4F, "Flash fired, compulsory flash mode, red-eye reduction mode, return light detected"),
      (0x59, "Flash fired, auto mode, red-eye reduction mode"),
      (0x5D, "Flash fired, auto mode, return light not detected, red-eye reduction mode"),
      (0x5F, "Flash fired, auto mode, return light detected, red-eye reduction mode")
    ]

ppColorSpace :: DirectoryEntryValue -> Text
ppColorSpace = fromNumberMap [(1, "sRGB"), (65535, "Uncalibrated")]

ppCustomRendered :: DirectoryEntryValue -> Text
ppCustomRendered = fromNumberMap [(0, "Normal process"), (1, "Custom process")]

ppExposureMode :: DirectoryEntryValue -> Text
ppExposureMode =
  fromNumberMap
    [ (0, "Auto exposure"),
      (1, "Manual exposure"),
      (2, "Auto bracket")
    ]

ppWhiteBalance :: DirectoryEntryValue -> Text
ppWhiteBalance =
  fromNumberMap
    [ (0, "Auto white balance"),
      (1, "Manual white balance")
    ]

ppSceneCaptureType :: DirectoryEntryValue -> Text
ppSceneCaptureType =
  fromNumberMap
    [ (0, "Standard"),
      (1, "Landscape"),
      (2, "Portrait"),
      (3, "Night scene")
    ]

ppGainControl :: DirectoryEntryValue -> Text
ppGainControl =
  fromNumberMap
    [ (0, "Normal"),
      (1, "Low gain up"),
      (2, "High gain up"),
      (3, "Low gain down"),
      (4, "High gain down")
    ]

ppContrastSharpness :: DirectoryEntryValue -> Text
ppContrastSharpness = fromNumberMap [(0, "Normal"), (1, "Soft"), (2, "Hard")]

ppSaturation :: DirectoryEntryValue -> Text
ppSaturation =
  fromNumberMap
    [ (0, "Normal"),
      (1, "Low saturation"),
      (2, "High saturation")
    ]

ppSensingMethod :: DirectoryEntryValue -> Text
ppSensingMethod =
  fromNumberMap
    [ (1, "Not defined"),
      (2, "One-chip color area sensor"),
      (3, "Two-chip color area sensor"),
      (4, "Three-chip color area sensor"),
      (5, "Color sequential area sensor"),
      (7, "Trilinear sensor"),
      (8, "Color sequential linear sensor")
    ]

ppSubjectDistanceRange :: DirectoryEntryValue -> Text
ppSubjectDistanceRange =
  fromNumberMap
    [ (0, "Unknown"),
      (1, "Macro"),
      (2, "Close view"),
      (3, "Distance view")
    ]

ppFocalPlaneResolutionUnit :: DirectoryEntryValue -> Text
ppFocalPlaneResolutionUnit =
  fromNumberMap
    [ (1, "No absolute unit of measurement"),
      (2, "Inch"),
      (3, "Centimeter")
    ]

ppOrientation :: DirectoryEntryValue -> Text
ppOrientation =
  fromNumberMap
    [ (1, "Top-left"),
      (2, "Top-right"),
      (3, "Bottom-right"),
      (4, "Bottom-left"),
      (5, "Left-top"),
      (6, "Right-top"),
      (7, "Right-bottom"),
      (8, "Left-bottom")
    ]

ppSceneType :: DirectoryEntryValue -> Text
ppSceneType = fromNumberMap [(1, "Directly photographed")]

ppGPSAltitudeRef :: DirectoryEntryValue -> Text
ppGPSAltitudeRef =
  fromNumberMap
    [ (0, "Sea level"),
      (1, "Below sea level")
    ]

componentMap :: Map Int Text
componentMap = M.fromList $ zip [0 ..] ["-", "Y", "Cb", "Cr", "R", "G", "B"]

ppComponentConfiguration :: DirectoryEntryValue -> Text
ppComponentConfiguration (Undefined bs) = T.concat $ Prelude.map formatComponent numbers
  where
    numbers = fromIntegral <$> BS.unpack bs
    formatComponent = fromMaybe "?" . flip M.lookup componentMap
ppComponentConfiguration v = unknown v

ppFlashPixVersion :: DirectoryEntryValue -> Text
ppFlashPixVersion = formatVersion "FlashPix version %.1f"

ppExifVersion :: DirectoryEntryValue -> Text
ppExifVersion = formatVersion "Exif version %.2f"

formatVersion :: String -> DirectoryEntryValue -> Text
formatVersion fmt (Undefined s) = T.pack $ printf fmt num
  where
    num :: Float = read asStr / 100.0
    asStr = T.unpack $ TE.decodeUtf8 s
formatVersion _ v = unknown v

ppUserComment :: DirectoryEntryValue -> Text
ppUserComment (Undefined v) = TE.decodeUtf8 $ BS.drop 8 v
ppUserComment v = unknown v

-- | Pretty printer for the FileSource tag
ppFileSource :: DirectoryEntryValue -> Text
ppFileSource (Undefined v)
  | BS.head v == 3 = "DSC"
  | otherwise = "(unknown)"
ppFileSource v = unknown v

fromNumberMap :: [(Int, Text)] -> DirectoryEntryValue -> Text
fromNumberMap m = fromMap convertedMap
  where
    convertedMap = M.unions convertedMaps
    convertedMaps =
      [ M.fromList $ fmap (first $ UnsignedByte . pure . fromIntegral) m,
        M.fromList $ fmap (first $ UnsignedShort . pure . fromIntegral) m,
        M.fromList $ fmap (first $ UnsignedLong . pure . fromIntegral) m,
        M.fromList $ fmap (first $ UnsignedLong8 . pure . fromIntegral) m,
        M.fromList $ fmap (first $ SignedByte . pure . fromIntegral) m,
        M.fromList $ fmap (first $ SignedShort . pure . fromIntegral) m,
        M.fromList $ fmap (first $ SignedLong . pure . fromIntegral) m,
        M.fromList $ fmap (first $ SignedLong8 . pure . fromIntegral) m
      ]

fromMap :: Map DirectoryEntryValue Text -> DirectoryEntryValue -> Text
fromMap m v = M.findWithDefault (unknown v) v m

unknown :: (Show a) => a -> Text
unknown v = T.pack $ "Unknown value: " ++ show v
