{-# LANGUAGE PatternSynonyms #-}

module Codec.Exif.Patterns (module Codec.Exif.Patterns) where

import Codec.Exif.Types

-- IFD0
-- "IFD1 tags are not listed separately. All IFD0 tags may also be present in IFD1, according to
-- the standard" - https://exiv2.org/tags.html

pattern ImageProcessingSoftware :: Tag
pattern ImageProcessingSoftware = Tag 0x000b

pattern ImageNewSubfileType :: Tag
pattern ImageNewSubfileType = Tag 0x00fe

pattern ImageSubfileType :: Tag
pattern ImageSubfileType = Tag 0x00ff

pattern ImageImageWidth :: Tag
pattern ImageImageWidth = Tag 0x0100

pattern ImageImageLength :: Tag
pattern ImageImageLength = Tag 0x0101

pattern ImageBitsPerSample :: Tag
pattern ImageBitsPerSample = Tag 0x0102

pattern ImageCompression :: Tag
pattern ImageCompression = Tag 0x0103

pattern ImagePhotometricInterpretation :: Tag
pattern ImagePhotometricInterpretation = Tag 0x0106

pattern ImageThresholding :: Tag
pattern ImageThresholding = Tag 0x0107

pattern ImageCellWidth :: Tag
pattern ImageCellWidth = Tag 0x0108

pattern ImageCellLength :: Tag
pattern ImageCellLength = Tag 0x0109

pattern ImageFillOrder :: Tag
pattern ImageFillOrder = Tag 0x010a

pattern ImageDocumentName :: Tag
pattern ImageDocumentName = Tag 0x010d

pattern ImageImageDescription :: Tag
pattern ImageImageDescription = Tag 0x010e

pattern ImageMake :: Tag
pattern ImageMake = Tag 0x010f

pattern ImageModel :: Tag
pattern ImageModel = Tag 0x0110

pattern ImageStripOffsets :: Tag
pattern ImageStripOffsets = Tag 0x0111

pattern ImageOrientation :: Tag
pattern ImageOrientation = Tag 0x0112

pattern ImageSamplesPerPixel :: Tag
pattern ImageSamplesPerPixel = Tag 0x0115

pattern ImageRowsPerStrip :: Tag
pattern ImageRowsPerStrip = Tag 0x0116

pattern ImageStripByteCounts :: Tag
pattern ImageStripByteCounts = Tag 0x0117

pattern ImageXResolution :: Tag
pattern ImageXResolution = Tag 0x011a

pattern ImageYResolution :: Tag
pattern ImageYResolution = Tag 0x011b

pattern ImagePlanarConfiguration :: Tag
pattern ImagePlanarConfiguration = Tag 0x011c

pattern ImagePageName :: Tag
pattern ImagePageName = Tag 0x011d

pattern ImageXPosition :: Tag
pattern ImageXPosition = Tag 0x011e

pattern ImageYPosition :: Tag
pattern ImageYPosition = Tag 0x011f

pattern ImageGrayResponseUnit :: Tag
pattern ImageGrayResponseUnit = Tag 0x0122

pattern ImageGrayResponseCurve :: Tag
pattern ImageGrayResponseCurve = Tag 0x0123

pattern ImageT4Options :: Tag
pattern ImageT4Options = Tag 0x0124

pattern ImageT6Options :: Tag
pattern ImageT6Options = Tag 0x0125

pattern ImageResolutionUnit :: Tag
pattern ImageResolutionUnit = Tag 0x0128

pattern ImagePageNumber :: Tag
pattern ImagePageNumber = Tag 0x0129

pattern ImageTransferFunction :: Tag
pattern ImageTransferFunction = Tag 0x012d

pattern ImageSoftware :: Tag
pattern ImageSoftware = Tag 0x0131

pattern ImageDateTime :: Tag
pattern ImageDateTime = Tag 0x0132

pattern ImageArtist :: Tag
pattern ImageArtist = Tag 0x013b

pattern ImageHostComputer :: Tag
pattern ImageHostComputer = Tag 0x013c

pattern ImagePredictor :: Tag
pattern ImagePredictor = Tag 0x013d

pattern ImageWhitePoint :: Tag
pattern ImageWhitePoint = Tag 0x013e

pattern ImagePrimaryChromaticities :: Tag
pattern ImagePrimaryChromaticities = Tag 0x013f

pattern ImageColorMap :: Tag
pattern ImageColorMap = Tag 0x0140

pattern ImageHalftoneHints :: Tag
pattern ImageHalftoneHints = Tag 0x0141

pattern ImageTileWidth :: Tag
pattern ImageTileWidth = Tag 0x0142

pattern ImageTileLength :: Tag
pattern ImageTileLength = Tag 0x0143

pattern ImageTileOffsets :: Tag
pattern ImageTileOffsets = Tag 0x0144

pattern ImageTileByteCounts :: Tag
pattern ImageTileByteCounts = Tag 0x0145

pattern ImageSubIFDs :: Tag
pattern ImageSubIFDs = Tag 0x014a

pattern ImageInkSet :: Tag
pattern ImageInkSet = Tag 0x014c

pattern ImageInkNames :: Tag
pattern ImageInkNames = Tag 0x014d

pattern ImageNumberOfInks :: Tag
pattern ImageNumberOfInks = Tag 0x014e

pattern ImageDotRange :: Tag
pattern ImageDotRange = Tag 0x0150

pattern ImageTargetPrinter :: Tag
pattern ImageTargetPrinter = Tag 0x0151

pattern ImageExtraSamples :: Tag
pattern ImageExtraSamples = Tag 0x0152

pattern ImageSampleFormat :: Tag
pattern ImageSampleFormat = Tag 0x0153

pattern ImageSMinSampleValue :: Tag
pattern ImageSMinSampleValue = Tag 0x0154

pattern ImageSMaxSampleValue :: Tag
pattern ImageSMaxSampleValue = Tag 0x0155

pattern ImageTransferRange :: Tag
pattern ImageTransferRange = Tag 0x0156

pattern ImageClipPath :: Tag
pattern ImageClipPath = Tag 0x0157

pattern ImageXClipPathUnits :: Tag
pattern ImageXClipPathUnits = Tag 0x0158

pattern ImageYClipPathUnits :: Tag
pattern ImageYClipPathUnits = Tag 0x0159

pattern ImageIndexed :: Tag
pattern ImageIndexed = Tag 0x015a

pattern ImageJPEGTables :: Tag
pattern ImageJPEGTables = Tag 0x015b

pattern ImageOPIProxy :: Tag
pattern ImageOPIProxy = Tag 0x015f

pattern ImageJPEGProc :: Tag
pattern ImageJPEGProc = Tag 0x0200

pattern ImageJPEGInterchangeFormat :: Tag
pattern ImageJPEGInterchangeFormat = Tag 0x0201

pattern ImageJPEGInterchangeFormatLength :: Tag
pattern ImageJPEGInterchangeFormatLength = Tag 0x0202

pattern ImageJPEGRestartInterval :: Tag
pattern ImageJPEGRestartInterval = Tag 0x0203

pattern ImageJPEGLosslessPredictors :: Tag
pattern ImageJPEGLosslessPredictors = Tag 0x0205

pattern ImageJPEGPointTransforms :: Tag
pattern ImageJPEGPointTransforms = Tag 0x0206

pattern ImageJPEGQTables :: Tag
pattern ImageJPEGQTables = Tag 0x0207

pattern ImageJPEGDCTables :: Tag
pattern ImageJPEGDCTables = Tag 0x0208

pattern ImageJPEGACTables :: Tag
pattern ImageJPEGACTables = Tag 0x0209

pattern ImageYCbCrCoefficients :: Tag
pattern ImageYCbCrCoefficients = Tag 0x0211

pattern ImageYCbCrSubSampling :: Tag
pattern ImageYCbCrSubSampling = Tag 0x0212

pattern ImageYCbCrPositioning :: Tag
pattern ImageYCbCrPositioning = Tag 0x0213

pattern ImageReferenceBlackWhite :: Tag
pattern ImageReferenceBlackWhite = Tag 0x0214

pattern ImageXMLPacket :: Tag
pattern ImageXMLPacket = Tag 0x02bc

pattern ImageRating :: Tag
pattern ImageRating = Tag 0x4746

pattern ImageRatingPercent :: Tag
pattern ImageRatingPercent = Tag 0x4749

pattern ImageVignettingCorrParams :: Tag
pattern ImageVignettingCorrParams = Tag 0x7032

pattern ImageChromaticAberrationCorrParams :: Tag
pattern ImageChromaticAberrationCorrParams = Tag 0x7035

pattern ImageDistortionCorrParams :: Tag
pattern ImageDistortionCorrParams = Tag 0x7037

pattern ImageImageID :: Tag
pattern ImageImageID = Tag 0x800d

pattern ImageCFARepeatPatternDim :: Tag
pattern ImageCFARepeatPatternDim = Tag 0x828d

pattern ImageCFAPattern :: Tag
pattern ImageCFAPattern = Tag 0x828e

pattern ImageBatteryLevel :: Tag
pattern ImageBatteryLevel = Tag 0x828f

pattern ImageCopyright :: Tag
pattern ImageCopyright = Tag 0x8298

pattern ImageExposureTime :: Tag
pattern ImageExposureTime = Tag 0x829a

pattern ImageFNumber :: Tag
pattern ImageFNumber = Tag 0x829d

pattern ImageIPTCNAA :: Tag
pattern ImageIPTCNAA = Tag 0x83bb

pattern ImageImageResources :: Tag
pattern ImageImageResources = Tag 0x8649

pattern ImageExifTag :: Tag
pattern ImageExifTag = Tag 0x8769

pattern ImageInterColorProfile :: Tag
pattern ImageInterColorProfile = Tag 0x8773

pattern ImageExposureProgram :: Tag
pattern ImageExposureProgram = Tag 0x8822

pattern ImageSpectralSensitivity :: Tag
pattern ImageSpectralSensitivity = Tag 0x8824

pattern ImageGPSTag :: Tag
pattern ImageGPSTag = Tag 0x8825

pattern ImageISOSpeedRatings :: Tag
pattern ImageISOSpeedRatings = Tag 0x8827

pattern ImageOECF :: Tag
pattern ImageOECF = Tag 0x8828

pattern ImageInterlace :: Tag
pattern ImageInterlace = Tag 0x8829

pattern ImageTimeZoneOffset :: Tag
pattern ImageTimeZoneOffset = Tag 0x882a

pattern ImageSelfTimerMode :: Tag
pattern ImageSelfTimerMode = Tag 0x882b

pattern ImageDateTimeOriginal :: Tag
pattern ImageDateTimeOriginal = Tag 0x9003

pattern ImageCompressedBitsPerPixel :: Tag
pattern ImageCompressedBitsPerPixel = Tag 0x9102

pattern ImageShutterSpeedValue :: Tag
pattern ImageShutterSpeedValue = Tag 0x9201

pattern ImageApertureValue :: Tag
pattern ImageApertureValue = Tag 0x9202

pattern ImageBrightnessValue :: Tag
pattern ImageBrightnessValue = Tag 0x9203

pattern ImageExposureBiasValue :: Tag
pattern ImageExposureBiasValue = Tag 0x9204

pattern ImageMaxApertureValue :: Tag
pattern ImageMaxApertureValue = Tag 0x9205

pattern ImageSubjectDistance :: Tag
pattern ImageSubjectDistance = Tag 0x9206

pattern ImageMeteringMode :: Tag
pattern ImageMeteringMode = Tag 0x9207

pattern ImageLightSource :: Tag
pattern ImageLightSource = Tag 0x9208

pattern ImageFlash :: Tag
pattern ImageFlash = Tag 0x9209

pattern ImageFocalLength :: Tag
pattern ImageFocalLength = Tag 0x920a

pattern ImageFlashEnergy :: Tag
pattern ImageFlashEnergy = Tag 0x920b

pattern ImageSpatialFrequencyResponse :: Tag
pattern ImageSpatialFrequencyResponse = Tag 0x920c

pattern ImageNoise :: Tag
pattern ImageNoise = Tag 0x920d

pattern ImageFocalPlaneXResolution :: Tag
pattern ImageFocalPlaneXResolution = Tag 0x920e

pattern ImageFocalPlaneYResolution :: Tag
pattern ImageFocalPlaneYResolution = Tag 0x920f

pattern ImageFocalPlaneResolutionUnit :: Tag
pattern ImageFocalPlaneResolutionUnit = Tag 0x9210

pattern ImageImageNumber :: Tag
pattern ImageImageNumber = Tag 0x9211

pattern ImageSecurityClassification :: Tag
pattern ImageSecurityClassification = Tag 0x9212

pattern ImageImageHistory :: Tag
pattern ImageImageHistory = Tag 0x9213

pattern ImageSubjectLocation :: Tag
pattern ImageSubjectLocation = Tag 0x9214

pattern ImageExposureIndex :: Tag
pattern ImageExposureIndex = Tag 0x9215

pattern ImageTIFFEPStandardID :: Tag
pattern ImageTIFFEPStandardID = Tag 0x9216

pattern ImageSensingMethod :: Tag
pattern ImageSensingMethod = Tag 0x9217

pattern ImageXPTitle :: Tag
pattern ImageXPTitle = Tag 0x9c9b

pattern ImageXPComment :: Tag
pattern ImageXPComment = Tag 0x9c9c

pattern ImageXPAuthor :: Tag
pattern ImageXPAuthor = Tag 0x9c9d

pattern ImageXPKeywords :: Tag
pattern ImageXPKeywords = Tag 0x9c9e

pattern ImageXPSubject :: Tag
pattern ImageXPSubject = Tag 0x9c9f

pattern ImagePrintImageMatching :: Tag
pattern ImagePrintImageMatching = Tag 0xc4a5

pattern ImageDNGVersion :: Tag
pattern ImageDNGVersion = Tag 0xc612

pattern ImageDNGBackwardVersion :: Tag
pattern ImageDNGBackwardVersion = Tag 0xc613

pattern ImageUniqueCameraModel :: Tag
pattern ImageUniqueCameraModel = Tag 0xc614

pattern ImageLocalizedCameraModel :: Tag
pattern ImageLocalizedCameraModel = Tag 0xc615

pattern ImageCFAPlaneColor :: Tag
pattern ImageCFAPlaneColor = Tag 0xc616

pattern ImageCFALayout :: Tag
pattern ImageCFALayout = Tag 0xc617

pattern ImageLinearizationTable :: Tag
pattern ImageLinearizationTable = Tag 0xc618

pattern ImageBlackLevelRepeatDim :: Tag
pattern ImageBlackLevelRepeatDim = Tag 0xc619

pattern ImageBlackLevel :: Tag
pattern ImageBlackLevel = Tag 0xc61a

pattern ImageBlackLevelDeltaH :: Tag
pattern ImageBlackLevelDeltaH = Tag 0xc61b

pattern ImageBlackLevelDeltaV :: Tag
pattern ImageBlackLevelDeltaV = Tag 0xc61c

pattern ImageWhiteLevel :: Tag
pattern ImageWhiteLevel = Tag 0xc61d

pattern ImageDefaultScale :: Tag
pattern ImageDefaultScale = Tag 0xc61e

pattern ImageDefaultCropOrigin :: Tag
pattern ImageDefaultCropOrigin = Tag 0xc61f

pattern ImageDefaultCropSize :: Tag
pattern ImageDefaultCropSize = Tag 0xc620

pattern ImageColorMatrix1 :: Tag
pattern ImageColorMatrix1 = Tag 0xc621

pattern ImageColorMatrix2 :: Tag
pattern ImageColorMatrix2 = Tag 0xc622

pattern ImageCameraCalibration1 :: Tag
pattern ImageCameraCalibration1 = Tag 0xc623

pattern ImageCameraCalibration2 :: Tag
pattern ImageCameraCalibration2 = Tag 0xc624

pattern ImageReductionMatrix1 :: Tag
pattern ImageReductionMatrix1 = Tag 0xc625

pattern ImageReductionMatrix2 :: Tag
pattern ImageReductionMatrix2 = Tag 0xc626

pattern ImageAnalogBalance :: Tag
pattern ImageAnalogBalance = Tag 0xc627

pattern ImageAsShotNeutral :: Tag
pattern ImageAsShotNeutral = Tag 0xc628

pattern ImageAsShotWhiteXY :: Tag
pattern ImageAsShotWhiteXY = Tag 0xc629

pattern ImageBaselineExposure :: Tag
pattern ImageBaselineExposure = Tag 0xc62a

pattern ImageBaselineNoise :: Tag
pattern ImageBaselineNoise = Tag 0xc62b

pattern ImageBaselineSharpness :: Tag
pattern ImageBaselineSharpness = Tag 0xc62c

pattern ImageBayerGreenSplit :: Tag
pattern ImageBayerGreenSplit = Tag 0xc62d

pattern ImageLinearResponseLimit :: Tag
pattern ImageLinearResponseLimit = Tag 0xc62e

pattern ImageCameraSerialNumber :: Tag
pattern ImageCameraSerialNumber = Tag 0xc62f

pattern ImageLensInfo :: Tag
pattern ImageLensInfo = Tag 0xc630

pattern ImageChromaBlurRadius :: Tag
pattern ImageChromaBlurRadius = Tag 0xc631

pattern ImageAntiAliasStrength :: Tag
pattern ImageAntiAliasStrength = Tag 0xc632

pattern ImageShadowScale :: Tag
pattern ImageShadowScale = Tag 0xc633

pattern ImageDNGPrivateData :: Tag
pattern ImageDNGPrivateData = Tag 0xc634

pattern ImageMakerNoteSafety :: Tag
pattern ImageMakerNoteSafety = Tag 0xc635

pattern ImageCalibrationIlluminant1 :: Tag
pattern ImageCalibrationIlluminant1 = Tag 0xc65a

pattern ImageCalibrationIlluminant2 :: Tag
pattern ImageCalibrationIlluminant2 = Tag 0xc65b

pattern ImageBestQualityScale :: Tag
pattern ImageBestQualityScale = Tag 0xc65c

pattern ImageRawDataUniqueID :: Tag
pattern ImageRawDataUniqueID = Tag 0xc65d

pattern ImageOriginalRawFileName :: Tag
pattern ImageOriginalRawFileName = Tag 0xc68b

pattern ImageOriginalRawFileData :: Tag
pattern ImageOriginalRawFileData = Tag 0xc68c

pattern ImageActiveArea :: Tag
pattern ImageActiveArea = Tag 0xc68d

pattern ImageMaskedAreas :: Tag
pattern ImageMaskedAreas = Tag 0xc68e

pattern ImageAsShotICCProfile :: Tag
pattern ImageAsShotICCProfile = Tag 0xc68f

pattern ImageAsShotPreProfileMatrix :: Tag
pattern ImageAsShotPreProfileMatrix = Tag 0xc690

pattern ImageCurrentICCProfile :: Tag
pattern ImageCurrentICCProfile = Tag 0xc691

pattern ImageCurrentPreProfileMatrix :: Tag
pattern ImageCurrentPreProfileMatrix = Tag 0xc692

pattern ImageColorimetricReference :: Tag
pattern ImageColorimetricReference = Tag 0xc6bf

pattern ImageCameraCalibrationSignature :: Tag
pattern ImageCameraCalibrationSignature = Tag 0xc6f3

pattern ImageProfileCalibrationSignature :: Tag
pattern ImageProfileCalibrationSignature = Tag 0xc6f4

pattern ImageExtraCameraProfiles :: Tag
pattern ImageExtraCameraProfiles = Tag 0xc6f5

pattern ImageAsShotProfileName :: Tag
pattern ImageAsShotProfileName = Tag 0xc6f6

pattern ImageNoiseReductionApplied :: Tag
pattern ImageNoiseReductionApplied = Tag 0xc6f7

pattern ImageProfileName :: Tag
pattern ImageProfileName = Tag 0xc6f8

pattern ImageProfileHueSatMapDims :: Tag
pattern ImageProfileHueSatMapDims = Tag 0xc6f9

pattern ImageProfileHueSatMapData1 :: Tag
pattern ImageProfileHueSatMapData1 = Tag 0xc6fa

pattern ImageProfileHueSatMapData2 :: Tag
pattern ImageProfileHueSatMapData2 = Tag 0xc6fb

pattern ImageProfileToneCurve :: Tag
pattern ImageProfileToneCurve = Tag 0xc6fc

pattern ImageProfileEmbedPolicy :: Tag
pattern ImageProfileEmbedPolicy = Tag 0xc6fd

pattern ImageProfileCopyright :: Tag
pattern ImageProfileCopyright = Tag 0xc6fe

pattern ImageForwardMatrix1 :: Tag
pattern ImageForwardMatrix1 = Tag 0xc714

pattern ImageForwardMatrix2 :: Tag
pattern ImageForwardMatrix2 = Tag 0xc715

pattern ImagePreviewApplicationName :: Tag
pattern ImagePreviewApplicationName = Tag 0xc716

pattern ImagePreviewApplicationVersion :: Tag
pattern ImagePreviewApplicationVersion = Tag 0xc717

pattern ImagePreviewSettingsName :: Tag
pattern ImagePreviewSettingsName = Tag 0xc718

pattern ImagePreviewSettingsDigest :: Tag
pattern ImagePreviewSettingsDigest = Tag 0xc719

pattern ImagePreviewColorSpace :: Tag
pattern ImagePreviewColorSpace = Tag 0xc71a

pattern ImagePreviewDateTime :: Tag
pattern ImagePreviewDateTime = Tag 0xc71b

pattern ImageRawImageDigest :: Tag
pattern ImageRawImageDigest = Tag 0xc71c

pattern ImageOriginalRawFileDigest :: Tag
pattern ImageOriginalRawFileDigest = Tag 0xc71d

pattern ImageSubTileBlockSize :: Tag
pattern ImageSubTileBlockSize = Tag 0xc71e

pattern ImageRowInterleaveFactor :: Tag
pattern ImageRowInterleaveFactor = Tag 0xc71f

pattern ImageProfileLookTableDims :: Tag
pattern ImageProfileLookTableDims = Tag 0xc725

pattern ImageProfileLookTableData :: Tag
pattern ImageProfileLookTableData = Tag 0xc726

pattern ImageOpcodeList1 :: Tag
pattern ImageOpcodeList1 = Tag 0xc740

pattern ImageOpcodeList2 :: Tag
pattern ImageOpcodeList2 = Tag 0xc741

pattern ImageOpcodeList3 :: Tag
pattern ImageOpcodeList3 = Tag 0xc74e

pattern ImageNoiseProfile :: Tag
pattern ImageNoiseProfile = Tag 0xc761

pattern ImageTimeCodes :: Tag
pattern ImageTimeCodes = Tag 0xc763

pattern ImageFrameRate :: Tag
pattern ImageFrameRate = Tag 0xc764

pattern ImageTStop :: Tag
pattern ImageTStop = Tag 0xc772

pattern ImageReelName :: Tag
pattern ImageReelName = Tag 0xc789

pattern ImageCameraLabel :: Tag
pattern ImageCameraLabel = Tag 0xc7a1

pattern ImageOriginalDefaultFinalSize :: Tag
pattern ImageOriginalDefaultFinalSize = Tag 0xc791

pattern ImageOriginalBestQualityFinalSize :: Tag
pattern ImageOriginalBestQualityFinalSize = Tag 0xc792

pattern ImageOriginalDefaultCropSize :: Tag
pattern ImageOriginalDefaultCropSize = Tag 0xc793

pattern ImageProfileHueSatMapEncoding :: Tag
pattern ImageProfileHueSatMapEncoding = Tag 0xc7a3

pattern ImageProfileLookTableEncoding :: Tag
pattern ImageProfileLookTableEncoding = Tag 0xc7a4

pattern ImageBaselineExposureOffset :: Tag
pattern ImageBaselineExposureOffset = Tag 0xc7a5

pattern ImageDefaultBlackRender :: Tag
pattern ImageDefaultBlackRender = Tag 0xc7a6

pattern ImageNewRawImageDigest :: Tag
pattern ImageNewRawImageDigest = Tag 0xc7a7

pattern ImageRawToPreviewGain :: Tag
pattern ImageRawToPreviewGain = Tag 0xc7a8

pattern ImageDefaultUserCrop :: Tag
pattern ImageDefaultUserCrop = Tag 0xc7b5

pattern ImageDepthFormat :: Tag
pattern ImageDepthFormat = Tag 0xc7e9

pattern ImageDepthNear :: Tag
pattern ImageDepthNear = Tag 0xc7ea

pattern ImageDepthFar :: Tag
pattern ImageDepthFar = Tag 0xc7eb

pattern ImageDepthUnits :: Tag
pattern ImageDepthUnits = Tag 0xc7ec

pattern ImageDepthMeasureType :: Tag
pattern ImageDepthMeasureType = Tag 0xc7ed

pattern ImageEnhanceParams :: Tag
pattern ImageEnhanceParams = Tag 0xc7ee

pattern ImageProfileGainTableMap :: Tag
pattern ImageProfileGainTableMap = Tag 0xcd2d

pattern ImageSemanticName :: Tag
pattern ImageSemanticName = Tag 0xcd2e

pattern ImageSemanticInstanceID :: Tag
pattern ImageSemanticInstanceID = Tag 0xcd30

pattern ImageCalibrationIlluminant3 :: Tag
pattern ImageCalibrationIlluminant3 = Tag 0xcd31

pattern ImageCameraCalibration3 :: Tag
pattern ImageCameraCalibration3 = Tag 0xcd32

pattern ImageColorMatrix3 :: Tag
pattern ImageColorMatrix3 = Tag 0xcd33

pattern ImageForwardMatrix3 :: Tag
pattern ImageForwardMatrix3 = Tag 0xcd34

pattern ImageIlluminantData1 :: Tag
pattern ImageIlluminantData1 = Tag 0xcd35

pattern ImageIlluminantData2 :: Tag
pattern ImageIlluminantData2 = Tag 0xcd36

pattern ImageIlluminantData3 :: Tag
pattern ImageIlluminantData3 = Tag 0xcd37

pattern ImageMaskSubArea :: Tag
pattern ImageMaskSubArea = Tag 0xcd38

pattern ImageProfileHueSatMapData3 :: Tag
pattern ImageProfileHueSatMapData3 = Tag 0xcd39

pattern ImageReductionMatrix3 :: Tag
pattern ImageReductionMatrix3 = Tag 0xcd3a

pattern ImageRGBTables :: Tag
pattern ImageRGBTables = Tag 0xcd3f

pattern ImageProfileGainTableMap2 :: Tag
pattern ImageProfileGainTableMap2 = Tag 0xcd40

pattern ImageColumnInterleaveFactor :: Tag
pattern ImageColumnInterleaveFactor = Tag 0xcd43

pattern ImageImageSequenceInfo :: Tag
pattern ImageImageSequenceInfo = Tag 0xcd44

pattern ImageImageStats :: Tag
pattern ImageImageStats = Tag 0xcd46

pattern ImageProfileDynamicRange :: Tag
pattern ImageProfileDynamicRange = Tag 0xcd47

pattern ImageProfileGroupName :: Tag
pattern ImageProfileGroupName = Tag 0xcd48

pattern ImageJXLDistance :: Tag
pattern ImageJXLDistance = Tag 0xcd49

pattern ImageJXLEffort :: Tag
pattern ImageJXLEffort = Tag 0xcd4a

pattern ImageJXLDecodeSpeed :: Tag
pattern ImageJXLDecodeSpeed = Tag 0xcd4b

-- SubIFD

pattern PhotoExposureTime :: Tag
pattern PhotoExposureTime = Tag 0x829a

pattern PhotoFNumber :: Tag
pattern PhotoFNumber = Tag 0x829d

pattern PhotoExposureProgram :: Tag
pattern PhotoExposureProgram = Tag 0x8822

pattern PhotoSpectralSensitivity :: Tag
pattern PhotoSpectralSensitivity = Tag 0x8824

pattern PhotoISOSpeedRatings :: Tag
pattern PhotoISOSpeedRatings = Tag 0x8827

pattern PhotoOECF :: Tag
pattern PhotoOECF = Tag 0x8828

pattern PhotoSensitivityType :: Tag
pattern PhotoSensitivityType = Tag 0x8830

pattern PhotoStandardOutputSensitivity :: Tag
pattern PhotoStandardOutputSensitivity = Tag 0x8831

pattern PhotoRecommendedExposureIndex :: Tag
pattern PhotoRecommendedExposureIndex = Tag 0x8832

pattern PhotoISOSpeed :: Tag
pattern PhotoISOSpeed = Tag 0x8833

pattern PhotoISOSpeedLatitudeyyy :: Tag
pattern PhotoISOSpeedLatitudeyyy = Tag 0x8834

pattern PhotoISOSpeedLatitudezzz :: Tag
pattern PhotoISOSpeedLatitudezzz = Tag 0x8835

pattern PhotoExifVersion :: Tag
pattern PhotoExifVersion = Tag 0x9000

pattern PhotoDateTimeOriginal :: Tag
pattern PhotoDateTimeOriginal = Tag 0x9003

pattern PhotoDateTimeDigitized :: Tag
pattern PhotoDateTimeDigitized = Tag 0x9004

pattern PhotoOffsetTime :: Tag
pattern PhotoOffsetTime = Tag 0x9010

pattern PhotoOffsetTimeOriginal :: Tag
pattern PhotoOffsetTimeOriginal = Tag 0x9011

pattern PhotoOffsetTimeDigitized :: Tag
pattern PhotoOffsetTimeDigitized = Tag 0x9012

pattern PhotoComponentsConfiguration :: Tag
pattern PhotoComponentsConfiguration = Tag 0x9101

pattern PhotoCompressedBitsPerPixel :: Tag
pattern PhotoCompressedBitsPerPixel = Tag 0x9102

pattern PhotoShutterSpeedValue :: Tag
pattern PhotoShutterSpeedValue = Tag 0x9201

pattern PhotoApertureValue :: Tag
pattern PhotoApertureValue = Tag 0x9202

pattern PhotoBrightnessValue :: Tag
pattern PhotoBrightnessValue = Tag 0x9203

pattern PhotoExposureBiasValue :: Tag
pattern PhotoExposureBiasValue = Tag 0x9204

pattern PhotoMaxApertureValue :: Tag
pattern PhotoMaxApertureValue = Tag 0x9205

pattern PhotoSubjectDistance :: Tag
pattern PhotoSubjectDistance = Tag 0x9206

pattern PhotoMeteringMode :: Tag
pattern PhotoMeteringMode = Tag 0x9207

pattern PhotoLightSource :: Tag
pattern PhotoLightSource = Tag 0x9208

pattern PhotoFlash :: Tag
pattern PhotoFlash = Tag 0x9209

pattern PhotoFocalLength :: Tag
pattern PhotoFocalLength = Tag 0x920a

pattern PhotoSubjectArea :: Tag
pattern PhotoSubjectArea = Tag 0x9214

pattern PhotoMakerNote :: Tag
pattern PhotoMakerNote = Tag 0x927c

pattern PhotoUserComment :: Tag
pattern PhotoUserComment = Tag 0x9286

pattern PhotoLearningOptOutIn :: Tag
pattern PhotoLearningOptOutIn = Tag 0x9287

pattern PhotoSubSecTime :: Tag
pattern PhotoSubSecTime = Tag 0x9290

pattern PhotoSubSecTimeOriginal :: Tag
pattern PhotoSubSecTimeOriginal = Tag 0x9291

pattern PhotoSubSecTimeDigitized :: Tag
pattern PhotoSubSecTimeDigitized = Tag 0x9292

pattern PhotoTemperature :: Tag
pattern PhotoTemperature = Tag 0x9400

pattern PhotoHumidity :: Tag
pattern PhotoHumidity = Tag 0x9401

pattern PhotoPressure :: Tag
pattern PhotoPressure = Tag 0x9402

pattern PhotoWaterDepth :: Tag
pattern PhotoWaterDepth = Tag 0x9403

pattern PhotoAcceleration :: Tag
pattern PhotoAcceleration = Tag 0x9404

pattern PhotoCameraElevationAngle :: Tag
pattern PhotoCameraElevationAngle = Tag 0x9405

pattern PhotoFlashpixVersion :: Tag
pattern PhotoFlashpixVersion = Tag 0xa000

pattern PhotoColorSpace :: Tag
pattern PhotoColorSpace = Tag 0xa001

pattern PhotoPixelXDimension :: Tag
pattern PhotoPixelXDimension = Tag 0xa002

pattern PhotoPixelYDimension :: Tag
pattern PhotoPixelYDimension = Tag 0xa003

pattern PhotoRelatedSoundFile :: Tag
pattern PhotoRelatedSoundFile = Tag 0xa004

pattern PhotoInteroperabilityTag :: Tag
pattern PhotoInteroperabilityTag = Tag 0xa005

pattern PhotoFlashEnergy :: Tag
pattern PhotoFlashEnergy = Tag 0xa20b

pattern PhotoSpatialFrequencyResponse :: Tag
pattern PhotoSpatialFrequencyResponse = Tag 0xa20c

pattern PhotoFocalPlaneXResolution :: Tag
pattern PhotoFocalPlaneXResolution = Tag 0xa20e

pattern PhotoFocalPlaneYResolution :: Tag
pattern PhotoFocalPlaneYResolution = Tag 0xa20f

pattern PhotoFocalPlaneResolutionUnit :: Tag
pattern PhotoFocalPlaneResolutionUnit = Tag 0xa210

pattern PhotoSubjectLocation :: Tag
pattern PhotoSubjectLocation = Tag 0xa214

pattern PhotoExposureIndex :: Tag
pattern PhotoExposureIndex = Tag 0xa215

pattern PhotoSensingMethod :: Tag
pattern PhotoSensingMethod = Tag 0xa217

pattern PhotoFileSource :: Tag
pattern PhotoFileSource = Tag 0xa300

pattern PhotoSceneType :: Tag
pattern PhotoSceneType = Tag 0xa301

pattern PhotoCFAPattern :: Tag
pattern PhotoCFAPattern = Tag 0xa302

pattern PhotoCustomRendered :: Tag
pattern PhotoCustomRendered = Tag 0xa401

pattern PhotoExposureMode :: Tag
pattern PhotoExposureMode = Tag 0xa402

pattern PhotoWhiteBalance :: Tag
pattern PhotoWhiteBalance = Tag 0xa403

pattern PhotoDigitalZoomRatio :: Tag
pattern PhotoDigitalZoomRatio = Tag 0xa404

pattern PhotoFocalLengthIn35mmFilm :: Tag
pattern PhotoFocalLengthIn35mmFilm = Tag 0xa405

pattern PhotoSceneCaptureType :: Tag
pattern PhotoSceneCaptureType = Tag 0xa406

pattern PhotoGainControl :: Tag
pattern PhotoGainControl = Tag 0xa407

pattern PhotoContrast :: Tag
pattern PhotoContrast = Tag 0xa408

pattern PhotoSaturation :: Tag
pattern PhotoSaturation = Tag 0xa409

pattern PhotoSharpness :: Tag
pattern PhotoSharpness = Tag 0xa40a

pattern PhotoDeviceSettingDescription :: Tag
pattern PhotoDeviceSettingDescription = Tag 0xa40b

pattern PhotoSubjectDistanceRange :: Tag
pattern PhotoSubjectDistanceRange = Tag 0xa40c

pattern PhotoDevelopmentType :: Tag
pattern PhotoDevelopmentType = Tag 0xa40d

pattern PhotoDevelopmentTypeDescription :: Tag
pattern PhotoDevelopmentTypeDescription = Tag 0xa40e

pattern PhotoDistortionCorrection :: Tag
pattern PhotoDistortionCorrection = Tag 0xa40f

pattern PhotoChromaticAberrationCorrection :: Tag
pattern PhotoChromaticAberrationCorrection = Tag 0xa410

pattern PhotoShadingCorrection :: Tag
pattern PhotoShadingCorrection = Tag 0xa411

pattern PhotoNoiseReduction :: Tag
pattern PhotoNoiseReduction = Tag 0xa412

pattern PhotoImageUniqueID :: Tag
pattern PhotoImageUniqueID = Tag 0xa420

pattern PhotoCameraOwnerName :: Tag
pattern PhotoCameraOwnerName = Tag 0xa430

pattern PhotoBodySerialNumber :: Tag
pattern PhotoBodySerialNumber = Tag 0xa431

pattern PhotoLensSpecification :: Tag
pattern PhotoLensSpecification = Tag 0xa432

pattern PhotoLensMake :: Tag
pattern PhotoLensMake = Tag 0xa433

pattern PhotoLensModel :: Tag
pattern PhotoLensModel = Tag 0xa434

pattern PhotoLensSerialNumber :: Tag
pattern PhotoLensSerialNumber = Tag 0xa435

pattern PhotoImageTitle :: Tag
pattern PhotoImageTitle = Tag 0xa436

pattern PhotoPhotographer :: Tag
pattern PhotoPhotographer = Tag 0xa437

pattern PhotoImageEditor :: Tag
pattern PhotoImageEditor = Tag 0xa438

pattern PhotoCameraFirmware :: Tag
pattern PhotoCameraFirmware = Tag 0xa439

pattern PhotoRAWDevelopingSoftware :: Tag
pattern PhotoRAWDevelopingSoftware = Tag 0xa43a

pattern PhotoImageEditingSoftware :: Tag
pattern PhotoImageEditingSoftware = Tag 0xa43b

pattern PhotoMetadataEditingSoftware :: Tag
pattern PhotoMetadataEditingSoftware = Tag 0xa43c

pattern PhotoCompositeImage :: Tag
pattern PhotoCompositeImage = Tag 0xa460

pattern PhotoSourceImageNumberOfCompositeImage :: Tag
pattern PhotoSourceImageNumberOfCompositeImage = Tag 0xa461

pattern PhotoSourceExposureTimesOfCompositeImage :: Tag
pattern PhotoSourceExposureTimesOfCompositeImage = Tag 0xa462

pattern PhotoGamma :: Tag
pattern PhotoGamma = Tag 0xa500

-- Interoperability IFD

pattern IopInteroperabilityIndex :: Tag
pattern IopInteroperabilityIndex = Tag 0x0001

pattern IopInteroperabilityVersion :: Tag
pattern IopInteroperabilityVersion = Tag 0x0002

pattern IopRelatedImageFileFormat :: Tag
pattern IopRelatedImageFileFormat = Tag 0x1000

pattern IopRelatedImageWidth :: Tag
pattern IopRelatedImageWidth = Tag 0x1001

pattern IopRelatedImageLength :: Tag
pattern IopRelatedImageLength = Tag 0x1002

-- GPS IFD

pattern GPSInfoGPSVersionID :: Tag
pattern GPSInfoGPSVersionID = Tag 0x0000

pattern GPSInfoGPSLatitudeRef :: Tag
pattern GPSInfoGPSLatitudeRef = Tag 0x0001

pattern GPSInfoGPSLatitude :: Tag
pattern GPSInfoGPSLatitude = Tag 0x0002

pattern GPSInfoGPSLongitudeRef :: Tag
pattern GPSInfoGPSLongitudeRef = Tag 0x0003

pattern GPSInfoGPSLongitude :: Tag
pattern GPSInfoGPSLongitude = Tag 0x0004

pattern GPSInfoGPSAltitudeRef :: Tag
pattern GPSInfoGPSAltitudeRef = Tag 0x0005

pattern GPSInfoGPSAltitude :: Tag
pattern GPSInfoGPSAltitude = Tag 0x0006

pattern GPSInfoGPSTimeStamp :: Tag
pattern GPSInfoGPSTimeStamp = Tag 0x0007

pattern GPSInfoGPSSatellites :: Tag
pattern GPSInfoGPSSatellites = Tag 0x0008

pattern GPSInfoGPSStatus :: Tag
pattern GPSInfoGPSStatus = Tag 0x0009

pattern GPSInfoGPSMeasureMode :: Tag
pattern GPSInfoGPSMeasureMode = Tag 0x000a

pattern GPSInfoGPSDOP :: Tag
pattern GPSInfoGPSDOP = Tag 0x000b

pattern GPSInfoGPSSpeedRef :: Tag
pattern GPSInfoGPSSpeedRef = Tag 0x000c

pattern GPSInfoGPSSpeed :: Tag
pattern GPSInfoGPSSpeed = Tag 0x000d

pattern GPSInfoGPSTrackRef :: Tag
pattern GPSInfoGPSTrackRef = Tag 0x000e

pattern GPSInfoGPSTrack :: Tag
pattern GPSInfoGPSTrack = Tag 0x000f

pattern GPSInfoGPSImgDirectionRef :: Tag
pattern GPSInfoGPSImgDirectionRef = Tag 0x0010

pattern GPSInfoGPSImgDirection :: Tag
pattern GPSInfoGPSImgDirection = Tag 0x0011

pattern GPSInfoGPSMapDatum :: Tag
pattern GPSInfoGPSMapDatum = Tag 0x0012

pattern GPSInfoGPSDestLatitudeRef :: Tag
pattern GPSInfoGPSDestLatitudeRef = Tag 0x0013

pattern GPSInfoGPSDestLatitude :: Tag
pattern GPSInfoGPSDestLatitude = Tag 0x0014

pattern GPSInfoGPSDestLongitudeRef :: Tag
pattern GPSInfoGPSDestLongitudeRef = Tag 0x0015

pattern GPSInfoGPSDestLongitude :: Tag
pattern GPSInfoGPSDestLongitude = Tag 0x0016

pattern GPSInfoGPSDestBearingRef :: Tag
pattern GPSInfoGPSDestBearingRef = Tag 0x0017

pattern GPSInfoGPSDestBearing :: Tag
pattern GPSInfoGPSDestBearing = Tag 0x0018

pattern GPSInfoGPSDestDistanceRef :: Tag
pattern GPSInfoGPSDestDistanceRef = Tag 0x0019

pattern GPSInfoGPSDestDistance :: Tag
pattern GPSInfoGPSDestDistance = Tag 0x001a

pattern GPSInfoGPSProcessingMethod :: Tag
pattern GPSInfoGPSProcessingMethod = Tag 0x001b

pattern GPSInfoGPSAreaInformation :: Tag
pattern GPSInfoGPSAreaInformation = Tag 0x001c

pattern GPSInfoGPSDateStamp :: Tag
pattern GPSInfoGPSDateStamp = Tag 0x001d

pattern GPSInfoGPSDifferential :: Tag
pattern GPSInfoGPSDifferential = Tag 0x001e

pattern GPSInfoGPSHPositioningError :: Tag
pattern GPSInfoGPSHPositioningError = Tag 0x001f
