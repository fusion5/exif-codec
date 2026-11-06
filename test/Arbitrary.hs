module Arbitrary () where

import Codec.Exif.Types
import Control.Monad (forM, replicateM)
import Data.ByteString qualified as BS
import Data.Map qualified as M
import Test.QuickCheck

instance Arbitrary Exif where
  arbitrary = Exif <$> arbitrary

instance Arbitrary Tiff where
  arbitrary =
    oneof
      [ IntelOrder <$> arbitrary,
        MotorolaOrder <$> arbitrary
      ]

instance Arbitrary ByteOrder where
  arbitrary = elements [Intel, Motorola]

instance Arbitrary ExifData where
  arbitrary = sized $ \n -> do
    -- Generate a sensible chain length for IFDs (e.g., 1 to 3 IFDs)
    k <- choose (1, max 1 (n `min` 3))
    ifds <- scale (`div` 2) (replicateM k arbitrary)
    return $ ExifData ifds

pointerTags :: [Tag]
pointerTags = [exifOffsetTag, gpsInfoTag, interopOffsetTag]

instance Arbitrary IFD where
  arbitrary = sized $ \n -> do
    -- Generate a small number of entries per IFD
    numEntries <- choose (0, max 0 (n `min` 5))
    normalTags <- vectorOf numEntries (arbitrary `suchThat` (`notElem` pointerTags))
    normalVals <- replicateM numEntries arbitrary
    let normalEntries = M.fromList (zip normalTags normalVals)
    if n <= 0
      then return $ IFD normalEntries
      else do
        pointers <- sublistOf pointerTags
        pointerEntries <- forM pointers $ \ptag -> do
          subIfd <- scale (`div` 2) arbitrary
          return (ptag, IFDPointer subIfd)
        return $ IFD (M.union (M.fromList pointerEntries) normalEntries)

instance Arbitrary DirectoryEntryValue where
  arbitrary =
    oneof
      [ UnsignedByte <$> arbitrary,
        Ascii . BS.pack <$> arbitrary,
        UnsignedShort <$> arbitrary,
        UnsignedLong <$> arbitrary,
        UnsignedRational <$> arbitrary,
        SignedByte <$> arbitrary,
        Undefined . BS.pack <$> arbitrary,
        SignedShort <$> arbitrary,
        SignedLong <$> arbitrary,
        SignedRational <$> arbitrary,
        SingleFloat <$> listOf (arbitrary `suchThat` (not . isNaN)),
        DoubleFloat <$> listOf (arbitrary `suchThat` (not . isNaN)),
        UnsignedLong8 <$> arbitrary,
        SignedLong8 <$> arbitrary,
        genUnknown
      ]

genUnknown :: Gen DirectoryEntryValue
genUnknown = do
  typ <- arbitrary `suchThat` (\t -> t /= 0 && (t < 1 || t > 12) && t /= 16 && t /= 17)
  bs <- BS.pack <$> arbitrary
  let count = fromIntegral (BS.length bs)
  return $ Unknown typ count bs

instance Arbitrary URational where
  arbitrary = URational <$> arbitrary <*> arbitrary

instance Arbitrary SRational where
  arbitrary = SRational <$> arbitrary <*> arbitrary

instance Arbitrary Thumbnail where
  arbitrary =
    oneof
      [ JpegThumbnail . BS.pack <$> arbitrary,
        TiffThumbnail . BS.pack <$> arbitrary
      ]
