module Arbitrary () where

import Codec.Exif.Patterns
import Codec.Exif.Types
import Control.Monad (replicateM)
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
    ifd0 <- arbitrary
    numChained <- choose (0, max 0 (n `min` 2))
    chained <- scale (`div` 2) (replicateM numChained arbitrary)
    return $ ExifData ifd0 chained

instance Arbitrary Tag where
  arbitrary = mkTag <$> arbitrary

instance Arbitrary IFD0 where
  arbitrary = sized $ \n -> do
    numEntries <- choose (0, min 5 n)
    tags <-
      vectorOf
        numEntries
        (arbitrary `suchThat` (`notElem` [exifSubIFDOffsetTag, ImageGPSTag]))
    vals <- replicateM numEntries arbitrary
    let entries = M.fromList (zip tags vals)
    if n <= 0
      then return $ IFD0 entries Nothing Nothing
      else IFD0 entries <$> arbitrary <*> arbitrary

instance Arbitrary SubIFD where
  arbitrary = sized $ \n -> do
    numEntries <- choose (0, min 5 n)
    tags <- vectorOf numEntries (arbitrary `suchThat` (/= PhotoInteroperabilityTag))
    vals <- replicateM numEntries arbitrary
    let entries = M.fromList (zip tags vals)
    if n <= 0
      then return $ SubIFD entries Nothing
      else SubIFD entries <$> arbitrary

instance Arbitrary IFD where
  arbitrary = sized $ \n -> do
    numEntries <- choose (0, min 5 n)
    tags <- replicateM numEntries arbitrary
    vals <- replicateM numEntries arbitrary
    return $ IFD (M.fromList (zip tags vals))

instance Arbitrary DirectoryEntryValue where
  arbitrary =
    oneof
      [ UnsignedByte <$> arbitrary,
        ASCII . BS.pack <$> arbitrary,
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
