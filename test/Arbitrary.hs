module Arbitrary () where

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
    -- Generate a sensible chain length for IFDs (e.g., 1 to 3 IFDs)
    k <- choose (1, max 1 (n `min` 3))
    ifds <- scale (`div` 2) (replicateM k arbitrary)
    return $ ExifData ifds

instance Arbitrary IFD where
  arbitrary = sized $ \n -> do
    -- Generate a small number of entries per IFD
    numEntries <- choose (0, max 1 (n `min` 10))
    tags <- replicateM numEntries arbitrary
    vals <- scale (`div` 2) (replicateM numEntries arbitrary)
    return $ IFD (M.fromList (zip tags vals))

instance Arbitrary DirectoryEntryValue where
  arbitrary = sized $ \n ->
    if n <= 0
      then nonRecursiveValue
      else
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
            SingleFloat <$> arbitrary,
            DoubleFloat <$> arbitrary,
            UnsignedLong8 <$> arbitrary,
            SignedLong8 <$> arbitrary,
            IFDPointer <$> scale (`div` 2) arbitrary,
            Unknown <$> arbitrary <*> arbitrary <*> (BS.pack <$> arbitrary)
          ]

nonRecursiveValue :: Gen DirectoryEntryValue
nonRecursiveValue =
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
      SingleFloat <$> arbitrary,
      DoubleFloat <$> arbitrary,
      UnsignedLong8 <$> arbitrary,
      SignedLong8 <$> arbitrary
    ]

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
