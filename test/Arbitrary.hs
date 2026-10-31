module Arbitrary
  ( genTagLocation,
    genExifTag,
    genExifValue,
  )
where

import Codec.Exif
  ( ExifTag (..),
    ExifValue (..),
    TagLocation (..),
    allTags,
  )
import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.Text qualified as T
import Data.Word (Word32)
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    NonNegative (..),
    Positive (..),
    elements,
    frequency,
    listOf,
    oneof,
    shrink,
    suchThat,
  )
import Prelude

genTagLocation :: Gen TagLocation
genTagLocation = elements [IFD0, ExifSubIFD, GpsSubIFD]

instance Arbitrary TagLocation where
  arbitrary = genTagLocation

genExifTag :: Gen ExifTag
genExifTag =
  frequency
    [ (8, elements allTags),
      ( 2,
        do
          loc <- arbitrary
          -- Prevent generating structural offset tags which corrupt the TIFF tree
          key <- arbitrary `suchThat` (\k -> k /= 0x8769 && k /= 0x8825)
          name <- arbitrary
          -- Use (T.pack . show) to fulfill the (ExifValue -> Text) signature
          pure $ ExifTag loc name key (T.pack . show)
      )
    ]

instance Arbitrary ExifTag where
  arbitrary = genExifTag

genExifValue :: Gen ExifValue
genExifValue =
  oneof
    [ -- Constrain to Word32 to prevent 64-bit negative wrap-arounds
      ExifNumber . fromIntegral <$> (arbitrary :: Gen Word32),
      ExifText <$> listOf (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " ,.-_")),
      do
        n <- fromIntegral <$> (arbitrary :: Gen Int32)
        Positive (d :: Int32) <- arbitrary
        pure (ExifRational n (fromIntegral d)),
      -- Guarantee length >= 2 so HsExif doesn't parse it as a scalar ExifNumber
      do
        x <- fromIntegral <$> (arbitrary :: Gen Word32)
        y <- fromIntegral <$> (arbitrary :: Gen Word32)
        xs <- listOf (fromIntegral <$> (arbitrary :: Gen Word32))
        pure (ExifNumberList (x : y : xs)),
      -- Guarantee length >= 2 so HsExif doesn't parse it as a scalar ExifRational
      do
        let genRat = do
              n <- fromIntegral <$> (arbitrary :: Gen Int32)
              Positive (d :: Int32) <- arbitrary
              pure (n, fromIntegral d)
        x <- genRat
        y <- genRat
        xs <- listOf genRat
        pure (ExifRationalList (x : y : xs)),
      ExifUndefined . BS.pack <$> arbitrary,
      do
        -- Format must be 0 or >10, otherwise HsExif parses it as a native type
        fmt <- arbitrary `suchThat` (\f -> f == 0 || f > 10)
        NonNegative count <- arbitrary
        -- Value must fit in unsigned 32-bit to avoid Int wrap-arounds
        val <- fromIntegral <$> (arbitrary :: Gen Word32)
        pure (ExifUnknown fmt count val)
    ]

instance Arbitrary ExifValue where
  arbitrary = genExifValue

  shrink = \case
    ExifNumber n -> ExifNumber <$> shrink n
    ExifText s -> ExifText <$> shrink s
    ExifRational n d ->
      [ExifRational n' d | n' <- shrink n]
        ++ [ExifRational n d' | Positive d' <- shrink (Positive d)]
    -- Filter out lists of length 1 during shrinking!
    ExifNumberList xs ->
      ExifNumberList <$> filter (\l -> length l /= 1) (shrink xs)
    ExifRationalList rs ->
      ExifRationalList <$> filter (\l -> length l /= 1) (shrink rs)
    ExifUndefined bs -> ExifUndefined . BS.pack <$> shrink (BS.unpack bs)
    ExifUnknown t c v ->
      -- Ensure shrunk format doesn't accidentally become 1-10
      [ExifUnknown t' c v | t' <- shrink t, t' == 0 || t' > 10]
        ++ [ExifUnknown t c' v | NonNegative c' <- shrink (NonNegative c)]
        ++ [ExifUnknown t c v' | NonNegative v' <- shrink (NonNegative v)]
