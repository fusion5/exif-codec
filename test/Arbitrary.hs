{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary
  ( genTagLocation
  , genExifTag
  , genExifValue
  ) where

import Codec.Exif
  ( ExifTag (..)
  , ExifValue (..)
  , TagLocation (..)
  , allTags
  )
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Prelude
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Positive (..)
  , elements
  , frequency
  , listOf
  , listOf1
  , oneof
  , shrink
  )

genTagLocation :: Gen TagLocation
genTagLocation = elements [IFD0, ExifSubIFD, GpsSubIFD]

instance Arbitrary TagLocation where
  arbitrary = genTagLocation

genExifTag :: Gen ExifTag
genExifTag = frequency
  [ (8, elements allTags)
  , (2, ExifTag <$> arbitrary <*> arbitrary <*> arbitrary <*> pure (T.pack . show))
  ]

instance Arbitrary ExifTag where
  arbitrary = genExifTag

genExifValue :: Gen ExifValue
genExifValue = oneof
  [ ExifNumber <$> arbitrary
  , ExifText <$> listOf (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " ,.-_"))
  , do
      n <- arbitrary
      Positive d <- arbitrary
      pure (ExifRational n d)
  , ExifNumberList <$> listOf1 arbitrary
  , do
      rats <- listOf1 $ do
        n <- arbitrary
        Positive d <- arbitrary
        pure (n, d)
      pure (ExifRationalList rats)
  , ExifUndefined . BS.pack <$> arbitrary
  , ExifUnknown <$> arbitrary <*> arbitrary <*> arbitrary
  ]

instance Arbitrary ExifValue where
  arbitrary = genExifValue
  shrink = \case
    ExifNumber n -> ExifNumber <$> shrink n
    ExifText s -> ExifText <$> shrink s
    ExifRational n d ->
      [ExifRational n' d | n' <- shrink n]
        ++ [ExifRational n d' | Positive d' <- shrink (Positive d)]
    ExifNumberList xs -> ExifNumberList <$> shrink xs
    ExifRationalList rs -> ExifRationalList <$> shrink rs
    ExifUndefined bs -> ExifUndefined . BS.pack <$> shrink (BS.unpack bs)
    ExifUnknown t c v ->
      [ExifUnknown t' c v | t' <- shrink t]
        ++ [ExifUnknown t c' v | c' <- shrink c]
        ++ [ExifUnknown t c v' | v' <- shrink v]
