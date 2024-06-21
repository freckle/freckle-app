module Freckle.App.Monoid.Average
  ( Average (..)
  , average
  , average1
  , averageDatum
  , getAverage
  , getAverageInt
  , getRoundedAverage
  , weightedAverage
  , weightedAverages
  , AverageableEnum
  , averageableEnum
  , getAverageableEnum
  ) where

import Freckle.App.Prelude

import Freckle.App.Foldable1 (foldMap1')
import Autodocodec qualified
import Data.Aeson (ToJSON (..))
import Data.Semigroup.Foldable (Foldable1)
import Test.QuickCheck

average :: (Foldable t, Fractional a) => t a -> Maybe a
average = getAverage . foldMap' averageDatum

average1 :: (Foldable1 t, Fractional a) => t a -> a
average1 = getAverageSafe . foldMap1' averageDatum
 where
  getAverageSafe (Average l n) = n / fromIntegral l

data Average n = Average !Int !n
  deriving stock (Show)

deriving via
  (Autodocodec.Autodocodec (Average Int))
  instance
    (ToJSON (Average Int))

deriving via
  (Autodocodec.Autodocodec (Average a))
  instance
    (Fractional a, Autodocodec.HasCodec a)
    => (ToJSON (Average a))

instance Arbitrary n => Arbitrary (Average n) where
  arbitrary = Average <$> (abs <$> arbitrary @Int) <*> arbitrary @n

instance (Fractional n, Eq n) => Eq (Average n) where
  a == b = getAverage a == getAverage b

getAverage :: Fractional n => Average n -> Maybe n
getAverage (Average l n) =
  if l == 0 then Nothing else Just $ n / fromIntegral l

getRoundedAverage :: RealFrac n => Average n -> Maybe Int
getRoundedAverage = fmap round . getAverage

getAverageInt :: Average Int -> Maybe Int
getAverageInt (Average l n) =
  roundInt
    <$> getAverage (Average l (fromIntegral n))
 where
  roundInt = round :: Double -> Int

-- Calculate a weighted average from a list with integral weights.
--
-- This is useful for calculating correctness values for assignment sessions.
--
-- >>> weightedAverages [(0, 2), (1, 1)]
-- Average 3 1
--
-- >>> getAverage (weightedAverages [(0, 2), (1, 1)])
-- Just 0.3333333333333333
--
weightedAverages
  :: (Integral weight, Foldable f, Num n) => f (n, weight) -> Average n
weightedAverages correctnesses = foldMap avg correctnesses
 where
  avg (correctness, worth) = weightedAverage worth correctness

-- Construct a weighted average with integral weights.
--
-- This is useful for calculating correctness values for assignment sessions.
--
-- >>> weightedAverage 3 2
-- Average 6 6
--
weightedAverage :: (Integral weight, Num n) => weight -> n -> Average n
weightedAverage weight =
  mconcat . replicate (fromIntegral weight) . averageDatum

averageDatum :: n -> Average n
averageDatum = Average 1

instance Num n => Semigroup (Average n) where
  Average lx nx <> Average ly ny = Average (lx + ly) (nx + ny)

instance Num n => Monoid (Average n) where
  mempty = Average 0 0

-- | Averaging `Int`s is something we often want to do for display purposes.
-- To do this, the average is rounded.
instance {-# OVERLAPPING #-} Autodocodec.HasCodec (Average Int) where
  codec =
    Autodocodec.bimapCodec
      (const $ Left "Can't decode Average")
      getAverageInt
      (Autodocodec.codec @(Maybe Int))

instance (Fractional n, Autodocodec.HasCodec n) => Autodocodec.HasCodec (Average n) where
  codec =
    Autodocodec.bimapCodec
      (const $ Left "Can't decode Average")
      getAverage
      (Autodocodec.codec @(Maybe n))

-- | Representation of an @'Enum'@ type that can be averaged
--
-- An enumeration can be averaged if it represents evenly-spaced points along a
-- linear path, much like numbers. It works as you would expect:
--
-- @
-- data Fruit = Apple | Orange | Pear deriving Enum
--
-- 'averageEnum' [Apple, Pear]
-- => Just Orange
-- @
newtype AverageableEnum a
  = AverageableEnum (Average Double)
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

instance (Arbitrary a, Enum a) => Arbitrary (AverageableEnum a) where
  arbitrary = averageableEnum <$> arbitrary @a

averageableEnum :: Enum a => a -> AverageableEnum a
averageableEnum =
  AverageableEnum . averageDatum . fromIntegral . (+ 1) . fromEnum

getAverageableEnum :: Enum a => AverageableEnum a -> Maybe a
getAverageableEnum (AverageableEnum avg) =
  toEnum . subtract 1 <$> getRoundedAverage avg
