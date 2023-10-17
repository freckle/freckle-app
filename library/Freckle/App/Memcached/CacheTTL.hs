module Freckle.App.Memcached.CacheTTL
  ( CacheTTL
  , cacheTTL
  , fromCacheTTL
  , fiveMinuteTTL
  ) where

import Freckle.App.Prelude

import Data.Word (Word32)
import Database.Memcache.Types (Expiration)

newtype CacheTTL = CacheTTL Int
  deriving stock (Show)
  deriving newtype (Eq, Ord, Enum, Num, Real, Integral)

cacheTTL :: Int -> CacheTTL
cacheTTL = CacheTTL

fromCacheTTL :: CacheTTL -> Expiration
fromCacheTTL (CacheTTL i)
  | i < fromIntegral minWord = minWord
  | i > fromIntegral maxWord = maxWord
  | otherwise = fromIntegral i
 where
  minWord :: Word32
  minWord = minBound

  maxWord :: Word32
  maxWord = maxBound

-- | Standard 5 minute time to live
fiveMinuteTTL :: CacheTTL
fiveMinuteTTL = cacheTTL $ 5 * 60
