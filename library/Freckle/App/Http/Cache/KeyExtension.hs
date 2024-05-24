module Freckle.App.Http.Cache.KeyExtension
  ( CacheKeyExtension (..)
  , includeHeader

    -- * Normalizing helpers
  , normalizeGzip
  ) where

import Freckle.App.Prelude

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Simple (Request, getRequestHeader)
import Network.HTTP.Types.Header (HeaderName)

newtype CacheKeyExtension = CacheKeyExtension
  { run :: Request -> [ByteString]
  }

instance Semigroup CacheKeyExtension where
  CacheKeyExtension a <> CacheKeyExtension b =
    CacheKeyExtension $ (<>) <$> a <*> b

instance Monoid CacheKeyExtension where
  mempty = CacheKeyExtension $ const []

includeHeader
  :: (ByteString -> ByteString)
  -- ^ Function used to normalize values
  --
  -- If your header produces variations that should all be treated the same when
  -- used in a cache-key, you should normalize them with this function.
  --
  -- See <https://www.fastly.com/blog/best-practices-using-vary-header/#normalization>
  -> HeaderName
  -> CacheKeyExtension
includeHeader f h = CacheKeyExtension $ map f . getRequestHeader h

normalizeGzip :: ByteString -> ByteString
normalizeGzip bs
  | "gzip" `BS8.isInfixOf` bs = "gzip"
  | otherwise = bs
