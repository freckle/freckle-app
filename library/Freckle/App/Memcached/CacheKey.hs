module Freckle.App.Memcached.CacheKey
  ( CacheKey
  , cacheKey
  , cacheKeyThrow
  , fromCacheKey
  ) where

import Freckle.App.Prelude

import Data.Char (isControl, isSpace)
import qualified Data.Text as T
import Database.Memcache.Types (Key)
import OpenTelemetry.Trace (ToAttribute (..))

newtype CacheKey = CacheKey Text
  deriving stock (Show)
  deriving newtype (Eq, Hashable)

unCacheKey :: CacheKey -> Text
unCacheKey (CacheKey x) = x

instance ToAttribute CacheKey where
  toAttribute = toAttribute . unCacheKey

-- | Build a 'CacheKey', ensuring it's valid for Memcached
--
-- <https://github.com/memcached/memcached/blob/master/doc/protocol.txt#L41>
--
-- @
-- Currently the length limit of a key is set at 250 characters (of course,
-- normally clients wouldn't need to use such long keys); the key must not
-- include control characters or whitespace.
-- @
cacheKey :: Text -> Either String CacheKey
cacheKey t
  | T.length t > 250 = invalid "Must be fewer than 250 characters"
  | T.any isControl t = invalid "Cannot contain control characters"
  | T.any isSpace t = invalid "Cannot container whitespace"
  | otherwise = Right $ CacheKey t
 where
  invalid msg =
    Left $ "Not a valid memcached key:\n  " <> unpack t <> "\n\n" <> msg

-- | Build a 'CacheKey' and throw if invalid
cacheKeyThrow :: (MonadIO m, HasCallStack) => Text -> m CacheKey
cacheKeyThrow = either throwString pure . cacheKey

fromCacheKey :: CacheKey -> Key
fromCacheKey = encodeUtf8 . unCacheKey
