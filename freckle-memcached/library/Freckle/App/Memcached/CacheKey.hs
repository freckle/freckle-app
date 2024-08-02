module Freckle.App.Memcached.CacheKey
  ( CacheKey
  , cacheKey
  , cacheKeyThrow
  , fromCacheKey
  ) where

import Prelude

import Control.Exception.Annotated.UnliftIO (throwWithCallStack)
import Control.Monad.IO.Class (MonadIO)
import Data.Char (isControl, isSpace)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Database.Memcache.Types (Key)
import GHC.Stack (HasCallStack)
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
  | T.any isSpace t = invalid "Cannot contain whitespace"
  | otherwise = Right $ CacheKey t
 where
  invalid msg =
    Left $ "Not a valid memcached key:\n  " <> T.unpack t <> "\n\n" <> msg

-- | Build a 'CacheKey' and throw if invalid
cacheKeyThrow :: (MonadIO m, HasCallStack) => Text -> m CacheKey
cacheKeyThrow = either (throwWithCallStack . userError) pure . cacheKey

fromCacheKey :: CacheKey -> Key
fromCacheKey = T.encodeUtf8 . unCacheKey
