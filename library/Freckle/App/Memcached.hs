-- | App-level caching backed by Memcached
--
-- Usage:
--
-- 1. Have a Reader-like monad stack over some @App@
-- 2. Set up that @App@ with 'HasMemcachedClient'
-- 3. Give the value to cache a 'Cachable' instance
-- 4. Use 'caching'
--
-- To avoid 'Cachable', see 'cachingAs' and 'cachingAsJSON'.
module Freckle.App.Memcached
  ( Cachable (..)
  , caching
  , cachingAs
  , cachingAsJSON
  , cachingAsCBOR

    -- * Re-exports
  , module Freckle.App.Memcached.Client
  , module Freckle.App.Memcached.CacheKey
  , module Freckle.App.Memcached.CacheTTL
  , module Freckle.App.Memcached.MD5
  ) where

import Freckle.App.Prelude

import Blammo.Logging
import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Freckle.App.Memcached.CacheKey
import Freckle.App.Memcached.CacheTTL
import Freckle.App.Memcached.Client (HasMemcachedClient (..))
import qualified Freckle.App.Memcached.Client as Memcached
import Freckle.App.Memcached.MD5
import UnliftIO.Exception (Exception (..), handleAny)

class Cachable a where
  toCachable :: a -> ByteString
  fromCachable :: ByteString -> Either String a

instance Cachable ByteString where
  toCachable = id
  fromCachable = Right

instance Cachable BSL.ByteString where
  toCachable = BSL.toStrict
  fromCachable = Right . BSL.fromStrict

instance Cachable Text where
  toCachable = encodeUtf8
  fromCachable = Right . decodeUtf8With lenientDecode

data Cached a
  = CacheFound a
  | CacheNotFound
  | CacheError Text

-- | Memoize an action using Memcached and 'Cachable'
caching
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasMemcachedClient env
     , Cachable a
     )
  => CacheKey
  -> CacheTTL
  -> m a
  -> m a
caching = cachingAs fromCachable toCachable

-- | Like 'caching', but with explicit conversion functions
cachingAs
  :: (MonadUnliftIO m, MonadLogger m, MonadReader env m, HasMemcachedClient env)
  => (ByteString -> Either String a)
  -> (a -> ByteString)
  -> CacheKey
  -> CacheTTL
  -> m a
  -> m a
cachingAs from to key ttl f = do
  result <-
    fmap (maybe CacheNotFound (either (CacheError . pack) CacheFound . from)) $
      handleCachingError Nothing "getting" $
        Memcached.get key

  case result of
    CacheFound a -> pure a
    CacheNotFound -> store
    CacheError e -> do
      logCachingError "deserializing" e
      store
 where
  store = do
    a <- f
    a <$ handleCachingError () "setting" (Memcached.set key (to a) ttl)

-- | Like 'caching', but de/serializing the value as JSON
cachingAsJSON
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasMemcachedClient env
     , FromJSON a
     , ToJSON a
     )
  => CacheKey
  -> CacheTTL
  -> m a
  -> m a
cachingAsJSON = cachingAs eitherDecodeStrict encodeStrict

-- | Cache data in memcached in CBOR format
cachingAsCBOR
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasMemcachedClient env
     , Serialise a
     )
  => CacheKey
  -> CacheTTL
  -> m a
  -> m a
cachingAsCBOR =
  cachingAs
    (first show . deserialiseOrFail . BSL.fromStrict)
    (BSL.toStrict . serialise)

handleCachingError
  :: (MonadUnliftIO m, MonadLogger m) => a -> Text -> m a -> m a
handleCachingError value action = handleAny $ \ex -> do
  logCachingError action $ pack $ displayException ex
  pure value

logCachingError :: MonadLogger m => Text -> Text -> m ()
logCachingError action message =
  logErrorNS "caching" $
    "Error "
      <> action
      :# ["action" .= action, "message" .= message]

encodeStrict :: ToJSON a => a -> ByteString
encodeStrict = BSL.toStrict . encode
