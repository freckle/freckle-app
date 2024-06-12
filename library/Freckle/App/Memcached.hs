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
import Data.ByteString.Lazy qualified as BSL
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Freckle.App.Exception (annotatedExceptionMessage)
import Freckle.App.Memcached.CacheKey
import Freckle.App.Memcached.CacheTTL
import Freckle.App.Memcached.Client (HasMemcachedClient (..))
import Freckle.App.Memcached.Client qualified as Memcached
import Freckle.App.Memcached.MD5
import Freckle.App.OpenTelemetry

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

data CachingError
  = CacheGetError SomeException
  | CacheSetError SomeException
  | CacheDeserializeError String
  deriving stock (Show)

instance Exception CachingError where
  displayException = \case
    CacheGetError ex -> "Unable to get: " <> displayException ex
    CacheSetError ex -> "Unable to set: " <> displayException ex
    CacheDeserializeError err -> "Unable to deserialize: " <> err

-- | Log any thrown 'CachingError's as warnings and return the given value
warnOnCachingError :: (MonadUnliftIO m, MonadLogger m) => a -> m a -> m a
warnOnCachingError val =
  flip catch $
    (val <$)
      . logWarnNS "caching"
      . annotatedExceptionMessage @CachingError

-- | Memoize an action using Memcached and 'Cachable'
caching
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadTracer m
     , MonadReader env m
     , HasMemcachedClient env
     , Cachable a
     , HasCallStack
     )
  => CacheKey
  -> CacheTTL
  -> m a
  -> m a
caching = cachingAs fromCachable toCachable

-- | Like 'caching', but with explicit conversion functions
cachingAs
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadTracer m
     , MonadReader env m
     , HasMemcachedClient env
     , HasCallStack
     )
  => (ByteString -> Either String a)
  -> (a -> ByteString)
  -> CacheKey
  -> CacheTTL
  -> m a
  -> m a
cachingAs from to key ttl f = do
  mCached <- warnOnCachingError Nothing $ traverse cacheDeserialize =<< cacheGet
  maybe store pure mCached
 where
  store = do
    a <- f
    a <$ warnOnCachingError () (cacheSet a)

  cacheGet = flip catch (throwM . CacheGetError) $ Memcached.get key
  cacheSet a = flip catch (throwM . CacheSetError) $ Memcached.set key (to a) ttl
  cacheDeserialize = either (throwM . CacheDeserializeError) pure . from

-- | Like 'caching', but de/serializing the value as JSON
cachingAsJSON
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadTracer m
     , MonadReader env m
     , HasMemcachedClient env
     , FromJSON a
     , ToJSON a
     , HasCallStack
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
     , MonadTracer m
     , MonadReader env m
     , HasMemcachedClient env
     , Serialise a
     , HasCallStack
     )
  => CacheKey
  -> CacheTTL
  -> m a
  -> m a
cachingAsCBOR =
  cachingAs
    (first show . deserialiseOrFail . BSL.fromStrict)
    (BSL.toStrict . serialise)

encodeStrict :: ToJSON a => a -> ByteString
encodeStrict = BSL.toStrict . encode
