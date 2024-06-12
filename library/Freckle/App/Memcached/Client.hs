module Freckle.App.Memcached.Client
  ( MemcachedClient (..)
  , newMemcachedClient
  , withMemcachedClient
  , memcachedClientDisabled
  , HasMemcachedClient (..)
  , get
  , set
  , delete
  ) where

import Freckle.App.Prelude

import Control.Lens (Lens', view, _1)
import Data.HashMap.Strict qualified as HashMap
import Database.Memcache.Client qualified as Memcache
import Database.Memcache.Types (Value, Version)
import Freckle.App.Memcached.CacheKey
import Freckle.App.Memcached.CacheTTL
import Freckle.App.Memcached.Servers
import Freckle.App.OpenTelemetry
import OpenTelemetry.Trace qualified as Trace
import UnliftIO.Exception (finally)
import Yesod.Core.Lens
import Yesod.Core.Types (HandlerData)

data MemcachedClient
  = MemcachedClient Memcache.Client
  | MemcachedClientDisabled

class HasMemcachedClient env where
  memcachedClientL :: Lens' env MemcachedClient

instance HasMemcachedClient MemcachedClient where
  memcachedClientL = id

instance HasMemcachedClient site => HasMemcachedClient (HandlerData child site) where
  memcachedClientL = envL . siteL . memcachedClientL

newMemcachedClient :: MonadIO m => MemcachedServers -> m MemcachedClient
newMemcachedClient servers = case toServerSpecs servers of
  [] -> pure memcachedClientDisabled
  specs -> liftIO $ MemcachedClient <$> Memcache.newClient specs Memcache.def

withMemcachedClient
  :: MonadUnliftIO m => MemcachedServers -> (MemcachedClient -> m a) -> m a
withMemcachedClient servers f = do
  c <- newMemcachedClient servers
  f c `finally` quitClient c

memcachedClientDisabled :: MemcachedClient
memcachedClientDisabled = MemcachedClientDisabled

get
  :: (MonadUnliftIO m, MonadTracer m, MonadReader env m, HasMemcachedClient env)
  => CacheKey
  -> m (Maybe Value)
get k = traced $ with $ \case
  MemcachedClient mc -> liftIO $ view _1 <$$> Memcache.get mc (fromCacheKey k)
  MemcachedClientDisabled -> pure Nothing
 where
  traced =
    inSpan
      "cache.get"
      clientSpanArguments
        { Trace.attributes =
            HashMap.fromList
              [ ("service.name", "memcached")
              , ("key", Trace.toAttribute k)
              ]
        }

-- | Set a value to expire in the given seconds
--
-- Pass @0@ to set a value that never expires.
set
  :: (MonadUnliftIO m, MonadTracer m, MonadReader env m, HasMemcachedClient env)
  => CacheKey
  -> Value
  -> CacheTTL
  -> m ()
set k v expiration = traced $ with $ \case
  MemcachedClient mc ->
    void $
      liftIO $
        Memcache.set mc (fromCacheKey k) v 0 $
          fromCacheTTL
            expiration
  MemcachedClientDisabled -> pure ()
 where
  traced =
    inSpan
      "cache.set"
      clientSpanArguments
        { Trace.attributes =
            HashMap.fromList
              [ ("service.name", "memcached")
              , ("key", Trace.toAttribute k)
              , ("value", byteStringToAttribute v)
              , ("expiration", Trace.toAttribute expiration)
              ]
        }

-- | Delete a key
delete
  :: (MonadUnliftIO m, MonadTracer m, MonadReader env m, HasMemcachedClient env)
  => CacheKey
  -> m ()
delete k = traced $ with $ \case
  MemcachedClient mc -> void $ liftIO $ Memcache.delete mc (fromCacheKey k) bypassCAS
  MemcachedClientDisabled -> pure ()
 where
  traced =
    inSpan
      "cache.delete"
      clientSpanArguments
        { Trace.attributes = HashMap.fromList [("key", Trace.toAttribute k)]
        }

quitClient :: MonadIO m => MemcachedClient -> m ()
quitClient = \case
  MemcachedClient mc -> void $ liftIO $ Memcache.quit mc
  MemcachedClientDisabled -> pure ()

with
  :: (MonadReader env m, HasMemcachedClient env)
  => (MemcachedClient -> m a)
  -> m a
with f = do
  c <- view memcachedClientL
  f c

-- | The sentinal version @0@ means to not perform CAS checking
bypassCAS :: Version
bypassCAS = 0
