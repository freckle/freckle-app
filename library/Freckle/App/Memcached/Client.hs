module Freckle.App.Memcached.Client
  ( MemcachedClient
  , newMemcachedClient
  , memcachedClientDisabled
  , HasMemcachedClient(..)
  , get
  , set
  ) where

import Freckle.App.Prelude

import Control.Lens (Lens', _1, view)
import qualified Database.Memcache.Client as Memcache
import Database.Memcache.Types (Value)
import Freckle.App.Memcached.CacheKey
import Freckle.App.Memcached.CacheTTL
import Freckle.App.Memcached.Servers
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
newMemcachedClient urls =
  liftIO $ MemcachedClient <$> Memcache.newClient specs Memcache.def
  where specs = toServerSpecs urls

memcachedClientDisabled :: MemcachedClient
memcachedClientDisabled = MemcachedClientDisabled

get
  :: (MonadIO m, MonadReader env m, HasMemcachedClient env)
  => CacheKey
  -> m (Maybe Value)
get k = with $ \case
  MemcachedClient mc -> liftIO $ view _1 <$$> Memcache.get mc (fromCacheKey k)
  MemcachedClientDisabled -> pure Nothing

-- | Set a value to expire in the given seconds
--
-- Pass @0@ to set a value that never expires.
--
set
  :: (MonadIO m, MonadReader env m, HasMemcachedClient env)
  => CacheKey
  -> Value
  -> CacheTTL
  -> m ()
set k v expiration = with $ \case
  MemcachedClient mc ->
    void $ liftIO $ Memcache.set mc (fromCacheKey k) v 0 $ fromCacheTTL
      expiration
  MemcachedClientDisabled -> pure ()

with
  :: (MonadReader env m, HasMemcachedClient env)
  => (MemcachedClient -> m a)
  -> m a
with f = do
  c <- view memcachedClientL
  f c
