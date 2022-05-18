module Freckle.App.Memcached.Client
  ( MemcachedClient
  , newMemcachedClient
  , HasMemcachedClient(..)
  , get
  , set
  , with
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

newtype MemcachedClient = MemcachedClient
  { unMemcachedClient :: Memcache.Client
  }

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

get
  :: (MonadIO m, MonadReader env m, HasMemcachedClient env)
  => CacheKey
  -> m (Maybe Value)
get k = with $ \mc -> liftIO $ view _1 <$$> Memcache.get mc (fromCacheKey k)

-- | Set a value to expire in the given seconds
--
-- Pass @0@ to create a value that never expires.
--
set
  :: (MonadIO m, MonadReader env m, HasMemcachedClient env)
  => CacheKey
  -> Value
  -> CacheTTL
  -> m ()
set k v expiration = with $ \mc -> do
  void $ liftIO $ Memcache.set mc (fromCacheKey k) v 0 $ fromCacheTTL expiration

-- | Escape-hatch for usage not available through encapsulating functions
with
  :: (MonadReader env m, HasMemcachedClient env)
  => (Memcache.Client -> m a)
  -> m a
with f = do
  c <- view memcachedClientL
  f $ unMemcachedClient c
