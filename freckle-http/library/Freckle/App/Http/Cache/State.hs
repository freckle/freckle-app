{-# LANGUAGE NoFieldSelectors #-}

-- | HTTP caching via 'MonadState'
--
-- This module implements HTTP caching for simple use-cases, such as testing
-- "Freckle.App.Http.Cache" itself.
module Freckle.App.Http.Cache.State
  ( CachedResponse (..)
  , Cache (..)
  , HasCache (..)
  , stateHttpCacheSettings
  , stateHttpCacheCodec
  , stateHttpCache
  ) where

import Prelude

import Blammo.Logging (Message)
import Control.Lens (Lens', at, lens, use, (.=), (?=))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (ToLogStr (..), fromLogStr)
import Control.Monad.State (MonadState)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Data.Text.IO qualified as T
import Data.Time (getCurrentTime)
import Freckle.App.Http.Cache
import Freckle.App.Memcached.CacheKey
import Freckle.App.Memcached.CacheTTL
import System.IO qualified as IO

newtype Cache = Cache
  { map :: HashMap CacheKey CachedResponse
  }
  deriving newtype (Semigroup, Monoid)

mapL :: Lens' Cache (HashMap CacheKey CachedResponse)
mapL = lens (.map) $ \x y -> x {map = y}

class HasCache env where
  cacheL :: Lens' env Cache

instance HasCache Cache where
  cacheL = id

stateHttpCacheSettings
  :: ( MonadIO m
     , MonadState s m
     , HasCache s
     )
  => HttpCacheSettings m CachedResponse
stateHttpCacheSettings =
  HttpCacheSettings
    { shared = False
    , cacheable = const True
    , defaultTTL = fiveMinuteTTL
    , getCurrentTime = liftIO getCurrentTime
    , logDebug = \_ -> pure ()
    , logWarn = liftIO . T.hPutStrLn IO.stderr . messageToText
    , codec = stateHttpCacheCodec
    , cache = stateHttpCache
    }

stateHttpCacheCodec :: HttpCacheCodec CachedResponse
stateHttpCacheCodec =
  HttpCacheCodec
    { serialise = id
    , deserialise = const Right
    }

stateHttpCache
  :: (MonadIO m, MonadState s m, HasCache s) => HttpCache m CachedResponse
stateHttpCache =
  HttpCache
    { get = \key -> fmap Right $ use $ cacheL . mapL . at key
    , set = \key resp _ -> fmap Right $ cacheL . mapL . at key ?= resp
    , evict = \key -> fmap Right $ cacheL . mapL . at key .= Nothing
    }

messageToText :: Message -> Text
messageToText = T.decodeUtf8With T.lenientDecode . fromLogStr . toLogStr
