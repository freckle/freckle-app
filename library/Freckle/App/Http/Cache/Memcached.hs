{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Freckle.App.Http.Cache.Memcached
  ( memcachedHttpCacheSettings
  , memcachedHttpCodec
  , memcachedHttpCache
  ) where

import Freckle.App.Prelude

import Blammo.Logging (MonadLogger, logDebugNS, logWarnNS)
import Codec.Serialise (Serialise (..), deserialiseOrFail, serialise)
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Database.Memcache.Types (Value)
import Freckle.App.Http.Cache
import Freckle.App.Memcached
import Freckle.App.Memcached.Client qualified as Memcached
import Freckle.App.OpenTelemetry (MonadTracer)
import Network.HTTP.Client (Request)
import Network.HTTP.Client.Internal qualified as HTTP
import Network.HTTP.Types.Header (ResponseHeaders)
import Network.HTTP.Types.Status (Status (..))
import Network.HTTP.Types.Version (HttpVersion (..))

memcachedHttpCacheSettings
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadTracer m
     , MonadReader env m
     , HasMemcachedClient env
     )
  => CacheTTL
  -- ^ Default TTL, used when @max-age@ is not present
  -> HttpCacheSettings m Value
memcachedHttpCacheSettings defaultTTL =
  HttpCacheSettings
    { shared = True
    , cacheable = const True
    , defaultTTL
    , getCurrentTime = liftIO getCurrentTime
    , logDebug = logDebugNS "http.cache"
    , logWarn = logWarnNS "http.cache"
    , codec = memcachedHttpCodec
    , cache = memcachedHttpCache
    }

memcachedHttpCodec :: HttpCacheCodec Value
memcachedHttpCodec =
  HttpCacheCodec
    { serialise = BSL.toStrict . serialise . fromResponse
    , deserialise = \req ->
        bimap show (toResponse req)
          . deserialiseOrFail
          . BSL.fromStrict
    }

memcachedHttpCache
  :: ( MonadUnliftIO m
     , MonadTracer m
     , MonadReader env m
     , HasMemcachedClient env
     )
  => HttpCache m Value
memcachedHttpCache =
  HttpCache
    { get = try . Memcached.get
    , set = \k v -> try $ Memcached.set k v 0
    , evict = try . Memcached.delete
    }

-- | Representation of 'CachedResponse' that can be given a 'Serialise' instance
--
-- In 'fromResponse' we need to flatten the 'Response' down and remove fields
-- that can't (or shouldn't) be cached, then restore them again later in
-- 'toResponse'.
data SerialiseResponse = SerialiseResponse
  { sresponseStatus :: Status
  , sresponseVersion :: HttpVersion
  , sresponseHeaders :: ResponseHeaders
  , sresponseBody :: PotentiallyGzipped BSL.ByteString
  , sresponseEarlyHints :: ResponseHeaders
  , sinserted :: UTCTime
  , sttl :: CacheTTL
  }
  deriving stock (Generic)
  deriving anyclass (Serialise)

{- FOURMOLU_DISABLE -}
-- Fourmolu has trouble with this bit of CPP

toResponse :: Request -> SerialiseResponse -> CachedResponse
toResponse req c = CachedResponse
  { response = HTTP.Response
      { HTTP.responseStatus = sresponseStatus c
      , HTTP.responseVersion = sresponseVersion c
      , HTTP.responseHeaders = sresponseHeaders c
      , HTTP.responseBody = sresponseBody c
      , HTTP.responseCookieJar = mempty
      , HTTP.responseClose' = HTTP.ResponseClose (pure ())
      , HTTP.responseOriginalRequest = req
#if MIN_VERSION_http_client(0,7,16)
      , HTTP.responseEarlyHints = sresponseEarlyHints c
#endif
      }
  , inserted = c.sinserted
  , ttl = c.sttl
  }

fromResponse :: CachedResponse -> SerialiseResponse
fromResponse cr =
  SerialiseResponse
    { sresponseStatus = HTTP.responseStatus r
    , sresponseVersion = HTTP.responseVersion r
    , sresponseHeaders = HTTP.responseHeaders r
    , sresponseBody = HTTP.responseBody r
#if MIN_VERSION_http_client(0,7,16)
    , sresponseEarlyHints = HTTP.responseEarlyHints r
#else
    , sresponseEarlyHints = []
#endif
    , sinserted = cr.inserted
    , sttl = cr.ttl
    }
 where
  r = cr.response

#if !MIN_VERSION_http_types(0,12,4)
deriving stock instance Generic HttpVersion

deriving stock instance Generic Status
#endif

{- FOURMOLU_ENABLE -}

deriving anyclass instance Serialise HttpVersion

deriving anyclass instance Serialise Status

instance (CI.FoldCase a, Serialise a) => Serialise (CI a) where
  encode = encode . CI.original
  decode = CI.mk <$> decode
