{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Cache HTTP responses like a CDN or browser would
module Freckle.App.Http.Cache
  ( HttpCacheSettings (..)
  , HttpCacheCodec (..)
  , HttpCache (..)
  , httpCached
  , CachedResponse (..)
  , PotentiallyGzipped
  ) where

import Freckle.App.Prelude

import Blammo.Logging (Message (..), (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.List.Extra (firstJust)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (addUTCTime, defaultTimeLocale, parseTimeM)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Freckle.App.Http.Cache.Gzip
import Freckle.App.Http.Header
import Freckle.App.Memcached
import Network.HTTP.Client (Request, Response)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Simple
  ( addRequestHeader
  , getRequestHeader
  , getResponseStatus
  )
import Network.HTTP.Types.Header
  ( HeaderName
  , hAge
  , hCacheControl
  , hETag
  , hExpires
  , hIfNoneMatch
  )
import Network.HTTP.Types.Status (Status, statusCode)

data HttpCacheSettings m t = HttpCacheSettings
  { shared :: Bool
  , cacheable :: Request -> Bool
  , cacheKeyHeaders :: [HeaderName]
  , defaultTTL :: CacheTTL
  , getCurrentTime :: m UTCTime
  , logDebug :: Message -> m ()
  , logWarn :: Message -> m ()
  , codec :: HttpCacheCodec t
  , cache :: HttpCache m t
  }

data HttpCacheCodec t = HttpCacheCodec
  { serialise :: CachedResponse -> t
  , deserialise :: Request -> t -> Either String CachedResponse
  }

data HttpCache m t = HttpCache
  { get :: CacheKey -> m (Either SomeException (Maybe t))
  , set :: CacheKey -> t -> m (Either SomeException ())
  , evict :: CacheKey -> m (Either SomeException ())
  }

data CachedResponse = CachedResponse
  { response :: Response (PotentiallyGzipped BSL.ByteString)
  , inserted :: UTCTime
  , ttl :: CacheTTL
  }
  deriving stock (Show)

isCachedResponseStale :: CachedResponse -> UTCTime -> Bool
isCachedResponseStale cached now =
  addUTCTime (fromIntegral cached.ttl) cached.inserted < now

-- Wrap a function from "Freckle.App.Http" with caching
--
-- Verify that the request is cacheable (e.g. a @GET@), then cache it at a
-- derived key (from URL). The response will only be cached if @Cache-Control@
-- allows it. @Cache-Control@ is also used to determine TTL (e.g. @max-age@)
--
-- - <https://developer.mozilla.org/en-US/docs/Web/HTTP/Caching#fresh_and_stale_based_on_age>
--
-- If a cached response is stale, but it has an @ETag@ header, we will make the
-- request using @If-None-Match@ and still return (and retain) that cached
-- response if we receive a @304@ response.
--
-- - <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/ETag#caching_of_unchanged_resources>
--
httpCached
  :: forall m t
   . MonadIO m
  => HttpCacheSettings m t
  -> (Request -> m (Response BSL.ByteString))
  -> Request
  -> m (Response BSL.ByteString)
httpCached settings doHttp req =
  maybe (doHttp req) handleCachableRequest $ getCachableRequestKey settings req
 where
  handleCachableRequest key = do
    now <- settings.getCurrentTime
    result <- fromEx Nothing $ settings.cache.get key

    let tkey = decodeUtf8With lenientDecode $ fromCacheKey key

    case result of
      Nothing -> do
        settings.logDebug $ "Cache miss" :# ["key" .= tkey]
        writeCache now key =<< getResponse req
      Just val -> do
        settings.logDebug $ "Cache hit" :# ["key" .= tkey]
        case settings.codec.deserialise req val of
          Left err -> do
            settings.logWarn $ "Error deserialising" :# ["error" .= err]
            writeCache now key =<< getResponse req
          Right cresp | isCachedResponseStale cresp now -> do
            settings.logDebug $
              "Cached value stale"
                :# [ "key" .= tkey
                   , "inserted" .= cresp.inserted
                   , "ttl" .= fromCacheTTL cresp.ttl
                   , "now" .= now
                   ]
            case lookupHeader hETag cresp.response of
              Nothing -> do
                fromEx () $ settings.cache.evict key
                writeCache now key =<< getResponse req
              Just etag -> do
                settings.logDebug $
                  "Retrying with If-None-Match"
                    :# [ "key" .= tkey
                       , "etag" .= decodeUtf8With lenientDecode etag
                       ]
                resp <- getResponse $ addRequestHeader hIfNoneMatch etag req
                case statusCode (getResponseStatus resp) of
                  304 -> do
                    settings.logDebug "ETag matched (304), retaining cached response"

                    -- We want to rewrite the cache entry based on Cache-Control
                    -- from base do now. Otherwise, we'll continue to treat it
                    -- as stale and do this 304 dance every time. But we use the
                    -- Cache-Control header from this response, in case it
                    -- differs
                    writeCache now key $ setCacheControlFrom resp cresp.response
                  _ -> do
                    settings.logDebug "ETag not matched, evicting cache"
                    fromEx () $ settings.cache.evict key
                    writeCache now key resp
          Right cresp -> gunzipResponseBody req cresp.response

  getResponse :: Request -> m (Response (PotentiallyGzipped BSL.ByteString))
  getResponse = requestPotentiallyGzipped doHttp

  writeCache
    :: UTCTime
    -> CacheKey
    -> Response (PotentiallyGzipped BSL.ByteString)
    -> m (Response BSL.ByteString)
  writeCache now key resp = do
    for_ (getCachableResponseTTL settings resp) $ \ttl -> do
      settings.logDebug $
        "Write cache"
          :# [ "key" .= decodeUtf8With lenientDecode (fromCacheKey key)
             , "ttl" .= fromCacheTTL ttl
             ]
      let cresp = CachedResponse {response = resp, inserted = now, ttl = ttl}
      fromEx () $ settings.cache.set key $ settings.codec.serialise cresp

    gunzipResponseBody req resp

  fromEx :: a -> m (Either SomeException a) -> m a
  fromEx a f = do
    result <- f
    case result of
      Left ex -> do
        settings.logWarn $ "Caching error" :# ["error" .= displayException ex]
        pure a
      Right v -> pure v

-- | Return a 'CacheKey' for a 'Request', if it's cacheable
--
-- A 'Request' is cacheable if all are true:
--
-- - The given predicate succeeds
-- - The method is @GET@
-- - A @Cache-Control@ header with @no-store@ is not present
--
-- If cacheable, the 'CacheKey' is built from: method, scheme, host, port, path,
-- query + any @Vary@ headers.
getCachableRequestKey
  :: HttpCacheSettings m t -> Request -> Maybe CacheKey
getCachableRequestKey settings req = do
  guard $ settings.cacheable req
  guard $ HTTP.method req == "GET"
  guard $ NoStore `notElem` requestHeaders.cacheControl
  guard $ not settings.shared || Private `notElem` requestHeaders.cacheControl
  pure $ md5CacheKey cacheKeyAttributes
 where
  requestHeaders = getRequestHeaders req

  cacheKeyAttributes =
    ( HTTP.method req
    , HTTP.secure req
    , HTTP.host req
    , HTTP.port req
    , HTTP.path req
    , HTTP.queryString req
    , concatMap (`getRequestHeader` req) settings.cacheKeyHeaders
    )

-- | Return a 'CacheTTL' for a 'Response', if it's cacheable
--
-- A 'Response' is cacheable if all are true:
--
-- - A @Cache-Control@ header with @no-store@ is not present
-- - If the cache is shared (first argument), a @Cache-Control@ header with
--   @private@ is not preset
-- - The response has a cacheable status code
--
-- If cacheable, the @Cache-Control[max-age]@, @Age@, and @Expires@ response
-- headers are used to compute the 'CacheTTL'.
getCachableResponseTTL
  :: HttpCacheSettings m t -> Response body -> Maybe CacheTTL
getCachableResponseTTL settings resp = do
  guard $ NoStore `notElem` responseHeaders.cacheControl
  guard $ not settings.shared || Private `notElem` responseHeaders.cacheControl
  guard $ statusIsCacheable $ HTTP.responseStatus resp
  pure $ fromMaybe settings.defaultTTL $ responseHeadersToTTL responseHeaders
 where
  responseHeaders = getResponseHeaders resp

statusIsCacheable :: Status -> Bool
statusIsCacheable = (`elem` cacheableStatusCodes) . statusCode

-- | As per RFC 7231
--
-- <https://stackoverflow.com/a/39406969>
cacheableStatusCodes :: [Int]
cacheableStatusCodes =
  [ 200 -- OK
  , 203 -- Non-Authoritative Information
  , 204 -- No Content
  , 206 -- Partial Content
  , 300 -- Multiple Choices
  , 301 -- Moved Permanently
  , 404 -- Not Found
  , 405 -- Method Not Allowed
  , 410 -- Gone
  , 414 -- URI Too Long
  , 501 -- Not Implemented
  ]

newtype Seconds = Seconds {unwrap :: Int}
  deriving stock (Eq)
  deriving newtype (Num, Show, Read)

data CacheControl
  = Private
  | NoStore
  | MaxAge Seconds
  deriving stock (Eq, Show)

cacheControlMaxAge :: [CacheControl] -> Maybe Seconds
cacheControlMaxAge = firstJust $ \case
  MaxAge s -> Just s
  _ -> Nothing

readCacheControl :: ByteString -> Maybe CacheControl
readCacheControl = go . CI.foldCase
 where
  go = \case
    "private" -> Just Private
    "no-store" -> Just NoStore
    h | Just s <- BS8.stripPrefix "max-age=" h -> MaxAge <$> readMay (BS8.unpack s)
    _ -> Nothing

getCacheControl :: HasHeaders a => a -> [CacheControl]
getCacheControl = mapMaybe readCacheControl . getHeaderCsv hCacheControl

setCacheControlFrom :: Response a -> Response b -> Response b
setCacheControlFrom from to =
  to
    { HTTP.responseHeaders = toNonCCHeader <> fromCCHeader
    }
 where
  fromCCHeader = filter ((== hCacheControl) . fst) $ getHeaders from
  toNonCCHeader = filter ((/= hCacheControl) . fst) $ getHeaders to

newtype RequestHeaders = RequestHeaders
  { cacheControl :: [CacheControl]
  }

getRequestHeaders :: Request -> RequestHeaders
getRequestHeaders req =
  RequestHeaders
    { cacheControl = getCacheControl req
    }

data ResponseHeaders = ResponseHeaders
  { cacheControl :: [CacheControl]
  , age :: Seconds
  -- ^ Defaults to 0 if missing
  , expires :: Maybe UTCTime
  }

getResponseHeaders :: Response body -> ResponseHeaders
getResponseHeaders resp =
  ResponseHeaders
    { cacheControl = getCacheControl resp
    , age = fromMaybe 0 $ do
        h <- lookupHeader hAge resp
        readMay $ BS8.unpack h
    , expires = do
        h <- lookupHeader hExpires resp
        parseTimeM True defaultTimeLocale httpDateFormat $ BS8.unpack h
    }

-- | <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Date#syntax>
httpDateFormat :: String
httpDateFormat = "%a, %d %b %Y %H:%M:%S GMT"

responseHeadersToTTL :: ResponseHeaders -> Maybe CacheTTL
responseHeadersToTTL hs = cacheTTL . (.unwrap) <$> viaMaxAge <|> viaExpires
 where
  viaMaxAge = subtract hs.age <$> cacheControlMaxAge hs.cacheControl
  viaExpires = round . utcTimeToPOSIXSeconds <$> hs.expires
