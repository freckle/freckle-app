-- | Integration of "Freckle.App" tooling with "Network.Wai"
module Freckle.App.Wai
  ( makeRequestMetricsMiddleware
  , noCacheMiddleware
  , corsMiddleware
  , denyFrameEmbeddingMiddleware
  ) where

import Freckle.App.Prelude hiding (decodeUtf8)

import Control.Monad.Reader (runReaderT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Freckle.App.Datadog (HasDogStatsClient, HasDogStatsTags)
import qualified Freckle.App.Datadog as Datadog
import Network.HTTP.Types (ResponseHeaders)
import Network.HTTP.Types.Status (status200, statusCode)
import Network.Wai
import Network.Wai.Middleware.AddHeaders (addHeaders)

makeRequestMetricsMiddleware
  :: (HasDogStatsClient env, HasDogStatsTags env)
  => env
  -> (Request -> [(Text, Text)])
  -> Middleware
makeRequestMetricsMiddleware env getTags app req sendResponse' = do
  start <- getCurrentTime
  app req $ \res -> do
    flip runReaderT env $ do
      Datadog.increment "requests" $ tags res
      Datadog.histogramSinceMs "response_time_ms" (tags res) start
    sendResponse' res
 where
  tags res =
    getTags req
      <> [ ("method", decodeUtf8 $ requestMethod req)
         , ("status", pack $ show $ statusCode $ responseStatus res)
         ]

noCacheMiddleware :: Middleware
noCacheMiddleware = addHeaders [cacheControlHeader]
 where
  cacheControlHeader =
    ("Cache-Control", "no-cache, no-store, max-age=0, private")

corsMiddleware
  :: (ByteString -> Bool)
  -- ^ Predicate that returns 'True' for valid @Origin@ values
  -> [ByteString]
  -- ^ Extra headers to add to @Expose-Headers@
  -> Middleware
corsMiddleware validateOrigin extraExposedHeaders =
  handleOptions validateOrigin extraExposedHeaders
    . addCORSHeaders validateOrigin extraExposedHeaders

-- | Middleware that adds header to deny all frame embedding
denyFrameEmbeddingMiddleware :: Middleware
denyFrameEmbeddingMiddleware = addHeaders [("X-Frame-Options", "DENY")]

handleOptions :: (ByteString -> Bool) -> [ByteString] -> Middleware
handleOptions validateOrigin extraExposedHeaders app req sendResponse =
  case (requestMethod req, lookup "Origin" (requestHeaders req)) of
    ("OPTIONS", Just origin) -> sendResponse $ responseLBS
      status200
      (toHeaders $ corsResponseHeaders validateOrigin extraExposedHeaders origin
      )
      mempty
    _ -> app req sendResponse
 where
  toHeaders :: [(ByteString, ByteString)] -> ResponseHeaders
  toHeaders = map (first CI.mk)

addCORSHeaders :: (ByteString -> Bool) -> [ByteString] -> Middleware
addCORSHeaders validateOrigin extraExposedHeaders app req sendResponse =
  case lookup "Origin" (requestHeaders req) of
    Nothing -> app req sendResponse
    Just origin -> addHeaders
      (corsResponseHeaders validateOrigin extraExposedHeaders origin)
      app
      req
      sendResponse

corsResponseHeaders
  :: (ByteString -> Bool)
  -> [ByteString]
  -> ByteString
  -> [(ByteString, ByteString)]
corsResponseHeaders validateOrigin extraExposedHeaders origin =
  [ ("Access-Control-Allow-Origin", validatedOrigin)
  , ("Access-Control-Allow-Methods", "POST, GET, OPTIONS, PUT, DELETE, PATCH")
  , ("Access-Control-Allow-Credentials", "true")
  , ("Access-Control-Allow-Headers", "Content-Type, *")
  , ("Access-Control-Expose-Headers", BS.intercalate ", " exposedHeaders)
  ]
 where
  validatedOrigin = if validateOrigin origin then origin else "BADORIGIN"

  exposedHeaders =
    ["Set-Cookie", "Content-Disposition", "Link"] <> extraExposedHeaders

decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode
