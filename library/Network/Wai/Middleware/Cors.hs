-- | TODO: Can we use <https://hackage.haskell.org/package/wai-cors> instead?
module Network.Wai.Middleware.Cors
  ( corsMiddleware
  ) where

import Freckle.App.Prelude

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Types (ResponseHeaders)
import Network.HTTP.Types.Status (status200)
import Network.Wai
import Network.Wai.Middleware.AddHeaders

corsMiddleware
  :: (ByteString -> Bool)
  -- ^ Predicate that returns 'True' for valid @Origin@ values
  -> [ByteString]
  -- ^ Extra headers to add to @Expose-Headers@
  -> Middleware
corsMiddleware validateOrigin extraExposedHeaders =
  handleOptions validateOrigin extraExposedHeaders
    . addCORSHeaders validateOrigin extraExposedHeaders

handleOptions :: (ByteString -> Bool) -> [ByteString] -> Middleware
handleOptions validateOrigin extraExposedHeaders app req sendResponse =
  case (requestMethod req, lookup "Origin" (requestHeaders req)) of
    ("OPTIONS", Just origin) ->
      sendResponse $
        responseLBS
          status200
          ( toHeaders $ corsResponseHeaders validateOrigin extraExposedHeaders origin
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
    Just origin ->
      addHeaders
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
