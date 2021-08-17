-- | Integration of "Freckle.App" tooling with "Network.Wai"
module Freckle.App.Wai
  ( RouteName(..)
  , TraceId(..)
  , makeLoggingMiddleware
  , noCacheMiddleware
  , corsMiddleware
  )
where

import Prelude

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.Logger (LogLevel(..))
import Data.Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import Data.Default (def)
import Data.IP (fromHostAddress, fromHostAddress6)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Freckle.App.Logging
import Network.HTTP.Types (QueryItem, ResponseHeaders)
import Network.HTTP.Types.Status (Status, status200, statusCode)
import Network.Socket
import Network.Wai
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.RequestLogger
  ( Destination(Logger)
  , OutputFormat(..)
  , OutputFormatterWithDetails
  , destination
  , mkRequestLogger
  , outputFormat
  )
import System.Log.FastLogger (LoggerSet, toLogStr)

newtype RouteName = RouteName
  { unRouteName :: Text
  }
  deriving newtype ToJSON

newtype TraceId = TraceId
  { unTraceId :: Text
  }
  deriving newtype ToJSON

makeLoggingMiddleware
  :: HasLogging app
  => app
  -> (Request -> Maybe RouteName)
  -> (Request -> Maybe TraceId)
  -> LoggerSet
  -> IO Middleware
makeLoggingMiddleware app getRouteName getTraceId ls = case getLogFormat app of
  FormatJSON ->
    makeWith
      $ CustomOutputFormatWithDetails
      $ suppressByStatus (getLogLevel app)
      $ jsonOutputFormatter getRouteName getTraceId
  FormatTerminal -> makeWith $ Detailed $ getLogDefaultANSI app
 where
  makeWith format =
    mkRequestLogger def { outputFormat = format, destination = Logger ls }

suppressByStatus
  :: LogLevel -> OutputFormatterWithDetails -> OutputFormatterWithDetails
suppressByStatus minLevel f date req status responseSize duration reqBody response
  | statusLevel status >= minLevel
  = f date req status responseSize duration reqBody response
  | otherwise
  = ""

jsonOutputFormatter
  :: (Request -> Maybe RouteName)
  -> (Request -> Maybe TraceId)
  -> OutputFormatterWithDetails
jsonOutputFormatter getRouteName getTraceId date req status responseSize duration _reqBody response
  = toLogStr $ formatJsonNoLoc (statusLevel status) $ object
    [ "time" .= decodeUtf8 date
    , "method" .= decodeUtf8 (requestMethod req)
    , "route" .= getRouteName req
    , "path" .= decodeUtf8 (rawPathInfo req)
    , "query_string" .= map queryItemToJSON (queryString req)
    , "status" .= statusCode status
    , "duration_ms" .= (duration * 1000)
    , "request_size" .= requestBodyLengthToJSON (requestBodyLength req)
    , "response_size" .= responseSize
    , "response_body" .= do
      guard $ statusCode status >= 400
      Just $ maybeDecodeToValue $ toLazyByteString response
    , "trace_id" .= getTraceId req
    , "client_ip" .= (decodeUtf8 <$> clientIp)
    ]
  where clientIp = requestRealIp req <|> Just (sockAddrToIp $ remoteHost req)

statusLevel :: Status -> LogLevel
statusLevel status = case statusCode status of
  404 -> LevelInfo -- Special case
  code | code >= 500 -> LevelError
  code | code >= 400 -> LevelWarn
  code | code >= 300 -> LevelInfo
  _ -> LevelDebug

decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode

requestBodyLengthToJSON :: RequestBodyLength -> Value
requestBodyLengthToJSON ChunkedBody = String "Unknown"
requestBodyLengthToJSON (KnownLength l) = toJSON l

queryItemToJSON :: QueryItem -> Value
queryItemToJSON (name, mValue) =
  toJSON (decodeUtf8 name, decodeUtf8 <$> mValue)

-- Try to decode as a 'Value'. Otherwise make a JSON string.
maybeDecodeToValue :: BSL.ByteString -> Value
maybeDecodeToValue str =
  fromMaybe (toJSON . decodeUtf8With lenientDecode . BSL.toStrict $ str)
    . decode @Value
    $ str

-- Copied from bugnag-haskell

requestRealIp :: Request -> Maybe ByteString
requestRealIp request =
  requestForwardedFor request <|> lookup "X-Real-IP" (requestHeaders request)

requestForwardedFor :: Request -> Maybe ByteString
requestForwardedFor request =
  readForwardedFor =<< lookup "X-Forwarded-For" (requestHeaders request)

-- |
--
-- >>> readForwardedFor ""
-- Nothing
--
-- >>> readForwardedFor "123.123.123"
-- Just "123.123.123"
--
-- >>> readForwardedFor "123.123.123, 45.45.45"
-- Just "123.123.123"
--
readForwardedFor :: ByteString -> Maybe ByteString
readForwardedFor bs
  | BS8.null bs = Nothing
  | otherwise = Just $ fst $ BS8.break (== ',') bs

sockAddrToIp :: SockAddr -> ByteString
sockAddrToIp (SockAddrInet _ h) = BS8.pack $ show $ fromHostAddress h
sockAddrToIp (SockAddrInet6 _ _ h _) = BS8.pack $ show $ fromHostAddress6 h
sockAddrToIp (SockAddrUnix _) = "<socket>"

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
