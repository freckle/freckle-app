-- | Centralized module for making HTTP requests from the backend
--
-- These functions:
--
-- - Do not throw exceptions on non-200
-- - May throw for other 'HttpException' cases (e.g. 'ConnectionTimeout')
-- - Handle 429-@Retry-In@ for you
-- - Capture decoding failures with 'Either' values as the 'Response' body
--
-- == Examples
--
-- Make request, retry on 429s, and parse the body as JSON.
--
-- @
-- -- Throws, but only on a complete failure to perform the request
-- resp <- 'httpJson' $ 'parseRequest' "https://example.com"
--
-- -- Safe access
-- 'getResponseBody' resp :: Either ('HttpDecodeError' String) a
--
-- -- Unsafe access (throws on Left)
-- 'getResponseBodyUnsafe' resp :: m a
-- @
--
-- 'httpLbs' can be used to get a raw response (without risk of decoding
-- errors), and 'httpDecode' can be used to supply your own decoding function
-- (e.g. for CSV).
--
-- Interact with a paginated endpoint that uses @Link@, combining all the pages
-- monoidally (e.g. concat) and throwing on any decoding errors.
--
-- @
-- 'httpPaginated' 'httpJson' 'getResponseBodyUnsafe' $ 'parseRequest_' "https://..."
-- @
--
-- Decoding errors can be handled differently by adjusting what 'Monoid' you
-- convert each page's response into:
--
-- @
-- 'httpPaginated' 'httpJson' fromResponseLenient $ 'parseRequest_' "https://..."
--
-- fromResponseLenient
--   :: MonadLogger m
--   => Response (Either e [MyJsonThing])
--   -> m [MyJsonThing]
-- fromResponseLenient r = case getResponseBody r of
--      Left _ -> [] <$ logWarn "..."
--      Right a -> pure a
-- @
--
-- See "FrontRow.Http.App.Paginate" to process requested pages in a streaming
-- fashion, or perform pagination based on somethign other than @Link@.
--
module FrontRow.App.Http
  ( httpJson
  , HttpDecodeError(..)
  , httpDecode
  , httpLbs
  , httpPaginated
  , sourcePaginated

  -- * Request builders
  , Request
  , parseRequest
  , parseRequest_

  -- * Request modifiers
  , addRequestHeader
  , addAcceptHeader
  , addBearerAuthorizationHeader
  , addToRequestQueryString
  , setRequestBasicAuth

  -- * Response accessors
  , Response
  , getResponseStatus
  , getResponseBody

  -- ** Unsafe access
  , getResponseBodyUnsafe

  -- * "Network.HTTP.Types" re-exports
  , Status
  , statusCode
  )
where

import Prelude

import Conduit (foldC, mapMC, runConduit, (.|))
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import FrontRow.App.Http.Paginate
import FrontRow.App.Http.Retry
import Network.HTTP.Simple hiding (httpLbs)
import Network.HTTP.Types.Header (hAccept, hAuthorization)
import Network.HTTP.Types.Status (Status, statusCode)
import UnliftIO.Exception (Exception(..), throwIO)

data HttpDecodeError e = HttpDecodeError
  { hdeBody :: ByteString
  , hdeErrors :: NonEmpty e
  }
  deriving stock (Eq, Show)

instance Exception e => Exception (HttpDecodeError e) where
  displayException HttpDecodeError {..} =
    unlines
      $ ["Error decoding HTTP Response:", "Raw body:", BSL8.unpack hdeBody]
      <> fromErrors hdeErrors
   where
    fromErrors = \case
      err :| [] -> ["Error:", displayException err]
      errs -> "Errors:" : map (bullet . displayException) (NE.toList errs)
    bullet = (" • " <>)

-- | Request and decode a response as JSON
httpJson
  :: (MonadIO m, FromJSON a)
  => Request
  -> m (Response (Either (HttpDecodeError String) a))
httpJson = httpDecode (first pure . Aeson.eitherDecode)
  . addAcceptHeader "application/json"

-- | Request and decode a response
httpDecode
  :: MonadIO m
  => (ByteString -> Either (NonEmpty e) a)
  -> Request
  -> m (Response (Either (HttpDecodeError e) a))
httpDecode decode req = do
  resp <- httpLbs req
  let body = getResponseBody resp
  pure $ first (HttpDecodeError body) . decode <$> resp

-- | Request a lazy 'ByteString', handling 429 retries
httpLbs :: MonadIO m => Request -> m (Response ByteString)
httpLbs = rateLimited httpLBS . setRequestIgnoreStatus

-- | Request all pages of a paginated endpoint into a big list
--
-- This uses 'sourcePaginated', and so reads a @Link@ header. To do otherwise,
-- drop down to 'sourcePaginatedBy' directly.
--
-- The second argument is used to extract the data to combine out of the
-- response. This is particularly useful for 'Either' values, like you may get
-- from 'httpJson'. It lives in @m@ to support functions such as 'getResponseBodyUnsafe'.
--
httpPaginated
  :: (MonadIO m, Monoid b)
  => (Request -> m (Response a))
  -> (Response a -> m b)
  -> Request
  -> m b
httpPaginated runRequest getBody req =
  runConduit $ sourcePaginated runRequest req .| mapMC getBody .| foldC

addAcceptHeader :: BS.ByteString -> Request -> Request
addAcceptHeader = addRequestHeader hAccept

addBearerAuthorizationHeader :: BS.ByteString -> Request -> Request
addBearerAuthorizationHeader = addRequestHeader hAuthorization . ("Bearer " <>)

-- | Read an 'Either' response body, throwing any 'Left' as an exception
getResponseBodyUnsafe
  :: (MonadIO m, Exception e) => Response (Either e a) -> m a
getResponseBodyUnsafe = either throwIO pure . getResponseBody
