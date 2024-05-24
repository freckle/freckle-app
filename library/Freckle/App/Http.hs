-- | Centralized module for making HTTP requestshttp
--
-- These functions:
--
-- - Do not throw exceptions on non-200
-- - May throw for other 'HttpException' cases (e.g. 'ConnectionTimeout')
-- - Capture decoding failures with 'Either' values as the 'Response' body
-- - Handle 429-@Retry-In@ for you (if using an 'IO'-based instance)
module Freckle.App.Http
  ( MonadHttp (..)

    -- * Decoding responses
  , httpJson
  , HttpDecodeError (..)
  , httpDecode

    -- * Pagination
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
  , setRequestBodyJSON
  , setRequestBodyURLEncoded
  , setRequestCheckStatus
  , setRequestPath
  , disableRequestDecompress

    -- * Response accessors
  , Response
  , getResponseStatus
  , getResponseBody

    -- ** Unsafe access
  , getResponseBodyUnsafe

    -- * Exceptions
  , HttpException (..)
    -- | Predicates useful for handling 'HttpException's
    --
    -- For example, given a function 'guarded', which returns 'Just' a given value
    -- when a predicate holds for it (otherwise 'Nothing'), you can add
    -- error-handling specific to exceptions caused by 4XX responses:
    --
    -- @
    -- flip 'catchJust' (guard 'httpExceptionIsClientError' *> handle4XXError) $ do
    --   resp <- 'httpJson' $ 'setRequestCheckStatus' $ parseRequest_ "http://..."
    --   body <- 'getResponseBodyUnsafe' resp
    --
    --   -- ...
    -- @
  , httpExceptionIsInformational
  , httpExceptionIsRedirection
  , httpExceptionIsClientError
  , httpExceptionIsServerError

    -- * "Network.HTTP.Types" re-exports
  , Status
  , statusCode
  , statusIsInformational
  , statusIsSuccessful
  , statusIsRedirection
  , statusIsClientError
  , statusIsServerError
  ) where

import Freckle.App.Prelude

import Conduit (foldC, mapMC, runConduit, (.|))
import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Validate (ValidateT)
import Control.Monad.Writer (WriterT)
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.List.NonEmpty as NE
import Freckle.App.Http.Paginate
import Freckle.App.Http.Retry
import qualified Network.HTTP.Client as HTTP (Request (..))
import Network.HTTP.Conduit (HttpExceptionContent (..))
import Network.HTTP.Simple hiding (httpLbs, httpNoBody)
import qualified Network.HTTP.Simple as HTTP
import Network.HTTP.Types.Header (hAccept, hAuthorization)
import Network.HTTP.Types.Status
  ( Status
  , statusCode
  , statusIsClientError
  , statusIsInformational
  , statusIsRedirection
  , statusIsServerError
  , statusIsSuccessful
  )

-- | Type-class for making HTTP requests
--
-- Functions of this module require the 'MonadHttp' constraint. This type class
-- allows us to instantiate differently in different contexts, most usefully
-- with stubbed responses in test. (See "Freckle.App.Test.Http".)
--
-- The 'IO' instance does what you would expect, and can be used to either build
-- your own instances:
--
-- @
-- instance MonadIO m => MonadHttp (AppT m) where
--   httpLbs = liftIO . httpLbs
--
-- instance MonadHttp (HandlerFor App) where
--   httpLbs = liftIO . httpLbs
-- @
--
-- Or directly,
--
-- @
-- resp <- liftIO $ httpLbs ...
-- @
class Monad m => MonadHttp m where
  httpLbs :: Request -> m (Response ByteString)

instance MonadHttp IO where
  httpLbs = rateLimited HTTP.httpLbs

instance MonadHttp m => MonadHttp (MaybeT m) where
  httpLbs = lift . httpLbs

instance MonadHttp m => MonadHttp (ReaderT r m) where
  httpLbs = lift . httpLbs

instance (Monoid w, MonadHttp m) => MonadHttp (WriterT w m) where
  httpLbs = lift . httpLbs

instance MonadHttp m => MonadHttp (StateT s m) where
  httpLbs = lift . httpLbs

instance MonadHttp m => MonadHttp (ExceptT e m) where
  httpLbs = lift . httpLbs

instance MonadHttp m => MonadHttp (ValidateT e m) where
  httpLbs = lift . httpLbs

data HttpDecodeError = HttpDecodeError
  { hdeBody :: ByteString
  , hdeErrors :: NonEmpty String
  }
  deriving stock (Eq, Show)

instance Exception HttpDecodeError where
  displayException HttpDecodeError {..} =
    unlines $
      ["Error decoding HTTP Response:", "Raw body:", BSL8.unpack hdeBody]
        <> fromErrors hdeErrors
   where
    fromErrors = \case
      err NE.:| [] -> ["Error:", err]
      errs -> "Errors:" : map bullet (NE.toList errs)
    bullet = (" • " <>)

-- | Make a request and parse the body as JSON
--
-- @
-- -- Throws, but only on a complete failure to perform the request
-- resp <- 'httpJson' $ 'parseRequest_' "https://example.com"
--
-- -- Safe access
-- 'getResponseBody' resp :: Either 'HttpDecodeError' a
--
-- -- Unsafe access (throws on Left)
-- 'getResponseBodyUnsafe' resp :: m a
-- @
httpJson
  :: (MonadHttp m, FromJSON a)
  => Request
  -> m (Response (Either HttpDecodeError a))
httpJson =
  httpDecode (first pure . Aeson.eitherDecode)
    . addAcceptHeader "application/json"

-- | Make a request and decode the body using the given function
--
-- This be used to request other formats, e.g. CSV.
httpDecode
  :: MonadHttp m
  => (ByteString -> Either (NonEmpty String) a)
  -> Request
  -> m (Response (Either HttpDecodeError a))
httpDecode decode req = do
  resp <- httpLbs req
  let body = getResponseBody resp
  pure $ first (HttpDecodeError body) . decode <$> resp

-- | Request all pages of a paginated endpoint into some 'Monoid'
--
-- For example,
--
-- Interact with a paginated endpoint where each page is a JSON list, combining
-- all the pages into one list (i.e. 'concat') and throw on any decoding errors:
--
-- @
-- 'httpPaginated' 'httpJson' 'getResponseBodyUnsafe' $ 'parseRequest_' "https://..."
-- @
--
-- This uses 'sourcePaginated', and so reads a @Link@ header. To do otherwise,
-- drop down to 'sourcePaginatedBy' directly.
--
-- The second argument is used to extract the data to combine out of the
-- response. This is particularly useful for 'Either' values, like you may get
-- from 'httpJson'. It lives in @m@ to support functions such as
-- 'getResponseBodyUnsafe'.
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
-- See "Freckle.Http.App.Paginate" to process requested pages in a streaming
-- fashion, or perform pagination based on somethign other than @Link@.
httpPaginated
  :: (MonadHttp m, Monoid b)
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

disableRequestDecompress :: Request -> Request
disableRequestDecompress req =
  req
    { HTTP.decompress = const False
    }

-- | Read an 'Either' response body, throwing any 'Left' as an exception
--
-- If you plan to use this function, and haven't built your decoding to handle
-- error response bodies too, you'll want to use 'setRequestCheckStatus' so that
-- you see status-code exceptions before 'HttpDecodeError's.
getResponseBodyUnsafe
  :: (MonadIO m, Exception e, HasCallStack)
  => Response (Either e a)
  -> m a
getResponseBodyUnsafe = either throwM pure . getResponseBody

httpExceptionIsInformational :: HttpException -> Bool
httpExceptionIsInformational = filterStatusException statusIsInformational

httpExceptionIsRedirection :: HttpException -> Bool
httpExceptionIsRedirection = filterStatusException statusIsRedirection

httpExceptionIsClientError :: HttpException -> Bool
httpExceptionIsClientError = filterStatusException statusIsClientError

httpExceptionIsServerError :: HttpException -> Bool
httpExceptionIsServerError = filterStatusException statusIsServerError

filterStatusException :: (Status -> Bool) -> HttpException -> Bool
filterStatusException predicate = \case
  HttpExceptionRequest _ (StatusCodeException resp _) ->
    predicate $ getResponseStatus resp
  _ -> False
