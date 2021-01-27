module FrontRow.App.Http.Paginate
  ( sourcePaginated
  , sourcePaginatedBy
  )
where

import Prelude

import Conduit
import Data.Foldable (traverse_)
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Link hiding (linkHeader)
import Network.HTTP.Simple

-- | Stream pages of a paginated response, using @Link@ to find next pages
--
-- @
-- allPages <- runConduit
--   $ sourcePaginated httpJson req
--   .| iterM onEachPage
--   .| sinkList
-- @
--
sourcePaginated
  :: MonadIO m
  => (Request -> m (Response body))
  -- ^ Run one request
  -> Request
  -- ^ Initial request
  -> ConduitT i (Response body) m ()
sourcePaginated = sourcePaginatedBy linkHeader

-- | Stream pages of a paginated response, using a custom /find next/
sourcePaginatedBy
  :: MonadIO m
  => (Request -> Response body -> Maybe Request)
  -- ^ How to get the next page from each request
  -> (Request -> m (Response body))
  -- ^ Run one request
  -> Request
  -- ^ Initial request
  -> ConduitT i (Response body) m ()
sourcePaginatedBy mNextRequest runRequest req = do
  resp <- lift $ runRequest req
  yield resp
  traverse_ (sourcePaginatedBy mNextRequest runRequest) $ mNextRequest req resp

linkHeader :: Request -> Response body -> Maybe Request
linkHeader _req resp = do
  header <- listToMaybe $ getResponseHeader "Link" resp
  links <- parseLinkHeader $ decodeUtf8 header
  uri <- href <$> find (((Rel, "next") `elem`) . linkParams) links
  parseRequest $ show uri
