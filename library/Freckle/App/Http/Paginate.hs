-- | Streaming interface for paginated HTTP APIs
--
-- == Examples
--
-- Take an action on each page as it is requested:
--
-- @
-- let req = parseRequest_ "https://..."
--
-- runConduit
--   $ sourcePaginated httpJson req
--   .| mapM_C onEachPage
--
-- onEachPage :: Response (Either HttpDecodeError [MyJsonThing]) -> m ()
-- onEachPage = undefined
-- @
--
-- Take and action /and/ collect:
--
-- @
-- allPages <- runConduit
--   $ 'sourcePaginated' httpJson req
--   .| iterM onEachPage
--   .| sinkList
-- @
--
-- For APIs that do pagination not via @Link@, you can use 'sourcePaginatedBy'
--
-- @
-- data Page a = Page
--   { pData :: [a]
--   , pNext :: Int
--   }
--
-- instance FromJSON a => FromJSON (Item a) where
--   parseJSON = withObject "Page" $ \o -> Page
--     <$> o .: "data"
--     <*> o .: "next"
--
-- runConduit
--   $ 'sourcePaginatedBy' nextPage httpJson req
--   .| mapMC (fmap pData . 'getResponseBodyUnsafe')
--   .| foldC
--
-- nextPage
--   :: Request
--   -> Response (Either ('HttpDecodeError' String) (Page a))
--   -> Maybe Request
-- nextPage req resp = do
--   body <- hush $ getResponseBody resp
--   let next = C8.pack $ show $ pNext body
--   pure $ addToRequestQueryString [("next", Just next)] req
-- @
module Freckle.App.Http.Paginate
  ( sourcePaginated
  , sourcePaginatedBy
  ) where

import Freckle.App.Prelude

import Conduit
import Control.Error.Util (hush)
import Network.HTTP.Link.Compat hiding (linkHeader)
import Network.HTTP.Simple

-- | Stream pages of a paginated response, using @Link@ to find next pages
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
  links <- hush $ parseLinkURI $ decodeUtf8 header
  uri <- href <$> find (((Rel, "next") `elem`) . linkParams) links
  parseRequest $ show uri
