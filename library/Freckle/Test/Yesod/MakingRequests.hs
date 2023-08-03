module Freckle.Test.Yesod.MakingRequests
  ( get
  , post
  , postBody
  , performMethod
  , followRedirect
  , getLocation

    -- * With query parameters
  , getParams
  , deleteParams

    -- * With JSON body
  , jsonBody
  , postJsonBody
  , patchJsonBody
  , putJsonBody
  , deleteJsonBody
  )
where

import Freckle.App.Prelude
import Freckle.Test.Yesod.MonadYesodExample
import Freckle.Test.Yesod.RequestBuilder

import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Yesod.Core (ParseRoute, RedirectUrl, RenderRoute (Route))
import qualified Yesod.Test

-- | Perform a GET request to url
get
  :: forall url m site
   . (MonadYesodExample site m, RedirectUrl site url)
  => url
  -> m ()
get = liftYesodExample . Yesod.Test.get

-- | Perform a POST request to url
post
  :: forall url m site
   . (MonadYesodExample site m, RedirectUrl site url)
  => url
  -> m ()
post = liftYesodExample . Yesod.Test.post

-- | Perform a POST request to url with the given body
postBody
  :: forall url m site
   . (MonadYesodExample site m, RedirectUrl site url)
  => url
  -> BSL.ByteString
  -> m ()
postBody u b = liftYesodExample $ Yesod.Test.postBody u b

-- | Perform a request using a given method to url
performMethod
  :: forall url m site
   . (MonadYesodExample site m, RedirectUrl site url)
  => ByteString
  -> url
  -> m ()
performMethod b u = liftYesodExample $ Yesod.Test.performMethod b u

-- | Follow a redirect, if the last response was a redirect
followRedirect
  :: forall m site
   . MonadYesodExample site m
  => m (Either Text Text)
  -- ^ Left with an error message if not a redirect,
  --   Right with the redirected URL if it was
followRedirect = liftYesodExample Yesod.Test.followRedirect

-- | Parse the Location header of the last response
getLocation
  :: forall m site
   . (MonadYesodExample site m, ParseRoute site)
  => m (Either Text (Route site))
getLocation = liftYesodExample Yesod.Test.getLocation

-- | Send a request specified by method, URL, and JSON body
jsonBody
  :: forall url body m site
   . (ToJSON body, MonadYesodExample site m, RedirectUrl site url)
  => ByteString
  -- ^ Request method
  -> url
  -- ^ URL
  -> body
  -> m ()
jsonBody method url body = request $ do
  setUrl url
  setMethod method
  jsonHeaders
  setRequestBody $ encode body

-- | Set a POST request specified by URL and JSON body
postJsonBody
  :: forall j url m site
   . (ToJSON j, MonadYesodExample site m, RedirectUrl site url)
  => url
  -- ^ URL
  -> j
  -- ^ JSON body to send
  -> m ()
postJsonBody = jsonBody "POST"

-- | Set a PATCH request specified by URL and JSON body
patchJsonBody
  :: forall j url m site
   . (ToJSON j, MonadYesodExample site m, RedirectUrl site url)
  => url
  -- ^ URL
  -> j
  -- ^ JSON body to send
  -> m ()
patchJsonBody = jsonBody "PATCH"

-- | Set a PUT request specified by URL and JSON body
putJsonBody
  :: forall j url m site
   . (ToJSON j, MonadYesodExample site m, RedirectUrl site url)
  => url
  -- ^ URL
  -> j
  -- ^ JSON body to send
  -> m ()
putJsonBody = jsonBody "PUT"

-- | Set a DELETE request specified by URL and JSON body
deleteJsonBody
  :: forall j url m site
   . (ToJSON j, MonadYesodExample site m, RedirectUrl site url)
  => url
  -- ^ URL
  -> j
  -- ^ JSON body to send
  -> m ()
deleteJsonBody = jsonBody "DELETE"

-- | Make a GET request specified by URL and query parameters
getParams
  :: forall url m site
   . (MonadYesodExample site m, RedirectUrl site url)
  => url
  -- ^ URL
  -> [(Text, Text)]
  -- ^ Query parameters
  -> m ()
getParams = methodParams "GET"

-- | Make a DELETE request specified by URL and query parameters
deleteParams
  :: forall url m site
   . (MonadYesodExample site m, RedirectUrl site url)
  => url
  -- ^ URL
  -> [(Text, Text)]
  -- ^ Query parameters
  -> m ()
deleteParams = methodParams "DELETE"

-- | Make a request specified by method, URL, and query parameters
methodParams
  :: forall url m site
   . (MonadYesodExample site m, RedirectUrl site url)
  => ByteString
  -- ^ Request method
  -> url
  -- ^ URL
  -> [(Text, Text)]
  -- ^ Query parameters
  -> m ()
methodParams method url params = request $ do
  setMethod method
  setUrl url
  addGetParams params
  jsonHeaders
