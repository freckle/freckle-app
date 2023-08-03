module Freckle.Test.Yesod.RequestBuilder
  ( -- * The RequestBuilder type
    RequestBuilder

    -- * Using a RequestBuilder
  , request

    -- * Constructing RequestBuilders
  , addAcceptLanguage
  , addBasicAuthHeader
  , addFile
  , addGetParams
  , addPostParam
  , addRequestHeader
  , jsonHeaders
  , setLanguage
  , setMethod
  , setRequestBody
  , setUrl
  )
where

import Freckle.App.Prelude
import Freckle.Test.Yesod.MonadYesodExample

import Data.BCP47 (BCP47)
import qualified Data.BCP47 as BCP47
import qualified Data.Text as T
import Network.HTTP.Types.Header (hAccept, hAcceptLanguage, hContentType)
import Yesod.Test
  ( RequestBuilder
  , addBasicAuthHeader
  , addFile
  , addGetParam
  , addPostParam
  , addRequestHeader
  , setMethod
  , setRequestBody
  , setUrl
  )
import qualified Yesod.Test

-- | The general interface for performing requests
--
-- 'request' takes a 'RequestBuilder', constructs a request, and executes it.
--
-- The 'RequestBuilder' allows you to build up attributes of the request,
-- like the headers, parameters, and URL of the request.
request
  :: forall m site. MonadYesodExample site m => RequestBuilder site () -> m ()
request = liftYesodExample . Yesod.Test.request

-- | Set a language for the test Request
--
-- This uses a @_LANG@ query parameter since it's a singleton case, just to
-- exercise that machinery.
setLanguage :: BCP47 -> RequestBuilder site ()
setLanguage = addGetParam "_LANG" . BCP47.toText

-- | Set the @Accept-Language@ header to a list of raw values
--
-- This allows testing with actual quality-factors, etc.
addAcceptLanguage :: [Text] -> RequestBuilder site ()
addAcceptLanguage values =
  addRequestHeader (hAcceptLanguage, encodeUtf8 $ T.intercalate "," values)

-- | Sets both @Content-Type@ and @Accept@ fields to @application/json@
jsonHeaders :: RequestBuilder site ()
jsonHeaders = do
  addRequestHeader (hContentType, "application/json")
  addRequestHeader (hAccept, "application/json")

-- | Repeatedly applies 'addGetParam'
addGetParams :: [(Text, Text)] -> RequestBuilder site ()
addGetParams = traverse_ (uncurry addGetParam)
