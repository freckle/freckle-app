{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Similar to "Yesod.Test" from the yesod-test package
--
-- Actions in the 'YesodExample' monad are generalized to a
-- 'MonadYesodExample' constraint, allowing tests to be written
-- in custom monads more easily.
module Freckle.App.Test.Yesod
  ( -- * Monad class
    MonadYesodExample (..)

    -- * Making requests

    -- ** Via RequestBuilder
  , request
  , RequestBuilder
  , setMethod
  , setUrl
  , setRequestBody
  , addGetParam
  , addPostParam
  , addRequestHeader
  , addJsonHeaders
  , setLanguage
  , addAcceptLanguage
  , addFile

    -- ** Other ways
  , get
  , post
  , followRedirect

    -- * Inspecting the response

    -- ** Getting the body
  , getRawBody
  , getCsvBody
  , getJsonBody

    -- ** Dealing with the response
  , getResponse
  , withResponse
  , SResponse (..)

    -- * Assertions

    -- ** Status
  , statusIs
  , statusIs2XX

    -- ** Header fields
  , assertHeader
  , assertHeaderContains
  , assertHeaderSatisfies

    -- ** Body
  , bodyContains

    -- * Cookies
  , getRequestCookies
  , testSetCookie
  , testDeleteCookie
  , testClearCookies

    -- * Foundational details
  , SIO
  , TestApp
  , YesodExample
  , YesodExampleData (..)
  , getTestYesod
  )
where

import Freckle.App.Prelude

import Blammo.Logging.Setup (LoggingT)
import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Validate (ValidateT)
import Data.Aeson (FromJSON)
import Data.BCP47 (BCP47)
import Data.BCP47 qualified as BCP47
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.CaseInsensitive (CI)
import Data.Csv qualified as CSV
import Data.Text qualified as T
import Data.Vector qualified as V
import Freckle.App.Http (statusIsSuccessful)
import Freckle.App.Test (expectationFailure)
import Network.HTTP.Types.Header (hAccept, hAcceptLanguage, hContentType)
import Network.Wai.Test (SResponse (..))
import Test.HUnit qualified as HUnit
import Web.Cookie (SetCookie)
import Yesod.Core (RedirectUrl, Yesod)
import Yesod.Test
  ( RequestBuilder
  , SIO
  , TestApp
  , YesodExample
  , YesodExampleData (..)
  , addFile
  , addGetParam
  , addPostParam
  , addRequestHeader
  , getRequestCookies
  , setMethod
  , setRequestBody
  , setUrl
  , withResponse
  )
import Yesod.Test qualified
import Yesod.Test.Internal (getBodyTextPreview)

class (MonadIO m, Yesod site) => MonadYesodExample site m | m -> site where
  liftYesodExample :: YesodExample site a -> m a

instance Yesod site => MonadYesodExample site (YesodExample site) where
  liftYesodExample = id

instance MonadYesodExample site m => MonadYesodExample site (StateT s m) where
  liftYesodExample = lift . liftYesodExample

instance MonadYesodExample site m => MonadYesodExample site (ReaderT r m) where
  liftYesodExample = lift . liftYesodExample

instance MonadYesodExample site m => MonadYesodExample site (ValidateT e m) where
  liftYesodExample = lift . liftYesodExample

instance MonadYesodExample site m => MonadYesodExample site (MaybeT m) where
  liftYesodExample = lift . liftYesodExample

instance MonadYesodExample site m => MonadYesodExample site (ExceptT e m) where
  liftYesodExample = lift . liftYesodExample

instance MonadYesodExample site m => MonadYesodExample site (ResourceT m) where
  liftYesodExample = lift . liftYesodExample

instance MonadYesodExample site m => MonadYesodExample site (LoggingT m) where
  liftYesodExample = lift . liftYesodExample

-- | Assert the last response has the given text
--
-- The check is performed using the response body in full text form.
bodyContains :: forall m site. MonadYesodExample site m => String -> m ()
bodyContains = liftYesodExample . Yesod.Test.bodyContains

-- | Clears the current cookies
testClearCookies :: forall m site. MonadYesodExample site m => m ()
testClearCookies = liftYesodExample Yesod.Test.testClearCookies

-- | Deletes the cookie of the given name
testDeleteCookie
  :: forall m site. MonadYesodExample site m => ByteString -> m ()
testDeleteCookie = liftYesodExample . Yesod.Test.testDeleteCookie

-- | Sets a cookie
testSetCookie :: forall m site. MonadYesodExample site m => SetCookie -> m ()
testSetCookie = liftYesodExample . Yesod.Test.testSetCookie

-- | Get the body of the most recent response and decode it as JSON
getJsonBody
  :: forall a m site. (MonadYesodExample site m, FromJSON a, HasCallStack) => m a
getJsonBody = liftYesodExample Yesod.Test.requireJSONResponse

-- | Get the body of the most recent response and decode it as CSV
getCsvBody
  :: forall a m site
   . (MonadYesodExample site m, CSV.FromNamedRecord a, HasCallStack)
  => m [a]
getCsvBody =
  liftYesodExample $
    withResponse $ \(SResponse _status _headers body) ->
      -- todo - check the response header first
      case fmap (V.toList . snd) (CSV.decodeByName body) of
        Left err ->
          failure $
            T.concat
              [ "Failed to parse CSV response; error: "
              , T.pack err
              , "CSV: "
              , getBodyTextPreview body
              ]
        Right v -> pure v
 where
  failure reason = do
    _ <- liftIO $ HUnit.assertFailure $ T.unpack reason
    error ""

-- | Get the body of the most recent response as a byte string
getRawBody
  :: forall m site. (MonadYesodExample site m, HasCallStack) => m BSL.ByteString
getRawBody =
  fmap simpleBody . maybe (expectationFailure "Test response had no body") pure
    =<< getResponse

-- | Get the most recently provided response value, if available
getResponse :: forall m site. MonadYesodExample site m => m (Maybe SResponse)
getResponse = liftYesodExample Yesod.Test.getResponse

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
addJsonHeaders :: RequestBuilder site ()
addJsonHeaders = do
  addRequestHeader (hContentType, "application/json")
  addRequestHeader (hAccept, "application/json")

-- | Assert the last response status is as expected
--
-- If the status code doesn't match, a portion of the body is also
-- printed to aid in debugging.
statusIs
  :: forall m site. (MonadYesodExample site m, HasCallStack) => Int -> m ()
statusIs = liftYesodExample . Yesod.Test.statusIs

statusIs2XX :: MonadYesodExample site m => m ()
statusIs2XX = liftYesodExample $ withResponse $ \response -> do
  let status = simpleStatus response
  unless (statusIsSuccessful status) $ do
    expectationFailure $
      "Response status " <> show status <> " should have been 2xx"

-- | Assert that the given header field's value satisfied some predicate
assertHeaderSatisfies
  :: forall m site
   . MonadYesodExample site m
  => CI ByteString
  -- ^ Field name
  -> String
  -- ^ Some description of the predicate; this is included
  --   in the error message if the assertion fails
  -> (ByteString -> Bool)
  -- ^ Predicate applied to the field value which is expected
  --   to return 'True'
  -> m ()
assertHeaderSatisfies header predicateDesc predicate = liftYesodExample $ withResponse $ \res ->
  case lookup header $ simpleHeaders res of
    Just value | predicate value -> pure ()
    Just value ->
      expectationFailure $
        concat
          [ "Expected header "
          , show header
          , " "
          , predicateDesc
          , ", but received "
          , show value
          ]
    Nothing ->
      expectationFailure $
        concat
          [ "Expected header "
          , show header
          , predicateDesc
          , ", but it was not present"
          ]

-- | Assert that the given header field's value contains
--   some particular byte string within it
assertHeaderContains
  :: MonadYesodExample site m
  => CI ByteString
  -- ^ Field name
  -> ByteString
  -- ^ Substring that we expect to find anywhere within the field value
  -> m ()
assertHeaderContains header substring =
  assertHeaderSatisfies
    header
    ("to contain " <> show substring)
    (substring `BS.isInfixOf`)

-- | Assert the given header key/value pair was returned
assertHeader
  :: forall m site
   . MonadYesodExample site m
  => CI ByteString
  -- ^ Field name
  -> ByteString
  -- ^ Expected field value
  -> m ()
assertHeader k v = liftYesodExample $ Yesod.Test.assertHeader k v

-- | Follow a redirect, if the last response was a redirect
followRedirect
  :: forall m site
   . MonadYesodExample site m
  => m (Either Text Text)
  -- ^ Left with an error message if not a redirect,
  --   Right with the redirected URL if it was
followRedirect = liftYesodExample Yesod.Test.followRedirect

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

-- | Get the foundation value used for the current test
getTestYesod :: forall m site. MonadYesodExample site m => m site
getTestYesod = liftYesodExample Yesod.Test.getTestYesod
