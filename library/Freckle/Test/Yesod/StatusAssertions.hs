module Freckle.Test.Yesod.StatusAssertions
  ( statusIs

    -- * Expect success
  , getOk
  , postOk
  , patchOk
  , deleteOk

    -- * Expect error
  , getErr
  , postErr
  , patchErr
  , deleteErr
  )
where

import Freckle.App.Prelude
import Freckle.Test.Yesod.MonadYesodExample
import Freckle.Test.Yesod.RequestBuilder

import Data.ByteString (ByteString)
import Test.Hspec.Expectations.Lifted (expectationFailure)
import Yesod.Core (RedirectUrl)
import qualified Yesod.Test

-- | Assert the last response status is as expected
--
-- If the status code doesn't match, a portion of the body is also
-- printed to aid in debugging.
statusIs
  :: forall m site. (HasCallStack, MonadYesodExample site m) => Int -> m ()
statusIs = liftYesodExample . Yesod.Test.statusIs

-- | Make a GET request (specified only by URL)
--   and assert that the result has 200 (ok) status
getOk
  :: forall url m site
   . (HasCallStack, MonadYesodExample site m, RedirectUrl site url)
  => url
  -> m ()
getOk = methodOk "GET"

-- | Make a DELETE request (specified only by URL)
--   and assert that the result has 200 (ok) status
deleteOk
  :: forall url m site
   . (HasCallStack, MonadYesodExample site m, RedirectUrl site url)
  => url
  -> m ()
deleteOk = methodOk "DELETE"

-- | Make a POST request (specified only by URL)
--   and assert that the result has 200 (ok) status
postOk
  :: forall url m site
   . (HasCallStack, MonadYesodExample site m, RedirectUrl site url)
  => url
  -> m ()
postOk = methodOk "POST"

-- | Make a PATCH request (specified only by URL)
--   and assert that the result has 200 (ok) status
patchOk
  :: forall url m site
   . (HasCallStack, MonadYesodExample site m, RedirectUrl site url)
  => url
  -> m ()
patchOk = methodOk "PATCH"

-- | Make a GET request to the given URL and assert that the
--   result has a particular __error__ status
getErr
  :: forall url m site
   . (HasCallStack, MonadYesodExample site m, RedirectUrl site url)
  => Int
  -- ^ Expected error status; must not be in the 200 range
  -> url
  -- ^ URL
  -> m ()
getErr = methodErr "GET"

-- | Make a DELETE request to the given URL and assert that the
--   result has a particular __error__ status
deleteErr
  :: forall url m site
   . (HasCallStack, MonadYesodExample site m, RedirectUrl site url)
  => Int
  -- ^ Expected error status; must not be in the 200 range
  -> url
  -- ^ URL
  -> m ()
deleteErr = methodErr "DELETE"

-- | Make a POST request to the given URL and assert that the
--   result has a particular __error__ status
postErr
  :: forall url m site
   . (HasCallStack, MonadYesodExample site m, RedirectUrl site url)
  => Int
  -- ^ Expected error status; must not be in the 200 range
  -> url
  -- ^ URL
  -> m ()
postErr = methodErr "POST"

-- | Make a PATCH request to the given URL and assert that the
--   result has a particular __error__ status
patchErr
  :: forall url m site
   . (HasCallStack, MonadYesodExample site m, RedirectUrl site url)
  => Int
  -- ^ Expected error status; must not be in the 200 range
  -> url
  -- ^ URL
  -> m ()
patchErr = methodErr "PATCH"

-- | Make a request (specified by method and URL, with no body)
--   and assert that the result has 200 (ok) status
methodOk
  :: forall url m site
   . (HasCallStack, MonadYesodExample site m, RedirectUrl site url)
  => ByteString
  -- ^ Request method
  -> url
  -- ^ URL
  -> m ()
methodOk method = methodBase method 200

-- | Make a request (specified by method and URL, with no body)
--   and assert that the result has a particular __error__ status
methodErr
  :: forall url m site
   . (HasCallStack, MonadYesodExample site m, RedirectUrl site url)
  => ByteString
  -- ^ Request method
  -> Int
  -- ^ Expected error status; must not be in the 200 range
  -> url
  -- ^ URL
  -> m ()
methodErr method expectedStatus url
  | okStatus = expectationFailure errMsg
  | otherwise = methodBase method expectedStatus url
 where
  okStatus = expectedStatus >= 200 && expectedStatus < 300
  errMsg = "methodErr called with status that does not represent error."

-- | Make a request (specified by method and URL, with no body)
--   and assert that the result has a particular status
methodBase
  :: forall url m site
   . (HasCallStack, MonadYesodExample site m, RedirectUrl site url)
  => ByteString
  -- ^ Request method
  -> Int
  -- ^ Expected status
  -> url
  -- ^ URL
  -> m ()
methodBase method expectedStatus url = do
  request $ do
    setUrl url
    setMethod method
    jsonHeaders
  statusIs expectedStatus
