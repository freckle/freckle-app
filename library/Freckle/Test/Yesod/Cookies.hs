module Freckle.Test.Yesod.Cookies
  ( getRequestCookies
  , testSetCookie
  , testDeleteCookie
  , testClearCookies
  )
where

import Freckle.App.Prelude
import Freckle.Test.Yesod.MonadYesodExample

import Data.ByteString (ByteString)
import Web.Cookie (SetCookie)
import Yesod.Test (getRequestCookies)
import qualified Yesod.Test

-- | Clears the current cookies
testClearCookies :: forall m site. MonadYesodExample site m => m ()
testClearCookies = liftYesodExample $ Yesod.Test.testClearCookies

-- | Deletes the cookie of the given name
testDeleteCookie
  :: forall m site. MonadYesodExample site m => ByteString -> m ()
testDeleteCookie = liftYesodExample . Yesod.Test.testDeleteCookie

-- | Sets a cookie
testSetCookie :: forall m site. MonadYesodExample site m => SetCookie -> m ()
testSetCookie = liftYesodExample . Yesod.Test.testSetCookie
