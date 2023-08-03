module Freckle.Test.Yesod.SResponse
  ( SResponse (..)
  , getResponse
  , withResponse
  )
where

import Freckle.App.Prelude
import Freckle.Test.Yesod.MonadYesodExample

import Network.Wai.Test (SResponse (..))
import Yesod.Test (withResponse)
import qualified Yesod.Test

-- | Get the most recently provided response value, if available
getResponse :: forall m site. MonadYesodExample site m => m (Maybe SResponse)
getResponse = liftYesodExample Yesod.Test.getResponse
