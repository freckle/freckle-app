module Freckle.Test.Yesod.Foundation
  ( TestApp
  , YesodExample
  , YesodExampleData (..)
  , SIO
  , testApp
  , getTestYesod
  )
where

import Freckle.Test.Yesod.MonadYesodExample

import Yesod.Test
  ( SIO
  , TestApp
  , YesodExample
  , YesodExampleData (..)
  , testApp
  )
import qualified Yesod.Test

-- | Get the foundation value used for the current test
getTestYesod :: forall m site. MonadYesodExample site m => m site
getTestYesod = liftYesodExample Yesod.Test.getTestYesod
