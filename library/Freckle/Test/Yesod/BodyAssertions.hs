module Freckle.Test.Yesod.BodyAssertions
  ( bodyContains
  , bodyEquals
  , bodyNotContains
  )
where

import Freckle.App.Prelude
import Freckle.Test.Yesod.MonadYesodExample

import qualified Yesod.Test

-- | Assert the last response has the given text
--
-- The check is performed using the response body in full text form.
bodyContains :: forall m site. MonadYesodExample site m => String -> m ()
bodyContains = liftYesodExample . Yesod.Test.bodyContains

-- | Assert the last response is exactly equal to the given text
--
-- This is useful for testing API responses.
bodyEquals :: forall m site. MonadYesodExample site m => String -> m ()
bodyEquals = liftYesodExample . Yesod.Test.bodyEquals

-- | Assert the last response doesn't have the given text
--
-- The check is performed using the response body in full text form.
bodyNotContains :: forall m site. MonadYesodExample site m => String -> m ()
bodyNotContains = liftYesodExample . Yesod.Test.bodyNotContains
