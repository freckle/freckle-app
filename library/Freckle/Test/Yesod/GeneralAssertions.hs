module Freckle.Test.Yesod.GeneralAssertions
  ( assertEq
  , assertEqualNoShow
  , assertNotEq
  )
where

import Freckle.App.Prelude
import Freckle.Test.Yesod.MonadYesodExample

import qualified Yesod.Test

-- | Asserts that the two given values are equal
--
-- The error message includes the values.
assertEq
  :: forall a m site
   . (MonadYesodExample site m, Eq a, Show a)
  => String
  -> a
  -> a
  -> m ()
assertEq s a b = liftYesodExample $ Yesod.Test.assertEq s a b

-- | Like 'assertEq' but the error message does not include the values
assertEqualNoShow
  :: forall a m site. (MonadYesodExample site m, Eq a) => String -> a -> a -> m ()
assertEqualNoShow s a b = liftYesodExample $ Yesod.Test.assertEqualNoShow s a b

-- | Asserts that the two given values are not equal
--
-- In case they are equal, the error message includes the values.
assertNotEq
  :: forall a m site
   . (Eq a, Show a, MonadYesodExample site m)
  => String
  -> a
  -> a
  -> m ()
assertNotEq s a b = liftYesodExample $ Yesod.Test.assertNotEq s a b
