module Freckle.App.Test.Hspec.AnnotatedException
  ( unwrapAnnotatedHUnitFailure
  ) where

import Freckle.App.Prelude

import Control.Exception qualified
import Freckle.App.Exception (AnnotatedException (..))
import Test.HUnit.Lang (HUnitFailure)
import Test.Hspec

-- | An hspec hook that lets hspec catch and pretty-print 'HUnitFailure', the
--   exception that is thrown when a test assertion fails
--
-- Tests for any code that might throw 'AnnotatedException' (which includes anything
-- that uses freckle-app) should add this hook to their test suite. Without it, if
-- you end up with an @'AnnotatedException' 'HUnitFailure'@, hspec doesn't recognize
-- it as an assertion failure and you get ugly output instead of nice output.
unwrapAnnotatedHUnitFailure :: Spec -> Spec
unwrapAnnotatedHUnitFailure = around_ $
  Control.Exception.handle $ \AnnotatedException {exception = e} ->
    Control.Exception.throw (e :: HUnitFailure)
