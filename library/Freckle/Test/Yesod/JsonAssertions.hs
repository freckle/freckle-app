module Freckle.Test.Yesod.JsonAssertions
  ( requireJSONResponse
  , bodyArrayShouldSatisfyObjectProperties
  )
where

import Freckle.App.Prelude
import Freckle.Test.Yesod.GetBody
import Freckle.Test.Yesod.MonadYesodExample

import Control.Lens ((^?))
import qualified Data.Aeson.Key as Key
import Data.Aeson.Lens (key)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Test.HUnit.Lang
  ( FailureReason (..)
  , HUnitFailure (..)
  , formatFailureReason
  )
import UnliftIO.Exception (catch, throwIO)
import Yesod.Core (FromJSON, Value, Yesod)
import Yesod.Test (YesodExample)
import qualified Yesod.Test

-- | Parses the response body from JSON into a Haskell value,
--   throwing an error if parsing fails
--
-- This function also checks that the @Content-Type@ of the response
-- is @application/json@.
requireJSONResponse
  :: forall a m site. (MonadYesodExample site m, FromJSON a) => m a
requireJSONResponse = liftYesodExample Yesod.Test.requireJSONResponse

bodyArrayShouldSatisfyObjectProperties
  :: (HasCallStack, Yesod site)
  => [(Text, Maybe Value -> YesodExample site ())]
  -> YesodExample site ()
bodyArrayShouldSatisfyObjectProperties predicates = do
  values <- NE.toList <$> getJsonBody @(NonEmpty Value)

  for_ ((,) <$> values <*> predicates) $ \(value, (name, predicate)) -> do
    predicate (value ^? key (Key.fromText name)) `orScream` T.unpack name

orScream
  :: (HasCallStack, MonadUnliftIO m) => (HasCallStack => m a) -> String -> m a
orScream action message =
  action `catch` \(HUnitFailure l r) ->
    throwIO . HUnitFailure l . Reason $ message ++ "\n\n" ++ formatFailureReason r

infix 0 `orScream`
