module Freckle.Test.Yesod.JsonAssertions
  ( requireJSONResponse
  , bodyMatchesAsJson
  , bodyArrayShouldSatisfyObjectProperties
  , bodyDeepPermutationAsJson
  , bodyEditEqualsAsJson
  , bodyEqualsAsJson
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
import Test.Hspec.Expectations.Json.Lifted
  ( shouldBeJson
  , shouldBeUnorderedJson
  , shouldMatchJson
  )
import UnliftIO.Exception (catch, throwIO)
import Yesod.Core (FromJSON, ToJSON (toJSON), Value, Yesod)
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

-- | Assert that the response body contains the given JSON value
--
-- Per the documentation for 'shouldMatchJson', this will /not/ fail on extra
-- object keys or on the ordering of array elements.
--
-- Please see 'Test.Hspec.Expectations.Json' for more information on the
-- semantics of each matcher.
bodyMatchesAsJson
  :: forall a m site. (MonadYesodExample site m, HasCallStack, ToJSON a) => a -> m ()
bodyMatchesAsJson v = do
  body <- getJsonBody
  body `shouldMatchJson` toJSON v

bodyEqualsAsJson
  :: forall a m site. (MonadYesodExample site m, HasCallStack, ToJSON a) => a -> m ()
bodyEqualsAsJson = bodyEditEqualsAsJson id

bodyEditEqualsAsJson
  :: forall a m site
   . (MonadYesodExample site m, HasCallStack, ToJSON a)
  => (Value -> Value)
  -> a
  -> m ()
bodyEditEqualsAsJson selector v = do
  body <- selector <$> getJsonBody
  body `shouldBeJson` toJSON v

bodyDeepPermutationAsJson
  :: forall a m site. (MonadYesodExample site m, HasCallStack, ToJSON a) => a -> m ()
bodyDeepPermutationAsJson v = do
  body <- getJsonBody
  body `shouldBeUnorderedJson` toJSON v

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
