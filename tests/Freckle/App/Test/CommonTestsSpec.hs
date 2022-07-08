module Freckle.App.Test.CommonTestsSpec
  ( spec
  ) where

import Freckle.App.Prelude

import Data.Aeson
import Freckle.App.Test.CommonTests
import Test.Hspec
import Test.QuickCheck

data MyCustomType
  = A
  | B (Maybe String)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Arbitrary MyCustomType where
  arbitrary = oneof [pure A, B <$> arbitrary]

spec :: Spec
spec = do
  describe "jsonInstancesRoundTrip" $ do
    describe "Integer" $ jsonInstancesRoundTrip @Integer
    describe "Some custom type" $ jsonInstancesRoundTrip @MyCustomType
