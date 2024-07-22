module Freckle.App.Test.Properties.JSONSpec
  ( spec
  ) where

import Freckle.App.Prelude

import Data.Aeson
import Freckle.App.Test.Properties.JSON
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
  describe "prop_roundTripJSON" $ do
    it "round trips for Integer" $ property $ prop_roundTripJSON @Integer
    it "round trips for Bool" $ property $ prop_roundTripJSON @Bool
    it "round trips for some custom type" $
      property $
        prop_roundTripJSON @MyCustomType
