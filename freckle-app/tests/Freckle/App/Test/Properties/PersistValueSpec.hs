module Freckle.App.Test.Properties.PersistValueSpec
  ( spec
  ) where

import Freckle.App.Prelude

import Freckle.App.Test.Properties.PersistValue
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "persistFieldInstancesRoundTrip" $ do
    it "round trips for Integer" $ property $ prop_roundTripPersistValue @Int
    it "round trips for Bool" $ property $ prop_roundTripPersistValue @Bool
    it "round trips for Maybe String" $
      property $
        prop_roundTripPersistValue @(Maybe String)
