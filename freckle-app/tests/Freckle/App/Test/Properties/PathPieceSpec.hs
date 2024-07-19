module Freckle.App.Test.Properties.PathPieceSpec
  ( spec
  ) where

import Freckle.App.Prelude

import Freckle.App.Test.Properties.PathPiece
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "pathPieceInstancesRoundTrip" $ do
    it "round trips for Integer" $ property $ prop_roundTripPathPiece @Integer
    it "round trips for Bool" $ property $ prop_roundTripPathPiece @Bool
    it "round trips for Maybe String" $
      property $
        prop_roundTripPathPiece @(Maybe String)
