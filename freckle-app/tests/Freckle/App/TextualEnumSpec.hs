module Freckle.App.TextualEnumSpec
  ( spec
  ) where

import Freckle.App.Prelude

import Data.Csv qualified as CSV
import Data.List.NonEmpty (NonEmpty ((:|)))
import Freckle.App.Test.Properties.JSON
import Freckle.App.Test.Properties.PathPiece
import Freckle.App.Test.Properties.PersistValue
import Freckle.App.TextualEnum
import Servant
import Test.Hspec
import Test.QuickCheck

data PrimaryColor
  = Red
  | Blue
  | Yellow
  deriving stock (Bounded, Enum, Eq, Generic, Show)

instance EnumValue PrimaryColor where
  toText = \case
    Red -> "red"
    Blue -> "blue"
    Yellow -> "yellow"

data BadExample
  = Okay
  | Conflict1
  | Conflict2
  deriving stock (Bounded, Enum, Eq, Generic, Show)

instance EnumValue BadExample where
  toText = \case
    Okay -> "ok"
    Conflict1 -> "conflict"
    Conflict2 -> "conflict"

spec :: Spec
spec = do
  describe "TextualEnum" $ do
    describe "JSON" $
      it "round trips" $
        property $
          prop_roundTripJSON @(TextualEnum PrimaryColor)

    describe "PathPiece" $
      it "round trips" $
        property $
          prop_roundTripPathPiece @(TextualEnum PrimaryColor)

    describe "HttpApiData" $
      it "round trips" $
        property $ \(e :: TextualEnum PrimaryColor) ->
          parseUrlPiece (toUrlPiece e) == Right e

    describe "PersistValue" $
      it "round trips" $
        property $
          prop_roundTripPersistValue @(TextualEnum PrimaryColor)

    describe "CSV" $
      it "round trips" $
        property $ \(e :: TextualEnum PrimaryColor) ->
          CSV.runParser (CSV.parseField $ CSV.toField e) == Right e

    describe "prop_roundTripEnumText" $ do
      it "holds for a well defined toText" $
        property $
          prop_roundTripEnumText @PrimaryColor

      it "does not hold when toText doesn't map to distinct values" $
        any (not . prop_roundTripEnumText @BadExample) enums

    describe "enums" $ do
      it "returns all values" $
        enums @PrimaryColor
          `shouldBe` fmap TextualEnum (Red :| [Blue, Yellow])
