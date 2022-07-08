{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Freckle.App.Test.CommonTests
  ( jsonInstancesRoundTrip
  , pathPieceInstancesRoundTrip
  ) where

import Freckle.App.Prelude

import Data.Aeson
import Test.Hspec
import Test.QuickCheck hiding (Success)
import Web.PathPieces

-- | Property test that @fromJSON (toJSON value)@ is @value@
jsonInstancesRoundTrip
  :: forall a
   . (Arbitrary a, ToJSON a, FromJSON a, Show a, Eq a)
  => SpecWith ()
jsonInstancesRoundTrip =
  describe "JSON instances"
    $ it "are inverses (serialization then deserialization round-trips)"
    $ property
    $ \(value :: a) -> fromJSON (toJSON value) `shouldBe` Success value

-- | Property test that @fromPathPiece (toPathPiece value)@ is @value@
pathPieceInstancesRoundTrip
  :: forall a . (Arbitrary a, PathPiece a, Show a, Eq a) => SpecWith ()
pathPieceInstancesRoundTrip =
  describe "PathPiece instances"
    $ it "are inverses (serialization then deserialization round-trips)"
    $ property
    $ \(value :: a) -> fromPathPiece (toPathPiece value) `shouldBe` Just value
