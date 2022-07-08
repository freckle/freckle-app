{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Freckle.App.Test.CommonTests
  ( jsonInstancesRoundTrip
  ) where

import Freckle.App.Prelude

import Data.Aeson
import Test.Hspec
import Test.QuickCheck hiding (Success)

-- | Property test to check that @fromJSON (toJSON value)@ returns @value@
jsonInstancesRoundTrip
  :: forall a
   . (Arbitrary a, ToJSON a, FromJSON a, Show a, Eq a)
  => SpecWith ()
jsonInstancesRoundTrip =
  describe "JSON instances"
    $ it "are inverses (serialization then deserialization round-trips)"
    $ property
    $ \(value :: a) -> fromJSON (toJSON value) `shouldBe` Success value
