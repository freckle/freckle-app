module Freckle.App.Test.Properties.JSON
  ( prop_roundTripJSON
  ) where

import Freckle.App.Prelude

import Data.Aeson

-- | Check that @fromJSON (toJSON value)@ is @value@
prop_roundTripJSON :: (Eq a, FromJSON a, ToJSON a) => a -> Bool
prop_roundTripJSON a = fromJSON (toJSON a) == Success a
