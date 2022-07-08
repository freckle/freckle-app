module Freckle.App.Test.Properties.JSON
  ( prop_roundTripJSON
  ) where

import Freckle.App.Prelude

import Data.Aeson

-- | Check that @fromJSON (toJSON value)@ is @value@
prop_roundTripJSON :: (FromJSON a, ToJSON a, Eq a) => a -> Bool
prop_roundTripJSON a = fromJSON (toJSON a) == Success a
