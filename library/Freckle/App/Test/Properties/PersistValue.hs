module Freckle.App.Test.Properties.PersistValue
  ( prop_roundTripPersistValue
  ) where

import Freckle.App.Prelude

import Database.Persist (PersistField (..))

-- | Check that @fromPersistValue (toPersistValue value)@ is @value@
prop_roundTripPersistValue :: (PersistField a, Eq a) => a -> Bool
prop_roundTripPersistValue a = fromPersistValue (toPersistValue a) == Right a
