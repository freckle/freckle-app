module Freckle.App.Test.Properties.PathPiece
  ( prop_roundTripPathPiece
  ) where

import Freckle.App.Prelude

import Web.PathPieces

-- | Check that @fromPathPiece (toPathPiece value)@ is @value@
prop_roundTripPathPiece :: (PathPiece a, Eq a) => a -> Bool
prop_roundTripPathPiece a = fromPathPiece (toPathPiece a) == Just a
