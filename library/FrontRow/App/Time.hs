-- | Time types and functions for @App@
module FrontRow.App.Time
  ( Seconds(..)
  ) where

import Prelude

newtype Seconds = Seconds { unSeconds :: Int }
  deriving stock (Show, Read)
  deriving newtype (Eq, Num)
