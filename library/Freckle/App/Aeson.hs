{-# LANGUAGE CPP #-}

-- | Compatibility module for "Data.Aeson" 1.x vs 2.0
--
-- This should be its own package, but the obvious name (@aeson-compat@) is
-- taken by something old and unrelated. I think that's why no one is doing it
-- yet, including us.
--
module Freckle.App.Aeson
  ( module X
  ) where

import Prelude

-- Blammo provides a lot in its module
import Data.Aeson.Compat as X

-- But it doesn't expose the whole interface we need, so we'll add this too
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.KeyMap as X
#else
import Data.HashMap.Strict as X
#endif

-- Ignored twice because HLint changes the name in some version
{-# ANN module ("HLint: ignore Avoid restricted alias" :: String) #-}
{-# ANN module ("HLint: ignore Avoid restricted qualification" :: String) #-}
