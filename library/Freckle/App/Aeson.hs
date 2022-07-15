{-# LANGUAGE CPP #-}

-- | Compatibility module for "Data.Aeson" 1.x vs 2.0
--
-- TODO: An @aeson-compat@ package. Sadly, the name is taken.
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

{-# ANN module ("HLint: ignore Avoid restricted qualification" :: String) #-}
