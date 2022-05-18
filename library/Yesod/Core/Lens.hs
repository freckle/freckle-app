module Yesod.Core.Lens
  ( envL
  , siteL
  ) where

import Freckle.App.Prelude

import Control.Lens (Lens', lens)
import Yesod.Core.Types (HandlerData, RunHandlerEnv, handlerEnv, rheSite)

envL :: Lens' (HandlerData child site) (RunHandlerEnv child site)
envL = lens handlerEnv $ \x y -> x { handlerEnv = y }

siteL :: Lens' (RunHandlerEnv child site) site
siteL = lens rheSite $ \x y -> x { rheSite = y }
