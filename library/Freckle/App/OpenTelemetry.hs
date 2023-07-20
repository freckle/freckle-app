module Freckle.App.OpenTelemetry
  ( MonadTracer (..)
  ) where

import Freckle.App.Prelude

import Network.AWS.XRayClient.WAI (XRayVaultData, vaultDataFromRequest)
import Yesod.Core (HandlerFor, waiRequest)

-- | Class for reading 'XRayVaultData'
--
-- This is named the same as the OpenTelemetry class we'll use once we move to
-- that tracing system
class MonadTracer m where
  getVaultData :: m (Maybe XRayVaultData)

instance MonadTracer (HandlerFor app) where
  getVaultData = vaultDataFromRequest <$> waiRequest
