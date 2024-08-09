module Freckle.App.AsyncSpec
  ( spec
  ) where

import Freckle.App.Prelude

import Blammo.Logging
import Blammo.Logging.ThreadContext qualified as Blammo
import Control.Concurrent.Async (wait)
import Data.Aeson.KeyMap qualified as KeyMap
import Freckle.App.Async (async)
import OpenTelemetry.Context qualified as OpenTelemetry
import OpenTelemetry.Context.ThreadLocal qualified as OpenTelemetry
import Test.Hspec

spec :: Spec
spec = do
  describe "async" $ do
    it @(IO ()) "preserves Blammo context" $ do
      let kvs = ["a" .= ("xyz" :: Text)]
      Blammo.withThreadContext kvs $
        (wait =<< async Blammo.myThreadContext)
          `shouldReturn` KeyMap.fromList kvs
    it @(IO ()) "preserves OpenTelemetry context" $ do
      k <- OpenTelemetry.newKey "a"
      let v = "xyz" :: Text
      OpenTelemetry.adjustContext $ OpenTelemetry.insert k v
      (wait =<< async (OpenTelemetry.lookup k <$> OpenTelemetry.getContext))
        `shouldReturn` Just v
