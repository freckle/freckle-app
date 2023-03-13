{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Network.Wai.Middleware.OpenTelemetrySpec
  ( spec
  ) where

import Freckle.App.Prelude

import Network.Wai.Middleware.OpenTelemetry (ddTraceId)
import OpenTelemetry.Trace.Id (Base(Base16), baseEncodedToTraceId)
import Test.Hspec

spec :: Spec
spec = do
  describe "ddTraceId" $ do
    it "converts correctly to Word64" $ do
      let
        Right traceId =
          baseEncodedToTraceId Base16 "4e2ef0fa8943f417f6209f2b6f6ddcea"
      -- Example taken from a real test trace
      ddTraceId traceId `shouldBe` 17735350341486894314
