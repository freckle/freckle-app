module Freckle.App.OpenTelemetry.TraceEnvelopeSpec
  ( spec
  ) where

import Freckle.App.Test

import Blammo.Logging
import Blammo.Logging.Logger (newTestLogger)
import Control.Lens (lens, (^?))
import Data.Aeson
import Data.Aeson.Lens
import Freckle.App.OpenTelemetry
import Freckle.App.OpenTelemetry.TraceEnvelope (traceHeadersKey)
import qualified Freckle.App.OpenTelemetry.TraceEnvelope as TraceEnvelope
import qualified OpenTelemetry.Trace.Core as Trace

data App = App
  { appLogger :: Logger
  , appTracer :: Tracer
  }

instance HasLogger App where
  loggerL = lens appLogger $ \x y -> x {appLogger = y}

instance HasTracer App where
  tracerL = lens appTracer $ \x y -> x {appTracer = y}

loadApp :: (App -> IO ()) -> IO ()
loadApp f = do
  appLogger <- newTestLogger defaultLogSettings

  withTracerProvider $ \tracerProvider -> do
    let appTracer = makeTracer tracerProvider "app" tracerOptions
    f App {..}

spec :: Spec
spec = withApp loadApp $ do
  describe "encodeStrict" $ do
    it "adds request headers from existing context" $ appExample $ do
      inSpan "example" defaultSpanArguments $ do
        spanContext <- assertCurrentSpanContext

        let
          exampleValue :: Value
          exampleValue = object ["foo" .= True]

          expectedTraceParent =
            toTraceParent
              (traceIdToHex $ Trace.traceId spanContext)
              (spanIdToHex $ Trace.spanId spanContext)

          expectedHeaders :: [(Text, Text)]
          expectedHeaders =
            [ ("traceparent", expectedTraceParent)
            , ("tracestate", "")
            ]

        bs <- TraceEnvelope.encodeStrict exampleValue
        bs ^? key traceHeadersKey . _JSON `shouldBe` Just expectedHeaders
        bs ^? key "foo" . _Bool `shouldBe` Just True

    it "does not change non-Object values" $ appExample $ do
      inSpan "example" defaultSpanArguments $ do
        let
          exampleValue :: Value
          exampleValue = String "foo"

        bs <- TraceEnvelope.encodeStrict exampleValue
        bs ^? _String `shouldBe` Just "foo"

  describe "eitherDecodeStrict" $ do
    it "sets and removes the context from the envelope" $ appExample $ do
      (bs, parentTraceId) <- inSpan "parent" defaultSpanArguments $ do
        spanContext <- assertCurrentSpanContext

        let
          exampleValue :: Value
          exampleValue = object ["foo" .= True]

        bs <- TraceEnvelope.encodeStrict exampleValue
        pure (bs, Trace.traceId spanContext)

      inSpan "child" defaultSpanArguments $ do
        result <- TraceEnvelope.eitherDecodeStrict @_ @Value bs

        case result of
          Left err -> expectationFailure err
          Right decoded -> do
            decoded ^? key "foo" . _Bool `shouldBe` Just True
            decoded ^? key traceHeadersKey . _JSON `shouldBe` (Nothing :: Maybe Value)

        spanContext <- assertCurrentSpanContext
        Trace.traceId spanContext `shouldBe` parentTraceId

assertCurrentSpanContext :: (MonadIO m, HasCallStack) => m Trace.SpanContext
assertCurrentSpanContext =
  getCurrentSpanContext >>= \case
    Nothing -> expectationFailure "Expect there to be a SpanContext"
    Just spanContext -> pure spanContext

toTraceParent :: Text -> Text -> Text
toTraceParent traceId spanId = "00-" <> traceId <> "-" <> spanId <> "-00"
