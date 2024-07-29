module Freckle.App.OpenTelemetry.ContextSpec
  ( spec
  ) where

import Relude hiding (traceId)

import AppExample
import Blammo.Logging
import Blammo.Logging.Logger (newTestLogger)
import Control.Lens (lens)
import Data.List qualified as List
import Data.Text qualified as T
import Freckle.App.OpenTelemetry
import Freckle.App.OpenTelemetry.Context
import Network.HTTP.Types.Header (Header)
import OpenTelemetry.Trace.Core qualified as Trace
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Lifted
  ( shouldBe
  , shouldNotBe
  , shouldReturn
  )
import Test.Hspec.Expectations.Lifted qualified as Hspec (expectationFailure)

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
  describe "injectContext" $ do
    it "sets request headers from existing context" $ appExample @App $ do
      inSpan "example" defaultSpanArguments $ do
        spanContext <- assertCurrentSpanContext

        let expectedTraceParent =
              toTraceParent
                (traceIdToHex $ Trace.traceId spanContext)
                (spanIdToHex $ Trace.spanId spanContext)

        injectContext ([] :: [Header])
          `shouldReturn` [ ("traceparent", encodeUtf8 expectedTraceParent)
                         , ("tracestate", "")
                         ]

  describe "extractContext" $ do
    it "sets the context from the headers" $ appExample @App $ do
      let
        traceId = "fba7cd19bff3ef866a599d6f6a85baef"
        spanId = "5747fd6b144f4009"

        headers :: [Header]
        headers =
          [ ("traceparent", encodeUtf8 $ toTraceParent traceId spanId)
          , ("tracestate", "")
          ]

      extractContext headers

      spanContext <- assertCurrentSpanContext
      traceIdToHex (Trace.traceId spanContext) `shouldBe` traceId
      spanIdToHex (Trace.spanId spanContext) `shouldBe` spanId

    it "does nothing with no headers" $ appExample @App $ do
      getCurrentSpanContext `shouldReturn` Nothing

      extractContext ([] :: [Header])

      getCurrentSpanContext `shouldReturn` Nothing

  describe "processWithContext" $ do
    it "creates a child-span context" $ appExample @App $ do
      inSpan "example" defaultSpanArguments $ do
        outerSpanContext <- assertCurrentSpanContext

        let
          traceId = traceIdToHex $ Trace.traceId outerSpanContext
          spanId = spanIdToHex $ Trace.spanId outerSpanContext

          headers :: [Header]
          headers =
            [ ("traceparent", encodeUtf8 $ toTraceParent traceId spanId)
            , ("tracestate", "")
            ]

        processWithContext "loop" defaultSpanArguments headers $ \_ -> do
          innerSpanContext <- assertCurrentSpanContext

          -- Ideally we'd assert the inner span's parent is the outer span, but
          -- the library doesn't expose the necessary functionality. All we can
          -- do is assert it's a fresh span of the same trace.
          Trace.traceId innerSpanContext `shouldBe` Trace.traceId outerSpanContext
          Trace.spanId innerSpanContext `shouldNotBe` Trace.spanId outerSpanContext

    it "sets the current context back into the headers" $ appExample @App $ do
      inSpan "example" defaultSpanArguments $ do
        let
          traceId = "fba7cd19bff3ef866a599d6f6a85baef"
          spanId = "5747fd6b144f4009"

          headers :: [Header]
          headers =
            [ ("traceparent", encodeUtf8 $ toTraceParent traceId spanId)
            , ("tracestate", "")
            ]

        processWithContext "loop" defaultSpanArguments headers $ \headers' -> do
          spanContext <- assertCurrentSpanContext

          let headerTraceParent = do
                bs <- List.lookup "traceparent" headers'
                fromTraceParent $ decodeUtf8 bs

          fmap fst headerTraceParent `shouldBe` Just traceId
          fmap snd headerTraceParent
            `shouldBe` Just (spanIdToHex $ Trace.spanId spanContext)

    it "sets a fresh context back into the headers" $ appExample @App $ do
      inSpan "example" defaultSpanArguments $ do
        let
          headers :: [Header]
          headers = []

        processWithContext "loop" defaultSpanArguments headers $ \headers' -> do
          spanContext <- assertCurrentSpanContext

          let headerTraceParent = do
                bs <- List.lookup "traceparent" headers'
                fromTraceParent $ decodeUtf8 bs

          fmap fst headerTraceParent
            `shouldBe` Just (traceIdToHex $ Trace.traceId spanContext)
          fmap snd headerTraceParent
            `shouldBe` Just (spanIdToHex $ Trace.spanId spanContext)

assertCurrentSpanContext :: (MonadIO m, HasCallStack) => m Trace.SpanContext
assertCurrentSpanContext =
  getCurrentSpanContext >>= \case
    Nothing -> expectationFailure "Expect there to be a SpanContext"
    Just spanContext -> pure spanContext

toTraceParent :: Text -> Text -> Text
toTraceParent traceId spanId = "00-" <> traceId <> "-" <> spanId <> "-00"

fromTraceParent :: Text -> Maybe (Text, Text)
fromTraceParent a = do
  b <- T.stripPrefix "00-" a
  c <- T.stripSuffix "-00" b
  [traceId, spanId] <- Just $ T.splitOn "-" c
  pure (traceId, spanId)

expectationFailure :: (MonadIO m, HasCallStack) => String -> m a
expectationFailure msg = Hspec.expectationFailure msg >> error "unreachable"
