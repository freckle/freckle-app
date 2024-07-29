-- | Application tracing via <https://opentelemetry.io/>
--
-- @
-- data App = App
--   { -- ...
--   , appTracer :: Tracer
--   }
--
-- instance 'HasTracer' App where
--   tracerL = lens appTracer $ \x y -> x { appTracer = y }
--
-- loadApp f = do
--   -- ...
--   'withTracerProvider' $ \tracerProvider -> do
--     let appTracer = 'makeTracer' tracerProvider "my-app" 'tracerOptions'
--     f App {..}
-- @
--
-- You may need to do this even if you don't plan to manually trace things, in
-- order to satisfy the 'MonadTracer' constraint required by functions like
-- 'runDB'. If you don't need this feature, and don't plan on running an
-- otel-collector, set @OTEL_TRACES_EXPORTER=none@ in the environment, which
-- makes all tracing a no-op.
--
-- In the future, it should be possible to use @OTEL_SDK_DISABLED@ for the same
-- purpose. See <https://github.com/iand675/hs-opentelemetry/issues/60>.
module Freckle.App.OpenTelemetry
  ( HasTracer (..)
  , Tracer

    -- * Effects
  , MonadTracer (..)
  , inSpan
  , SpanArguments (..)
  , defaultSpanArguments
  , serverSpanArguments
  , clientSpanArguments
  , producerSpanArguments
  , consumerSpanArguments

    -- * Querying
  , getCurrentTraceId
  , getCurrentSpanContext

    -- * Ids
  , TraceId
  , traceIdToHex
  , SpanId
  , spanIdToHex

    -- * Attributes
  , ToAttribute (..)
  , addCurrentSpanAttributes

    -- * Setup
  , withTracerProvider

    -- ** 'Tracer'
  , makeTracer
  , tracerOptions

    -- ** Utilities
  , byteStringToAttribute
  , attributeValueLimit
  ) where

import Relude

import Data.Text qualified as T
import OpenTelemetry.Context (lookupSpan)
import OpenTelemetry.Context.ThreadLocal (getContext)
import OpenTelemetry.Trace hiding (inSpan)
import OpenTelemetry.Trace.Core (getSpanContext)
import OpenTelemetry.Trace.Core qualified as Trace (SpanContext (..))
import OpenTelemetry.Trace.Id
  ( Base (..)
  , SpanId
  , TraceId
  , spanIdBaseEncodedText
  , traceIdBaseEncodedText
  )
import OpenTelemetry.Trace.Monad
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket)

-- | 'defaultSpanArguments' with 'kind' set to 'Server'
--
-- Indicates that the span covers server-side handling of a synchronous RPC or
-- other remote request. This span is the child of a remote @Client@ span that
-- was expected to wait for a response.
serverSpanArguments :: SpanArguments
serverSpanArguments = defaultSpanArguments {kind = Server}

-- | 'defaultSpanArguments' with 'kind' set to 'Kind'
--
-- Indicates that the span describes a synchronous request to some remote
-- service. This span is the parent of a remote @Server@ span and waits for its
-- response.
clientSpanArguments :: SpanArguments
clientSpanArguments = defaultSpanArguments {kind = Client}

-- | 'defaultSpanArguments' with 'kind' set to 'Producer'
--
-- Indicates that the span describes the parent of an asynchronous request. This
-- parent span is expected to end before the corresponding child @Producer@
-- span, possibly even before the child span starts. In messaging scenarios with
-- batching, tracing individual messages requires a new @Producer@ span per
-- message to be created.
producerSpanArguments :: SpanArguments
producerSpanArguments = defaultSpanArguments {kind = Producer}

-- | 'defaultSpanArguments' with 'kind' set to 'Consumer'
--
-- Indicates that the span describes the child of an asynchronous @Producer@
-- request.
consumerSpanArguments :: SpanArguments
consumerSpanArguments = defaultSpanArguments {kind = Consumer}

withTracerProvider :: MonadUnliftIO m => (TracerProvider -> m a) -> m a
withTracerProvider =
  bracket
    (liftIO initializeGlobalTracerProvider)
    (liftIO . shutdownTracerProvider)

getCurrentTraceId :: MonadIO m => m (Maybe TraceId)
getCurrentTraceId = fmap Trace.traceId <$> getCurrentSpanContext

getCurrentSpanContext :: MonadIO m => m (Maybe SpanContext)
getCurrentSpanContext = withCurrentSpan getSpanContext

addCurrentSpanAttributes :: MonadIO m => HashMap Text Attribute -> m ()
addCurrentSpanAttributes attrs = void $ withCurrentSpan (`addAttributes` attrs)

withCurrentSpan :: MonadIO m => (Span -> m b) -> m (Maybe b)
withCurrentSpan f = do
  mSpan <- lookupSpan <$> getContext
  traverse f mSpan

traceIdToHex :: TraceId -> Text
traceIdToHex = traceIdBaseEncodedText Base16

spanIdToHex :: SpanId -> Text
spanIdToHex = spanIdBaseEncodedText Base16

-- | Convert a 'ByteString' to an 'Attribute' safely
--
-- - Decodes it as UTF-8 leniently,
-- - Truncates to fit within 'attributeValueLimit'
byteStringToAttribute :: ByteString -> Attribute
byteStringToAttribute =
  toAttribute
    . truncateText attributeValueLimit
    . decodeUtf8With lenientDecode

-- | Character limit for 'Attribute' values
--
-- OTel the spec doesn't specify a limit, but says that SDKs should decide
-- some limit. It's not clear what the Haskell SDK does, if anything. New
-- Relic applies a limit of 4095 characters on all metrics it handles,
-- including those coming from OTel. Seems reasonable enough.
--
-- <https://docs.newrelic.com/docs/more-integrations/open-source-telemetry-integrations/opentelemetry/best-practices/opentelemetry-best-practices-attributes/>
attributeValueLimit :: Int
attributeValueLimit = 4095

truncateText :: Int -> Text -> Text
truncateText l t
  | T.length t <= l = t
  | otherwise = T.take (l - 3) t <> "..."
