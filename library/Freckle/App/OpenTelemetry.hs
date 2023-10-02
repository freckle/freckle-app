-- | Application tracing via <https://opentelemetry.io/>
--
-- @
-- data App = App
--   { -- ...
--   , appTracer :: Tracer
--   }
--
-- instance HasTracer App where
--   tracerL = lens appTracer $ \x y -> x { appTracer = y }
--
-- loadApp f = do
--   -- ...
--   withTracerProvider $ \tracerProvider -> do
--     let appTracer = makeTracer tracerProvider "my-app" tracerOptions
--     f App {..}
-- @
--
-- You may need to do this even if you don't plan to manually trace things, in order to
-- satisfy the 'MonadTracer' constraint required by functions like 'runDB'. If
-- you don't need this feature, and don't plan on running an otel-collector, set
-- @OTEL_TRACES_EXPORTER=none@ in the environment, which makes all tracing a
-- no-op.
--
-- In the future, it should be possible to use @OTEL_SDK_DISABLED@ for the same
-- purpose. See <https://github.com/iand675/hs-opentelemetry/issues/60>.
module Freckle.App.OpenTelemetry
  ( HasTracer (..)
  , Tracer

    -- * Effects
  , MonadTracer (..)
  , inSpan
  , defaultSpanArguments

    -- * Querying
  , withTraceIdContext
  , getCurrentTraceId
  , getCurrentTraceIdAsDatadog
  , getCurrentSpanContext

    -- * Setup
  , withTracerProvider

    -- ** 'Tracer'
  , makeTracer
  , tracerOptions
  ) where

import Freckle.App.Prelude

import Blammo.Logging (MonadMask, withThreadContext, (.=))
import Data.Word (Word64)
import OpenTelemetry.Context (lookupSpan)
import OpenTelemetry.Context.ThreadLocal (getContext)
import OpenTelemetry.Propagator.Datadog
  ( convertOpenTelemetryTraceIdToDatadogTraceId
  )
import OpenTelemetry.Trace hiding (inSpan)
import OpenTelemetry.Trace.Core (getSpanContext)
import qualified OpenTelemetry.Trace.Core as Trace (SpanContext (..))
import OpenTelemetry.Trace.Id (TraceId)
import OpenTelemetry.Trace.Monad
import UnliftIO.Exception (bracket)

withTracerProvider :: MonadUnliftIO m => (TracerProvider -> m a) -> m a
withTracerProvider =
  bracket
    (liftIO initializeGlobalTracerProvider)
    (liftIO . shutdownTracerProvider)

getCurrentTraceId :: MonadIO m => m (Maybe TraceId)
getCurrentTraceId = fmap Trace.traceId <$> getCurrentSpanContext

getCurrentTraceIdAsDatadog :: MonadIO m => m (Maybe Word64)
getCurrentTraceIdAsDatadog =
  fmap convertOpenTelemetryTraceIdToDatadogTraceId <$> getCurrentTraceId

getCurrentSpanContext :: MonadIO m => m (Maybe SpanContext)
getCurrentSpanContext = do
  mSpan <- lookupSpan <$> getContext
  traverse getSpanContext mSpan

withTraceIdContext :: (MonadIO m, MonadMask m) => m a -> m a
withTraceIdContext f = do
  mTraceId <- getCurrentTraceIdAsDatadog
  maybe f (\traceId -> withThreadContext ["trace_id" .= traceId] f) mTraceId
