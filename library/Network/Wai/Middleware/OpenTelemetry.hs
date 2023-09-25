module Network.Wai.Middleware.OpenTelemetry
  ( newOpenTelemetryWaiMiddleware
  , openTelemetryMiddleware
  ) where

import Freckle.App.Prelude

import Blammo.Logging (withThreadContext, (.=))
import qualified Data.ByteString.Char8 as BS8
import Freckle.App.OpenTelemetry
import Network.Wai
import Network.Wai.Middleware.AddHeaders
import qualified OpenTelemetry.Instrumentation.Wai as Trace
import OpenTelemetry.Propagator.Datadog
  ( convertOpenTelemetryTraceIdToDatadogTraceId
  )

newOpenTelemetryWaiMiddleware :: IO Middleware
newOpenTelemetryWaiMiddleware = do
  otel <- Trace.newOpenTelemetryWaiMiddleware
  pure $ otel . openTelemetryMiddleware

-- | Add 'TraceId' information to context and responses
--
-- - Adds @trace_id@ to the logging context
-- - Adds @X-Datadog-TraceId@ to response headers
--
-- This is added automatically by our 'newOpenTelemetryWaiMiddleware'.
openTelemetryMiddleware :: Middleware
openTelemetryMiddleware app request respond = do
  mTraceId <- getCurrentTraceId
  case mTraceId of
    Nothing -> app request respond
    Just traceId -> do
      let
        traceIdInt = convertOpenTelemetryTraceIdToDatadogTraceId traceId
        traceIdIntBS = BS8.pack $ show traceIdInt

      withThreadContext ["trace_id" .= traceIdInt] $
        addHeaders
          [("X-Datadog-Trace-Id", traceIdIntBS)]
          app
          request
          respond
