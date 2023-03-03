module Freckle.App.OpenTelemetry.Wai
  ( newOpenTelemetryWaiMiddleware
  , addTraceIdResponseHeader
  , addTraceIdThreadContext
  , withTraceIdMiddleware
  ) where

import Freckle.App.Prelude

import Blammo.Logging (withThreadContext, (.=))
import Freckle.App.OpenTelemetry
import Network.Wai
import Network.Wai.Middleware.AddHeaders
import qualified OpenTelemetry.Instrumentation.Wai as Trace
import OpenTelemetry.Trace.Id
  (Base(Base16), TraceId, traceIdBaseEncodedByteString, traceIdBaseEncodedText)

-- | Like "OpenTelemetry.Instrumentation.Wai" with extra 'TraceId' handling
--
-- - Adds an @X-OTel-TraceId@ to responses
-- - Adds a @trace_id@ in the logging Thread Context
--
newOpenTelemetryWaiMiddleware :: IO Middleware
newOpenTelemetryWaiMiddleware = do
  otel <- Trace.newOpenTelemetryWaiMiddleware
  pure $ otel . addTraceIdResponseHeader . addTraceIdThreadContext

addTraceIdResponseHeader :: Middleware
addTraceIdResponseHeader =
  withTraceIdMiddleware $ \traceId app request respond -> do
    let traceIdBS = traceIdBaseEncodedByteString Base16 traceId
    addHeaders [("X-OTel-Trace-Id", traceIdBS)] app request respond

addTraceIdThreadContext :: Middleware
addTraceIdThreadContext =
  withTraceIdMiddleware $ \traceId app request respond -> do
    let traceIdHex = traceIdBaseEncodedText Base16 traceId
    withThreadContext ["trace_id" .= traceIdHex] $ app request respond

withTraceIdMiddleware :: (TraceId -> Middleware) -> Middleware
withTraceIdMiddleware f app request respond = do
  mTraceId <- getCurrentTraceId
  case mTraceId of
    Nothing -> app request respond
    Just traceId -> f traceId app request respond
