{-# OPTIONS_GHC -Wno-deprecations #-}

module Network.Wai.Middleware.OpenTelemetry
  ( newOpenTelemetryWaiMiddleware
  , openTelemetryMiddleware
  ) where

import Freckle.App.Prelude

import qualified Data.ByteString.Char8 as BS8
import Freckle.App.OpenTelemetry
import Network.Wai
import Network.Wai.Middleware.AddHeaders
import qualified OpenTelemetry.Instrumentation.Wai as Trace

newOpenTelemetryWaiMiddleware :: IO Middleware
newOpenTelemetryWaiMiddleware = do
  otel <- Trace.newOpenTelemetryWaiMiddleware
  pure $ otel . openTelemetryMiddleware
{-# DEPRECATED
  newOpenTelemetryWaiMiddleware
  "Use OpenTelemetry.Instrumentation.Wai.newOpenTelemetryWaiMiddleware"
  #-}

-- | Add 'TraceId' information to context and responses
--
-- - Adds @trace_id@ to the logging context
-- - Adds @X-Datadog-TraceId@ to response headers
--
-- This is added automatically by our 'newOpenTelemetryWaiMiddleware'.
openTelemetryMiddleware :: Middleware
openTelemetryMiddleware app request respond =
  withTraceIdContext $ addTraceIdHeader app request respond
{-# DEPRECATED openTelemetryMiddleware "Use addThreadContextFromTracing" #-}

addTraceIdHeader :: Middleware
addTraceIdHeader app request respond = do
  mTraceId <- getCurrentTraceIdAsDatadog
  maybe id addHeader mTraceId app request respond
 where
  addHeader traceId =
    addHeaders [("X-Datadog-Trace-Id", BS8.pack $ show traceId)]
