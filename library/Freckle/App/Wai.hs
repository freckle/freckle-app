-- | Integration of "Freckle.App" tooling with "Network.Wai"
module Freckle.App.Wai
  ( noCacheMiddleware
  , denyFrameEmbeddingMiddleware

    -- * CORS
  , corsMiddleware

    -- * Logs
  , requestLogger
  , addThreadContextFromRequest

    -- * Tracing
  , newOpenTelemetryWaiMiddleware
  , addThreadContextFromTracing

    -- * Metrics
  , addThreadContextFromStatsTags
  , requestStats
  ) where

import Freckle.App.Prelude

import Freckle.App.OpenTelemetry.ThreadContext
import Network.Wai
import Network.Wai.Middleware.AddHeaders
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Logging
import Network.Wai.Middleware.Stats
import OpenTelemetry.Instrumentation.Wai

-- | Middleware that adds header to disable all caching
noCacheMiddleware :: Middleware
noCacheMiddleware =
  addHeaders [("Cache-Control", "no-cache, no-store, max-age=0, private")]

-- | Middleware that adds header to deny all frame embedding
denyFrameEmbeddingMiddleware :: Middleware
denyFrameEmbeddingMiddleware = addHeaders [("X-Frame-Options", "DENY")]

-- | Middleware that adds trace context to logging context
addThreadContextFromTracing :: Middleware
addThreadContextFromTracing app req = withTraceContext . app req
