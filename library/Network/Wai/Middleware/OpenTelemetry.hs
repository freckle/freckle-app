module Network.Wai.Middleware.OpenTelemetry
  ( newOpenTelemetryWaiMiddleware
  , openTelemetryMiddleware

    -- * Exported for testing
  , ddTraceId
  ) where

import Freckle.App.Prelude

import Blammo.Logging (withThreadContext, (.=))
import Data.Bits (Bits (shift))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Short.Internal as SBI
import Data.Primitive.ByteArray (ByteArray (ByteArray), indexByteArray)
import Data.Word (Word64, Word8)
import Freckle.App.OpenTelemetry
import Network.Wai
import Network.Wai.Middleware.AddHeaders
import qualified OpenTelemetry.Instrumentation.Wai as Trace
import OpenTelemetry.Trace.Id (TraceId, traceIdBytes)

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
    Just (traceId, _) -> do
      let
        traceIdInt = ddTraceId traceId
        traceIdIntBS = BS8.pack $ show traceIdInt

      withThreadContext ["trace_id" .= traceIdInt] $
        addHeaders
          [("X-Datadog-Trace-Id", traceIdIntBS)]
          app
          request
          respond

-- | <https://github.com/iand675/hs-opentelemetry/blob/1f0328eb59fec2a97aec7ef98fe4f1e0d5c8f2ac/propagators/datadog/src/OpenTelemetry/Propagator/Datadog.hs#L103>
ddTraceId :: TraceId -> Word64
ddTraceId traceId = indexByteArrayNbo (ByteArray a) 1
 where
  !(SBI.SBS a) = SBI.toShort $ traceIdBytes traceId

-- | <https://github.com/iand675/hs-opentelemetry/blob/1f0328eb59fec2a97aec7ef98fe4f1e0d5c8f2ac/propagators/datadog/src/OpenTelemetry/Propagator/Datadog/Internal.hs#LL109-L118C97>
indexByteArrayNbo :: ByteArray -> Int -> Word64
indexByteArrayNbo ba offset = loop 0 0
 where
  loop 8 acc = acc
  loop n acc =
    loop (n + 1) $
      shift acc 8
        + fromIntegral @Word8 @Word64
          (indexByteArray ba $ 8 * offset + n)
