{-# LANGUAGE NamedFieldPuns #-}

module Freckle.App.OpenTelemetry.ThreadContext
  ( withTraceContext
  ) where

import Relude hiding (traceId)

import Blammo.Logging (MonadMask, withThreadContext)
import Data.Aeson ((.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Pair)
import Freckle.App.OpenTelemetry (getCurrentSpanContext)
import OpenTelemetry.Trace.Core (SpanContext (..))
import OpenTelemetry.Trace.Id
  ( Base (..)
  , spanIdBaseEncodedText
  , traceIdBaseEncodedText
  )
import OpenTelemetry.Trace.TraceState (Key (..), TraceState (..), Value (..))

-- | Add tracing context values to the logging context
--
-- Values are encoded to 'Base16' (i.e. hex).
withTraceContext :: (MonadIO m, MonadMask m) => m a -> m a
withTraceContext f = do
  mSpanContext <- getCurrentSpanContext

  case mSpanContext of
    Nothing -> f
    Just sc -> withThreadContext (toThreadContext sc) f

toThreadContext :: SpanContext -> [Pair]
toThreadContext SpanContext {traceId, spanId, traceState} =
  [ "trace_id" .= traceIdBaseEncodedText Base16 traceId
  , "span_id" .= spanIdBaseEncodedText Base16 spanId
  , "trace_state" .= traceStatePairs
  ]
 where
  traceStatePairs :: [Pair]
  traceStatePairs = map traceStatePair $ unTraceState traceState

  traceStatePair :: (Key, Value) -> Pair
  traceStatePair = uncurry (.=) . bimap (Key.fromText . unKey) unValue

unTraceState :: TraceState -> [(Key, Value)]
unTraceState (TraceState x) = x

unKey :: Key -> Text
unKey (Key x) = x

unValue :: Value -> Text
unValue (Value x) = x
