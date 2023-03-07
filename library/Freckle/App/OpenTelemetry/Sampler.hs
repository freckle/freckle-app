module Freckle.App.OpenTelemetry.Sampler
  ( freckleSampler
  ) where

import Freckle.App.Prelude

import Data.Coerce (coerce)
import Data.Fixed (Milli, Nano)
import Data.Function (on)
import qualified Network.HTTP.Types.Status as HTTP
import OpenTelemetry.Context (Context)
import qualified OpenTelemetry.Context as Trace
import OpenTelemetry.Trace
  (Attribute(..), PrimitiveAttribute(..), SpanArguments(..))
import qualified OpenTelemetry.Trace.Core as Trace hiding (inSpan)
import OpenTelemetry.Trace.Sampler
  (Sampler(..), SamplingResult(..), parentBased, parentBasedOptions)
import OpenTelemetry.Trace.TraceState (TraceState)
import qualified OpenTelemetry.Trace.TraceState as TraceState

-- | Traces requests if they're /interesting/: (non-200 or over 100ms)
freckleSampler :: Sampler
freckleSampler = parentBased $ parentBasedOptions $ Sampler
  { getDescription = "interesting_requests"
  , shouldSample = \ctxt _traceId _name args -> do
    mTraceState <- getTraceState ctxt
    mDuration <- getDuration args

    liftIO $ print $ attributes args
    liftIO $ print mDuration
    liftIO $ print $ milliToNano 100

    let
      mStatus = toEnum . round <$> getDoubleAttribute "http.status_code" args

      result = case (mStatus, mDuration) of
        (Just s, Just d) | httpIsInteresting s d -> RecordAndSample
        _ -> Drop

    liftIO $ print mStatus
    liftIO $ print result

    pure (result, [], fromMaybe TraceState.empty mTraceState)
  }

httpIsInteresting :: HTTP.Status -> Nano -> Bool
httpIsInteresting s d = s /= HTTP.status200 || d >= milliToNano 100

milliToNano :: Milli -> Nano
milliToNano = (* 1000000) . coerce

getTraceState :: MonadIO m => Context -> m (Maybe TraceState)
getTraceState ctxt = fmap Trace.traceState
  <$> traverse Trace.getSpanContext (Trace.lookupSpan ctxt)

getDuration :: MonadIO m => SpanArguments -> m (Maybe Nano)
getDuration args = for (startTime args) $ \start -> do
  now <- Trace.getTimestamp
  pure $ fromIntegral $ (subtract `on` Trace.timestampNanoseconds) now start

getDoubleAttribute :: Text -> SpanArguments -> Maybe Double
getDoubleAttribute name args = case lookup name (attributes args) of
  Just (AttributeValue (DoubleAttribute x)) -> Just x
  _ -> Nothing
