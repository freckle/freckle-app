module Freckle.App.OpenTelemetry
  ( HasTracer(..)
  , Tracer

  -- * Effects
  , MonadTracer(..)
  , inSpan

  -- * Querying
  , getCurrentTraceId
  , getCurrentTraceIdHex

  -- * Setup
  -- ** Built-ins
  , withTracerProviderXRay
  , withTracerProviderXRayLogger
  , withTracerProviderDisabled

  -- ** Combinatorially
  , withTracerProvider
  , setupAWSXRay
  , setupLogger
  , Milli
  , milliToNano
  , Nano
  , setupSampling
  , disableTracing

  -- ** 'Tracer'
  , makeTracer
  , tracerOptions
  ) where

import Freckle.App.Prelude

import Blammo.Logging
  (LogLevel(..), Logger, Message(..), logOtherNS, runLoggerLoggingT)
import Control.Lens ((.~), (<>~))
import Data.Coerce (coerce)
import Data.Fixed (Milli, Nano)
import Data.Function (on)
import qualified Network.HTTP.Types.Status as HTTP
import OpenTelemetry.AWSXRay
import OpenTelemetry.Context (Context)
import qualified OpenTelemetry.Context as Trace
import qualified OpenTelemetry.Context.ThreadLocal as Trace
import qualified OpenTelemetry.Logging.Core as Trace
import OpenTelemetry.Trace
  ( Attribute(..)
  , HasTracer(..)
  , PrimitiveAttribute(..)
  , SpanArguments(..)
  , Tracer
  , TracerProvider
  , TracerProviderOptions
  , defaultSpanArguments
  , emptyTracerProviderOptions
  , makeTracer
  , tracerOptions
  )
import qualified OpenTelemetry.Trace.Core as Trace hiding (inSpan)
import OpenTelemetry.Trace.Id (Base(Base16), TraceId, traceIdBaseEncodedText)
import OpenTelemetry.Trace.Monad (MonadTracer(..))
import qualified OpenTelemetry.Trace.Monad as Trace
import OpenTelemetry.Trace.Sampler
  (Sampler(..), SamplingResult(..), parentBased, parentBasedOptions)
import OpenTelemetry.Trace.Setup
import OpenTelemetry.Trace.Setup.Lens
import OpenTelemetry.Trace.TraceState (TraceState)
import qualified OpenTelemetry.Trace.TraceState as TraceState

inSpan :: (MonadUnliftIO m, MonadTracer m, HasCallStack) => Text -> m a -> m a
inSpan = flip Trace.inSpan defaultSpanArguments

-- | Configure tracing with Id values compatible with AWS X-Ray
withTracerProviderXRay :: MonadUnliftIO m => (TracerProvider -> m a) -> m a
withTracerProviderXRay =
  withTracerProvider $ setupAWSXRay . setupSampling httpIsInteresting

-- | 'withTracerProviderXRay' but also configure to log via 'Logger'
withTracerProviderXRayLogger
  :: MonadUnliftIO m => Logger -> (TracerProvider -> m a) -> m a
withTracerProviderXRayLogger logger =
  withTracerProvider
    $ setupAWSXRay
    . setupSampling httpIsInteresting
    . setupLogger logger

httpIsInteresting :: HTTP.Status -> Nano -> Bool
httpIsInteresting s d = HTTP.statusCode s /= 200 || d >= milliToNano 100

milliToNano :: Milli -> Nano
milliToNano = (* 1000000) . coerce

-- | Configure no tracing
--
-- This allows you to still make and set a 'Tracer' and satisfy 'MonadTracer',
-- but not actually emit any traces.
--
withTracerProviderDisabled :: MonadUnliftIO m => (TracerProvider -> m a) -> m a
withTracerProviderDisabled = withTracerProvider disableTracing

setupAWSXRay :: TracerProviderOptions -> TracerProviderOptions
setupAWSXRay =
  (idGeneratorL .~ awsXRayIdGenerator)
    . (propagatorL <>~ awsXRayContextPropagator)

setupLogger :: Logger -> (TracerProviderOptions -> TracerProviderOptions)
setupLogger logger = loggerL .~ go
 where
  go l = runLoggerLoggingT logger $ do
    let (level, message) = fromOTelLog l
    logOtherNS "hs-opentelemetry-sdk" level message

-- | Set up to sample only interesting requests
setupSampling
  :: (HTTP.Status -> Nano -> Bool)
  -> TracerProviderOptions
  -> TracerProviderOptions
setupSampling isInteresting = samplerL .~ sampler
 where
  sampler = parentBased $ parentBasedOptions $ Sampler
    { getDescription = ""
    , shouldSample = \ctxt _traceId _name args -> do
      mTraceState <- getTraceState ctxt
      mDuration <- getDuration args

      let
        mStatus =
          (`HTTP.mkStatus` "")
            . round
            <$> getDoubleAttribute "http.status_code" args

        result = case (mStatus, mDuration) of
          (Just s, Just d) | isInteresting s d -> RecordAndSample
          _ -> Drop

      pure (result, [], fromMaybe TraceState.empty mTraceState)
    }

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

disableTracing :: TracerProviderOptions -> TracerProviderOptions
disableTracing = const emptyTracerProviderOptions

-- |
--
-- TODO: Use more fields, i.e. metadata
--
-- <https://hackage.haskell.org/package/hs-opentelemetry-api-0.0.3.6/docs/OpenTelemetry-Logging-Core.html#t:Log>
--
fromOTelLog :: Trace.Log Text -> (LogLevel, Message)
fromOTelLog = fromOTelSeverity . Trace.severityNumber &&& (:# []) . Trace.body

fromOTelSeverity :: Maybe Int64 -> LogLevel
fromOTelSeverity = \case
  Just n | n > 17 -> LevelError
  Just n | n > 13 -> LevelWarn
  Just n | n > 9 -> LevelInfo
  Just n | n > 1 -> LevelDebug
  _ -> LevelOther "unknown"

getCurrentTraceId :: MonadIO m => m (Maybe TraceId)
getCurrentTraceId = do
  mSpan <- Trace.lookupSpan <$> Trace.getContext
  traverse (fmap Trace.traceId . Trace.getSpanContext) mSpan

getCurrentTraceIdHex :: MonadIO m => m (Maybe Text)
getCurrentTraceIdHex =
  fmap (traceIdBaseEncodedText Base16) <$> getCurrentTraceId
