module Freckle.App.OpenTelemetry
  ( HasTracer(..)
  , Tracer

  -- * Effects
  , MonadTracer(..)
  , inSpan

  -- * Setup
  -- ** Built-ins
  , withTracerProviderXRay
  , withTracerProviderXRayLogger
  , withTracerProviderDisabled

  -- ** Combinatorially
  , withTracerProvider
  , setupAWSXRay
  , setupLogger
  , disableTracing

  -- ** 'Tracer'
  , makeTracer
  , tracerOptions
  ) where

import Freckle.App.Prelude

import Blammo.Logging
  (LogLevel(..), Logger, Message(..), logOtherNS, runLoggerLoggingT)
import Control.Lens ((.~), (<>~))
import OpenTelemetry.AWSXRay
import qualified OpenTelemetry.Logging.Core as Trace
import OpenTelemetry.Trace
  ( HasTracer(..)
  , Tracer
  , TracerProvider
  , TracerProviderOptions
  , defaultSpanArguments
  , emptyTracerProviderOptions
  , makeTracer
  , tracerOptions
  )
import OpenTelemetry.Trace.Monad (MonadTracer(..))
import qualified OpenTelemetry.Trace.Monad as Trace
import OpenTelemetry.Trace.Setup
import OpenTelemetry.Trace.Setup.Lens

inSpan :: (MonadUnliftIO m, MonadTracer m, HasCallStack) => Text -> m a -> m a
inSpan = flip Trace.inSpan defaultSpanArguments

-- | Configure tracing with Id values compatible with AWS X-Ray
withTracerProviderXRay :: MonadUnliftIO m => (TracerProvider -> m a) -> m a
withTracerProviderXRay = withTracerProvider setupAWSXRay

-- | 'withTracerProviderXRay' but also configure to log via 'Logger'
withTracerProviderXRayLogger
  :: MonadUnliftIO m => Logger -> (TracerProvider -> m a) -> m a
withTracerProviderXRayLogger logger =
  withTracerProvider $ setupAWSXRay . setupLogger logger

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
