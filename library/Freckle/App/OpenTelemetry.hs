module Freckle.App.OpenTelemetry
  ( HasTracer(..)
  , MonadTracer(..)
  , inSpan
  , withTracerProviderXRay
  , withTracerProviderDisabled
  ) where

import Freckle.App.Prelude

import Control.Lens ((.~), (<>~))
import OpenTelemetry.AWSXRay
import OpenTelemetry.Trace
  ( HasTracer(..)
  , TracerProvider
  , defaultSpanArguments
  , emptyTracerProviderOptions
  )
import OpenTelemetry.Trace.Monad (MonadTracer(..))
import qualified OpenTelemetry.Trace.Monad as Trace
import qualified OpenTelemetry.Trace.Setup as Trace
import OpenTelemetry.Trace.Setup.Lens

inSpan :: (MonadUnliftIO m, MonadTracer m, HasCallStack) => Text -> m a -> m a
inSpan = flip Trace.inSpan defaultSpanArguments

-- | Configure tracing with Id values compatible with AWS X-Ray
withTracerProviderXRay :: MonadUnliftIO m => (TracerProvider -> m a) -> m a
withTracerProviderXRay =
  Trace.withTracerProvider
    $ (idGeneratorL .~ awsXRayIdGenerator)
    . (propagatorL <>~ awsXRayContextPropagator)

-- | Configure no tracing
--
-- This allows you to still make and set a 'Tracer' and satisfy 'MonadTracer',
-- but not actually emit any traces.
--
withTracerProviderDisabled :: MonadUnliftIO m => (TracerProvider -> m a) -> m a
withTracerProviderDisabled =
  Trace.withTracerProvider $ const emptyTracerProviderOptions
