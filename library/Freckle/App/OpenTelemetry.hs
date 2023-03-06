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
  , withTracerProvider

  -- ** 'Tracer'
  , makeTracer
  , tracerOptions
  ) where

import Freckle.App.Prelude

import Control.Lens ((.~), (<>~))
import Data.Char (toLower)
import Freckle.App.OpenTelemetry.Sampler
import OpenTelemetry.AWSXRay
import qualified OpenTelemetry.Context as Trace
import qualified OpenTelemetry.Context.ThreadLocal as Trace
import OpenTelemetry.Trace
  ( HasTracer(..)
  , Tracer
  , TracerProvider
  , createTracerProvider
  , defaultSpanArguments
  , emptyTracerProviderOptions
  , getTracerProviderInitializationOptions
  , makeTracer
  , setGlobalTracerProvider
  , tracerOptions
  )
import qualified OpenTelemetry.Trace.Core as Trace hiding (inSpan)
import OpenTelemetry.Trace.Id (Base(Base16), TraceId, traceIdBaseEncodedText)
import OpenTelemetry.Trace.Monad (MonadTracer(..))
import qualified OpenTelemetry.Trace.Monad as Trace
import OpenTelemetry.Trace.Setup.Lens
import System.Environment (lookupEnv)
import UnliftIO.Exception (bracket)

inSpan :: (MonadUnliftIO m, MonadTracer m, HasCallStack) => Text -> m a -> m a
inSpan = flip Trace.inSpan defaultSpanArguments

-- | Setup tracing with our preferred options
--
-- - Use X-Ray-compatible Ids and propagation
-- - Sample interesting requests only
-- - Disable if @OTEL_SDK_DISABLED@ is set
--
withTracerProvider :: MonadUnliftIO m => (TracerProvider -> m a) -> m a
withTracerProvider =
  bracket initializeGlobalTracerProvider shutdownTracerProvider

initializeGlobalTracerProvider :: MonadIO m => m TracerProvider
initializeGlobalTracerProvider = liftIO $ do
  t <- initializeTracerProvider
  t <$ setGlobalTracerProvider t

initializeTracerProvider :: MonadIO m => m TracerProvider
initializeTracerProvider = liftIO $ do
  -- TODO: https://github.com/iand675/hs-opentelemetry/issues/60
  disabled <- maybe False ((== "true") . map toLower)
    <$> lookupEnv "OTEL_SDK_DISABLED"

  (processors, opts) <- if disabled
    then pure ([], emptyTracerProviderOptions)
    else getTracerProviderInitializationOptions

  createTracerProvider processors $ setup opts
 where
  setup =
    (idGeneratorL .~ awsXRayIdGenerator)
      . (propagatorL <>~ awsXRayContextPropagator)
      . (samplerL .~ freckleSampler)

shutdownTracerProvider :: MonadIO m => TracerProvider -> m ()
shutdownTracerProvider = liftIO . Trace.shutdownTracerProvider

getCurrentTraceId :: MonadIO m => m (Maybe TraceId)
getCurrentTraceId = do
  mSpan <- Trace.lookupSpan <$> Trace.getContext
  traverse (fmap Trace.traceId . Trace.getSpanContext) mSpan

getCurrentTraceIdHex :: MonadIO m => m (Maybe Text)
getCurrentTraceIdHex =
  fmap (traceIdBaseEncodedText Base16) <$> getCurrentTraceId
