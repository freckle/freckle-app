module Freckle.App.OpenTelemetry.Context
  ( HasHeaders (..)
  , CustomTraceContext (..)
  , extractContext
  , injectContext
  , processWithContext
  ) where

import Freckle.App.Prelude

import Control.Error.Util (hush)
import Control.Lens (Lens', lens, (&), (.~), (^.))
import Control.Monad.Catch (MonadMask)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Faktory.Job (Job, custom, jobOptions)
import Faktory.Job.Custom (fromCustom, toCustom)
import Faktory.JobOptions (JobOptions (..))
import Freckle.App.OpenTelemetry
  ( MonadTracer (..)
  , SpanArguments
  , inSpan
  )
import Freckle.App.OpenTelemetry.ThreadContext (withTraceContext)
import Network.HTTP.Client (Request, requestHeaders)
import Network.HTTP.Simple (setRequestHeaders)
import Network.HTTP.Types.Header (Header)
import OpenTelemetry.Context (Context)
import OpenTelemetry.Context.ThreadLocal (attachContext, getContext)
import OpenTelemetry.Propagator (Propagator, extract, inject)
import OpenTelemetry.Trace.Core
  ( getTracerProviderPropagators
  , getTracerTracerProvider
  )

class HasHeaders a where
  headersL :: Lens' a [Header]

instance HasHeaders [Header] where
  headersL = id

instance HasHeaders [(Text, Text)] where
  headersL = lens (map encode) $ \_ y -> map decode y

encode :: (Text, Text) -> (CI ByteString, ByteString)
encode = bimap (CI.mk . encodeUtf8) encodeUtf8

decode :: (CI ByteString, ByteString) -> (Text, Text)
decode = bimap (decodeUtf8 . CI.original) decodeUtf8

instance HasHeaders Request where
  headersL = lens requestHeaders $ \req hs -> setRequestHeaders hs req

instance HasHeaders (Job a) where
  headersL = optionsL . customTraceContextL . headersL

optionsL :: Lens' (Job a) JobOptions
optionsL = lens jobOptions $ \x y -> x {jobOptions = y}

customTraceContextL :: Lens' JobOptions CustomTraceContext
customTraceContextL = lens get set
 where
  get jo = fromMaybe mempty $ hush . fromCustom =<< joCustom jo
  set jo = (jo <>) . custom . toCustom

-- | A type that can be stored as the @custom@ field of a Faktory 'Job'
newtype CustomTraceContext = CustomTraceContext
  { traceHeaders :: [(Text, Text)]
  }
  deriving stock (Generic)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (FromJSON, ToJSON)

traceHeadersL :: Lens' CustomTraceContext [(Text, Text)]
traceHeadersL = lens traceHeaders $ \x y -> x {traceHeaders = y}

instance HasHeaders CustomTraceContext where
  headersL = traceHeadersL . headersL

-- | Update our trace context from that extracted from the given item's headers
extractContext
  :: (MonadIO m, MonadTracer m, HasHeaders a) => a -> m ()
extractContext a = do
  context <- getContext
  propagator <- getPropagator
  updatedContext <- extract propagator (a ^. headersL) context
  void $ attachContext updatedContext

-- | Inject our trace context into the given item's headers
injectContext
  :: (MonadIO m, MonadTracer m, HasHeaders a) => a -> m a
injectContext a = do
  context <- getContext
  propagator <- getPropagator
  headers <- inject propagator context $ a ^. headersL
  pure $ a & headersL .~ headers

getPropagator :: MonadTracer m => m (Propagator Context [Header] [Header])
getPropagator =
  getTracerProviderPropagators . getTracerTracerProvider <$> getTracer

-- | Process an item (a request, a Job, etc) in a top-level span and context
processWithContext
  :: ( MonadUnliftIO m
     , MonadMask m
     , MonadTracer m
     , HasHeaders a
     , HasCallStack
     )
  => Text
  -- ^ Span name
  -> SpanArguments
  -> a
  -> (a -> m b)
  -> m b
processWithContext name args a f = do
  extractContext a
  inSpan name args $ do
    a' <- injectContext a
    withTraceContext $ f a'
