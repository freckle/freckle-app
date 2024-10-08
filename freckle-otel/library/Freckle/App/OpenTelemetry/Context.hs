module Freckle.App.OpenTelemetry.Context
  ( HasHeaders (..)
  , CustomTraceContext (..)
  , extractContext
  , injectContext
  , processWithContext
  ) where

import Prelude

import Control.Error.Util (hush)
import Control.Lens (Lens', lens, (.~), (^.))
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Function ((&))
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Faktory.Job (Job, custom, jobOptions)
import Faktory.Job.Custom (fromCustom, toCustom)
import Faktory.JobOptions (JobOptions (..))
import Freckle.App.OpenTelemetry
  ( MonadTracer (..)
  , SpanArguments
  , inSpan
  )
import Freckle.App.OpenTelemetry.ThreadContext (withTraceContext)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
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
import UnliftIO (MonadUnliftIO)

class HasHeaders a where
  headersL :: Lens' a [Header]

instance HasHeaders [Header] where
  headersL = id

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

instance HasHeaders CustomTraceContext where
  headersL = lens (map encode . traceHeaders) $ \x y -> x {traceHeaders = map decode y}

encode :: (Text, Text) -> (CI ByteString, ByteString)
encode = bimap (CI.mk . T.encodeUtf8) T.encodeUtf8

decode :: (CI ByteString, ByteString) -> (Text, Text)
decode = bimap (T.decodeUtf8 . CI.original) T.decodeUtf8

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
