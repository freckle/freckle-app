{-# LANGUAGE OverloadedRecordDot #-}

-- | En/decode JSON values with trace headers
--
-- For example, before enqueing it as some kind of a job or event. That way,
-- consuming it can set the same trace context when possible. If the JSON
-- en/decoding works the same as without the envelope, so this is safe to add
-- when possible and/or transition to with in-flight jobs or events.
--
-- The module is meant to be imported qualified and used in place of the
-- same-named @aeson@ functions.
--
-- __NOTE__: if the value being wrapped does not en/decode as a JSON object, it
-- is left as-is and no trace-envelope will be added.
module Freckle.App.OpenTelemetry.TraceEnvelope
  ( encodeStrict
  , eitherDecodeStrict

    -- * Exported for use in tests
  , traceHeadersKey
  ) where

import Freckle.App.Prelude

import Control.Lens (Lens', lens)
import Data.Aeson (FromJSON, ToJSON, Value (..), (.!=), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Freckle.App.OpenTelemetry (MonadTracer)
import Freckle.App.OpenTelemetry.Context

data TraceEnvelope a = TraceEnvelope
  { headers :: [(Text, Text)]
  , payload :: a
  }

instance HasHeaders (TraceEnvelope a) where
  headersL = l . headersL
   where
    l :: Lens' (TraceEnvelope a) [(Text, Text)]
    l = lens headers $ \x y -> x {headers = y}

instance FromJSON a => FromJSON (TraceEnvelope a) where
  parseJSON = \case
    Object o ->
      TraceEnvelope
        <$> o .:? traceHeadersKey .!= []
        <*> Aeson.parseJSON (Object $ KeyMap.delete traceHeadersKey o)
    v -> TraceEnvelope [] <$> Aeson.parseJSON v

instance ToJSON a => ToJSON (TraceEnvelope a) where
  toJSON enveloped = case Aeson.toJSON enveloped.payload of
    Object o ->
      Object $
        KeyMap.insert traceHeadersKey (Aeson.toJSON enveloped.headers) o
    v -> v

traceHeadersKey :: Aeson.Key
traceHeadersKey = "__trace_headers"

encodeStrict :: (MonadIO m, MonadTracer m, ToJSON a) => a -> m ByteString
encodeStrict = fmap (BSL.toStrict . Aeson.encode) . injectContext . TraceEnvelope []

eitherDecodeStrict
  :: (MonadIO m, MonadTracer m, FromJSON a) => ByteString -> m (Either String a)
eitherDecodeStrict bs = do
  case Aeson.eitherDecodeStrict @(TraceEnvelope _) bs of
    Left err -> pure $ Left err
    Right enveloped -> Right enveloped.payload <$ extractContext enveloped
