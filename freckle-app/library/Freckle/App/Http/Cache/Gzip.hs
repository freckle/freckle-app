{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Type and functions for handling gzipped HTTP responses
--
-- In order to optimize caching of responses in storage with size limitations,
-- we cache gzipped responses as-is. This requires disabling the automatic
-- decompression of @http-client@ and handling it ourselves.
--
-- The module makes that a type-enforced process:
--
-- - 'requestPotentiallyGzipped' is the only way to get a 'PotentiallyGzipped'
-- - Which is the type needed for the response field in 'CachedResponse'
-- - 'gunzipResponseBody' is the only way to erase 'PotentiallyGzipped'
-- - Which is what you actually need to return
module Freckle.App.Http.Cache.Gzip
  ( PotentiallyGzipped
  , requestPotentiallyGzipped
  , gunzipResponseBody
  ) where

import Freckle.App.Prelude

import Codec.Serialise (Serialise)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Freckle.App.Http (disableRequestDecompress)
import Freckle.App.Http.Header
import Network.HTTP.Client (Request, Response)
import Network.HTTP.Client.Internal qualified as HTTP

newtype PotentiallyGzipped a = PotentiallyGzipped
  { unwrap :: a
  }
  deriving stock (Show, Eq)
  deriving newtype (Serialise)

-- | Run a request /without/ automatic 'decompress' and tag the @body@ type
requestPotentiallyGzipped
  :: Functor m
  => (Request -> m (Response body))
  -> Request
  -> m (Response (PotentiallyGzipped body))
requestPotentiallyGzipped doHttp =
  fmap (fmap PotentiallyGzipped) . doHttp . disableRequestDecompress

-- | Gunzip a 'PotentiallyGzipped' body, if necessary
gunzipResponseBody
  :: MonadIO m
  => Request
  -> Response (PotentiallyGzipped ByteString)
  -> m (Response ByteString)
gunzipResponseBody req resp
  | HTTP.needsGunzip req (getHeaders resp) = liftIO $ do
      body <- gunzipBody $ HTTP.responseBody resp
      pure $ body <$ resp
  | otherwise = pure $ (.unwrap) <$> resp

gunzipBody :: PotentiallyGzipped ByteString -> IO ByteString
gunzipBody body = do
  body1 <- HTTP.constBodyReader $ BSL.toChunks body.unwrap
  reader <- HTTP.makeGzipReader body1
  BSL.fromChunks <$> HTTP.brConsume reader
