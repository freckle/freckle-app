-- | RTS statistics sent to Datadog
module Freckle.App.RtsStats
  ( forkRtsStatPolling
  ) where

import Prelude

import qualified Control.Immortal as Immortal
import Control.Monad (void)
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Foldable (traverse_)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Freckle.App.Datadog (HasDogStatsClient, HasDogStatsTags)
import qualified Freckle.App.Datadog as Datadog
import qualified System.Metrics as Ekg
import qualified System.Metrics.Distribution.Internal as Ekg
import UnliftIO.Concurrent (threadDelay)

-- | Initialize a thread to poll RTS stats
--
-- Stats are collected via `ekg-core` and 'System.Metrics.registerGcMetrics'
--
forkRtsStatPolling
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => m ()
forkRtsStatPolling = do
  store <- liftIO Ekg.newStore
  liftIO $ Ekg.registerGcMetrics store

  void $ Immortal.create $ \_ -> do
    sample <- liftIO $ Ekg.sampleAll store
    traverse_ (uncurry flushEkgSample) $ HashMap.toList sample

    let seconds n = n * 1000000
    threadDelay $ seconds 1

flushEkgSample
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => Text
  -> Ekg.Value
  -> m ()
flushEkgSample name = \case
  Ekg.Counter n -> Datadog.counter name [] $ fromIntegral n
  Ekg.Gauge n -> Datadog.guage name [] $ fromIntegral n
  Ekg.Distribution d -> do
    Datadog.guage (name <> "." <> "mean") [] $ Ekg.mean d
    Datadog.guage (name <> "." <> "variance") [] $ Ekg.variance d
    Datadog.guage (name <> "." <> "sum") [] $ Ekg.sum d
    Datadog.guage (name <> "." <> "min") [] $ Ekg.min d
    Datadog.guage (name <> "." <> "max") [] $ Ekg.max d
    Datadog.counter (name <> "." <> "count") [] $ fromIntegral $ Ekg.count d
  Ekg.Label _ -> pure ()
