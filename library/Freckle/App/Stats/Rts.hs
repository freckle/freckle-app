-- | Send RTS statistics via "Freckle.App.Stats"
module Freckle.App.Stats.Rts
  ( forkRtsStatPolling
  ) where

import Freckle.App.Prelude

import qualified Control.Immortal as Immortal
import qualified Data.HashMap.Strict as HashMap
import Freckle.App.Stats (HasStatsClient)
import qualified Freckle.App.Stats as Stats
import qualified System.Metrics as Ekg
import qualified System.Metrics.Distribution.Internal as Ekg
import UnliftIO.Concurrent (threadDelay)

-- | Initialize a thread to poll RTS stats
--
-- Stats are collected via `ekg-core` and 'System.Metrics.registerGcMetrics'
--
forkRtsStatPolling
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env) => m ()
forkRtsStatPolling = do
  store <- liftIO Ekg.newStore
  liftIO $ Ekg.registerGcMetrics store

  void $ Immortal.create $ \_ -> do
    sample <- liftIO $ Ekg.sampleAll store
    traverse_ (uncurry flushEkgSample) $ HashMap.toList sample

    let seconds n = n * 1000000
    threadDelay $ seconds 1

flushEkgSample
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env)
  => Text
  -> Ekg.Value
  -> m ()
flushEkgSample name = \case
  Ekg.Counter n -> Stats.counter name $ fromIntegral n
  Ekg.Gauge n -> Stats.gauge name $ fromIntegral n
  Ekg.Distribution d -> do
    Stats.gauge (name <> "." <> "mean") $ Ekg.mean d
    Stats.gauge (name <> "." <> "variance") $ Ekg.variance d
    Stats.gauge (name <> "." <> "sum") $ Ekg.sum d
    Stats.gauge (name <> "." <> "min") $ Ekg.min d
    Stats.gauge (name <> "." <> "max") $ Ekg.max d
    Stats.counter (name <> "." <> "count") $ fromIntegral $ Ekg.count d
  Ekg.Label _ -> pure ()
