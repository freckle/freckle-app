-- | Stateful gauges for "Freckle.App.Stats"
module Freckle.App.Stats.Gauge
  ( Gauge
  , new
  , increment
  , decrement
  , add
  , subtract
  ) where

import Freckle.App.Prelude hiding (subtract)

import Freckle.App.Stats (HasStatsClient)
import qualified Freckle.App.Stats as Stats
import qualified System.Metrics.Gauge as EKG

-- | A data type containing all reporting values for a gauge
data Gauge = Gauge
  { name :: Text
  , tags :: [(Text, Text)]
  , ekgGauge :: EKG.Gauge
  }

-- | Create a gauge holding in memory state
new :: MonadIO m => Text -> [(Text, Text)] -> m Gauge
new name tags = Gauge name tags <$> liftIO EKG.new

-- | Increment gauge state and report its current value
increment
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env) => Gauge -> m ()
increment = add 1

-- | Add to gauge state and report its current value
add
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env)
  => Int64
  -> Gauge
  -> m ()
add i = withEKGGauge (`EKG.add` i)

-- | Decrement gauge state and report its current value
decrement
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env) => Gauge -> m ()
decrement = subtract 1

-- | Subtract from gauge state and report its current value
subtract
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env)
  => Int64
  -> Gauge
  -> m ()
subtract i = withEKGGauge (`EKG.subtract` i)

withEKGGauge
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env)
  => (EKG.Gauge -> IO ())
  -> Gauge
  -> m ()
withEKGGauge f Gauge {..} = do
  current <- liftIO $ do
    f ekgGauge
    EKG.read ekgGauge
  Stats.tagged tags $ Stats.gauge name $ fromIntegral current
