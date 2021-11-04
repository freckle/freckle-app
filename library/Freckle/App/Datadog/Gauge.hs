-- | Stateful gauges for Datadog
module Freckle.App.Datadog.Gauge
  ( Gauge
  , new
  , increment
  , decrement
  , add
  , subtract
  ) where

import Prelude hiding (subtract)

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader
import Data.Int (Int64)
import Data.Text (Text)
import Freckle.App.Datadog (HasDogStatsClient, HasDogStatsTags, gauge)
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
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => Gauge
  -> m ()
increment = add 1

-- | Add to gauge state and report its current value
add
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => Int64
  -> Gauge
  -> m ()
add i = withEKGGauge (`EKG.add` i)

-- | Decrement gauge state and report its current value
decrement
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => Gauge
  -> m ()
decrement = subtract 1

-- | Subtract from gauge state and report its current value
subtract
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => Int64
  -> Gauge
  -> m ()
subtract i = withEKGGauge (`EKG.subtract` i)

withEKGGauge
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => (EKG.Gauge -> IO ())
  -> Gauge
  -> m ()
withEKGGauge f Gauge {..} = do
  current <- liftIO $ do
    f ekgGauge
    EKG.read ekgGauge
  gauge name tags $ fromIntegral current
