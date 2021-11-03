{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Datadog access for your @App@
module Freckle.App.Datadog
  (
  -- * Reader environment interface
    HasDogStatsClient(..)
  , HasDogStatsTags(..)
  , StatsClient
  , Tag

  -- * Lower-level operations
  , sendAppMetricWithTags

  -- * Higher-level operations
  , increment
  , counter
  , gauge
  , histogram
  , histogramSince
  , histogramSinceMs

  -- ** Stateful Gauges
  , Gauge
  , newGauge
  , incrementGauge
  , decrementGauge
  , addGauge
  , subtractGauge

  -- * Reading settings at startup
  , DogStatsSettings(..)
  , envParseDogStatsEnabled
  , envParseDogStatsSettings
  , envParseDogStatsTags
  , mkStatsClient
  ) where

import Prelude

import Control.Lens (set)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import qualified Freckle.App.Env as Env
import Network.StatsD.Datadog hiding (Gauge, metric, name, tags)
import qualified Network.StatsD.Datadog as Datadog
import qualified System.Metrics.Gauge as EKG
import Yesod.Core.Types (HandlerData, handlerEnv, rheSite)

class HasDogStatsClient app where
  getDogStatsClient :: app -> Maybe StatsClient

instance HasDogStatsClient site =>  HasDogStatsClient (HandlerData child site) where
  getDogStatsClient = getDogStatsClient . rheSite . handlerEnv

class HasDogStatsTags app where
  getDogStatsTags :: app -> [Tag]

instance HasDogStatsTags site =>  HasDogStatsTags (HandlerData child site) where
  getDogStatsTags = getDogStatsTags . rheSite . handlerEnv

increment
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => Text
  -> [(Text, Text)]
  -> m ()
increment name tags = counter name tags 1

counter
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => Text
  -> [(Text, Text)]
  -> Int
  -> m ()
counter name tags = sendAppMetricWithTags name tags Datadog.Counter

-- | A data type containing all reporting values for a gauge
data Gauge = Gauge
  { gaugeName :: Text
  , gaugeTags :: [(Text, Text)]
  , gaugeAtomic :: EKG.Gauge
  }

-- | Create a gauge holding in memory state
newGauge :: (MonadIO m) => Text -> [(Text, Text)] -> m Gauge
newGauge name tags = Gauge name tags <$> liftIO EKG.new

-- | Increment gauge state and report its current value
incrementGauge
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => Gauge
  -> m ()
incrementGauge = addGauge 1

-- | Add to gauge state and report its current value
addGauge
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => Int64
  -> Gauge
  -> m ()
addGauge i = withGauge (`EKG.add` i)

-- | Decrement gauge state and report its current value
decrementGauge
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => Gauge
  -> m ()
decrementGauge = subtractGauge 1

-- | Subtract from gauge state and report its current value
subtractGauge
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => Int64
  -> Gauge
  -> m ()
subtractGauge i = withGauge (`EKG.subtract` i)

withGauge
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => (EKG.Gauge -> IO ())
  -> Gauge
  -> m ()
withGauge f Gauge {..} = do
  current <- liftIO $ do
    f gaugeAtomic
    EKG.read gaugeAtomic
  gauge gaugeName gaugeTags $ fromIntegral current

-- | Report the state of a gauge directly
--
-- This can be used by gauges where state is derived from other means.
--
gauge
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => Text
  -> [(Text, Text)]
  -> Double
  -> m ()
gauge name tags = sendAppMetricWithTags name tags Datadog.Gauge

histogram
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     , ToMetricValue n
     )
  => Text
  -> [(Text, Text)]
  -> n
  -> m ()
histogram name tags metricValue =
  sendAppMetricWithTags name tags Datadog.Histogram metricValue

histogramSince
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => Text
  -> [(Text, Text)]
  -> UTCTime
  -> m ()
histogramSince = histogramSinceBy toSeconds
  where
  -- N.B. NominalDiffTime is treated as seconds when using round. Replace round
  -- with nominalDiffTimeToSeconds once we upgrade our version of the time
  -- library.
        toSeconds = round @_ @Int

histogramSinceMs
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => Text
  -> [(Text, Text)]
  -> UTCTime
  -> m ()
histogramSinceMs = histogramSinceBy toMilliseconds
  where toMilliseconds = (* 1000) . realToFrac @_ @Double

histogramSinceBy
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     , ToMetricValue n
     )
  => (NominalDiffTime -> n)
  -> Text
  -> [(Text, Text)]
  -> UTCTime
  -> m ()
histogramSinceBy f name tags time = do
  now <- liftIO getCurrentTime
  let delta = f $ now `diffUTCTime` time
  sendAppMetricWithTags name tags Datadog.Histogram delta

sendAppMetricWithTags
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     , ToMetricValue v
     )
  => Text
  -> [(Text, Text)]
  -> MetricType
  -> v
  -> m ()
sendAppMetricWithTags name tags metricType metricValue = do
  mClient <- asks getDogStatsClient

  for_ mClient $ \client -> do
    appTags <- asks getDogStatsTags

    let
      ddTags = appTags <> map (uncurry tag) tags
      ddMetric = set Datadog.tags ddTags
        $ Datadog.metric (MetricName name) metricType metricValue

    send client ddMetric

envParseDogStatsEnabled :: Env.Parser Bool
envParseDogStatsEnabled = Env.switch "DOGSTATSD_ENABLED" $ Env.def False

envParseDogStatsSettings :: Env.Parser DogStatsSettings
envParseDogStatsSettings = do
  dogStatsSettingsHost <- Env.var Env.str "DOGSTATSD_HOST" $ Env.def "127.0.0.1"
  dogStatsSettingsPort <- Env.var Env.auto "DOGSTATSD_PORT" $ Env.def 8125
  dogStatsSettingsMaxDelay <-
    Env.var Env.auto "DOGSTATSD_MAX_DELAY_MICROSECONDS" $ Env.def 1000000
  pure defaultSettings
    { dogStatsSettingsHost
    , dogStatsSettingsPort
    , dogStatsSettingsMaxDelay
    }

envParseDogStatsTags :: Env.Parser [Tag]
envParseDogStatsTags =
  Env.var (map (uncurry tag) <$> Env.keyValues) "DOGSTATSD_TAGS" $ Env.def []
