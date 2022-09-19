{-# LANGUAGE TupleSections #-}

-- | An intentionally-leaky StatsD interface to Datadog
--
-- $usage
--
module Freckle.App.Stats
  ( StatsSettings
  , defaultStatsSettings
  , setStatsSettingsTags
  , envParseStatsSettings

  -- * Client
  , StatsClient
  , tagsL
  , withStatsClient
  , HasStatsClient(..)

  -- * Gauges
  , Gauges
  , GaugeName
  , dbConnections
  , withGauge

  -- * Reporting
  , tagged
  , increment
  , counter
  , gauge
  , histogram
  , histogramSince
  , histogramSinceMs
  ) where

import Freckle.App.Prelude

import Blammo.Logging
import Control.Lens (Lens', lens, to, view, (&), (.~), (<>~))
import Control.Monad.Reader (asks, local)
import Data.Aeson (Value(..))
import Data.String
import Data.Time (diffUTCTime)
import Freckle.App.Ecs
import qualified Freckle.App.Env as Env
import qualified Network.StatsD.Datadog as Datadog
import qualified System.Metrics.Gauge as EKG
import UnliftIO.Exception (bracket_)
import Yesod.Core.Lens
import Yesod.Core.Types (HandlerData)

data StatsSettings = StatsSettings
  { amsEnabled :: Bool
  , amsSettings :: Datadog.DogStatsSettings
  , amsTags :: [(Text, Text)]
  }

defaultStatsSettings :: StatsSettings
defaultStatsSettings = StatsSettings
  { amsEnabled = False
  , amsSettings = Datadog.defaultSettings
  , amsTags = []
  }

setStatsSettingsTags :: [(Text, Text)] -> StatsSettings -> StatsSettings
setStatsSettingsTags x settings = settings { amsTags = x }

envParseStatsSettings :: Env.Parser Env.Error StatsSettings
envParseStatsSettings =
  StatsSettings
    <$> Env.switch "DOGSTATSD_ENABLED" mempty
    <*> (buildSettings
        <$> optional (Env.var Env.str "DOGSTATSD_HOST" mempty)
        <*> optional (Env.var Env.auto "DOGSTATSD_PORT" mempty)
        )
    <*> (buildTags
        <$> optional (Env.var Env.nonempty "DD_ENV" mempty)
        <*> optional (Env.var Env.nonempty "DD_SERVICE" mempty)
        <*> optional (Env.var Env.nonempty "DD_VERSION" mempty)
        <*> Env.var Env.keyValues "DOGSTATSD_TAGS" (Env.def [])
        )
 where
  buildSettings mHost mPort =
    Datadog.defaultSettings
      & maybe id (Datadog.host .~) mHost
      . maybe id (Datadog.port .~) mPort

  buildTags mEnv mService mVersion tags =
    catMaybes
        [ ("env", ) <$> mEnv
        , ("environment", ) <$> mEnv -- Legacy
        , ("service", ) <$> mService
        , ("version", ) <$> mVersion
        ]
      <> tags

-- should be data but newtype for now because there's only one and only one planned
newtype Gauges = Gauges
  { gdbConnections :: EKG.Gauge
  -- ^ Track open db connections
  }

-- Opaque wrapper to hide EKG.Gauge
newtype GaugeName = GaugeName
  { unGaugeName :: Gauges -> EKG.Gauge
  }

dbConnections :: GaugeName
dbConnections = GaugeName gdbConnections

data StatsClient = StatsClient
  { scClient :: Datadog.StatsClient
  , scTags :: [(Text, Text)]
  , scGauges :: Gauges
  }

tagsL :: Lens' StatsClient [(Text, Text)]
tagsL = lens scTags $ \x y -> x { scTags = y }

gaugesL :: Lens' StatsClient Gauges
gaugesL = lens scGauges $ \x y -> x { scGauges = y }

class HasStatsClient env where
  statsClientL :: Lens' env StatsClient

instance HasStatsClient StatsClient where
  statsClientL = id

instance HasStatsClient site =>  HasStatsClient (HandlerData child site) where
  statsClientL = envL . siteL . statsClientL

withStatsClient
  :: (MonadMask m, MonadUnliftIO m)
  => StatsSettings
  -> (StatsClient -> m a)
  -> m a
withStatsClient StatsSettings {..} f = do
  gauges <- liftIO $ fmap Gauges EKG.new
  if amsEnabled
    then do
      tags <- (amsTags <>) <$> getEcsMetadataTags
      Datadog.withDogStatsD amsSettings $ \client ->
        -- Add the tags to the thread context so they're present in all logs
        withThreadContext (map toPair tags)
          $ f StatsClient { scClient = client, scTags = tags, scGauges = gauges }
    else do
      f $ StatsClient { scClient = Datadog.Dummy, scTags = amsTags, scGauges = gauges }
  where toPair = bimap (fromString . unpack) String

withGauge :: (MonadReader app m, HasStatsClient app, MonadUnliftIO m) => GaugeName -> m a -> m a
withGauge gaugeName f = do
  gauge' <- view $ statsClientL . gaugesL . to (unGaugeName gaugeName)
  bracket_ (plus1 gauge') (minus1 gauge') f
  where
    plus1 g = liftIO $ EKG.add g 1
    minus1 g = liftIO $ EKG.subtract g 1

-- | Include the given tags on all metrics emitted from a block
tagged
  :: (MonadReader env m, HasStatsClient env) => [(Text, Text)] -> m a -> m a
tagged tags = local $ statsClientL . tagsL <>~ tags

-- | Synonym for @'counter' 1@
increment
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env) => Text -> m ()
increment name = counter name 1

counter
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env)
  => Text
  -> Int
  -> m ()
counter = sendMetric Datadog.Counter

gauge
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env)
  => Text
  -> Double
  -> m ()
gauge = sendMetric Datadog.Gauge

-- | Emit an elapsed duration (which Datadog calls a /histogram/)
--
-- The 'ToMetricValue' constraint can be satisfied by most numeric types and is
-- assumed to be seconds.
--
histogram
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasStatsClient env
     , Datadog.ToMetricValue n
     )
  => Text
  -> n
  -> m ()
histogram = sendMetric Datadog.Histogram

histogramSince
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env)
  => Text
  -> UTCTime
  -> m ()
histogramSince = histogramSinceBy toSeconds where toSeconds = round @_ @Int

histogramSinceMs
  :: (MonadUnliftIO m, MonadReader env m, HasStatsClient env)
  => Text
  -> UTCTime
  -> m ()
histogramSinceMs = histogramSinceBy toMilliseconds
  where toMilliseconds = (* 1000) . realToFrac @_ @Double

histogramSinceBy
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasStatsClient env
     , Datadog.ToMetricValue n
     )
  => (NominalDiffTime -> n)
  -> Text
  -> UTCTime
  -> m ()
histogramSinceBy f name time = do
  now <- liftIO getCurrentTime
  let delta = f $ now `diffUTCTime` time
  sendMetric Datadog.Histogram name delta

sendMetric
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasStatsClient env
     , Datadog.ToMetricValue v
     )
  => Datadog.MetricType
  -> Text
  -> v
  -> m ()
sendMetric metricType name metricValue = do
  StatsClient {..} <- view statsClientL

  Datadog.send scClient
    $ Datadog.metric (Datadog.MetricName name) metricType metricValue
    & (Datadog.tags .~ map (uncurry Datadog.tag) scTags)

getEcsMetadataTags :: MonadIO m => m [(Text, Text)]
getEcsMetadataTags = maybe [] toTags <$> getEcsMetadata
 where
  toTags (EcsMetadata EcsContainerMetadata {..} EcsContainerTaskMetadata {..})
    = [ ("container_id", ecmDockerId)
      , ("container_name", ecmDockerName)
      , ("docker_image", ecmImage)
      , ("image_tag", ecmImageID)
      , ("cluster_name", ectmCluster)
      , ("task_arn", ectmTaskARN)
      , ("task_family", ectmFamily)
      , ("task_version", ectmRevision)
      ]

-- $usage
-- Usage:
--
-- - Use 'envParseStatsSettings' to configure things
--
--   @
--   data AppSettings = AppSettings
--    { -- ...
--    , appStatsSettings :: StatsSettings
--    }
--
--   loadSettings :: IO AppSettings
--   loadSettings = Env.parse id $ AppSettings
--     <$> -- ...
--     <*> 'envParseStatsSettings'
--   @
--
--   This will read,
--
--   - @DOGSTATSD_ENABLED=x@
--   - @DOGSTATSD_HOST=127.0.0.1@
--   - @DOGSTATSD_PORT=8125@
--   - @DOGSTATSD_TAGS=[<key>:<value>,...]@
--   - Optionally @DD_ENV@, @DD_SERVICE@, and @DD_VERSION@
--
-- - Give your @App@ a 'HasStatsClient' instance
--
--   @
--   data App = App
--     { -- ...
--     , appStatsClient :: 'StatsClient'
--     }
--
--   instance 'HasStatsClient' App where
--     'statsClientL' = lens appStatsClient $ \x y -> { appStatsClient = y }
--   @
--
-- - Use 'withStatsClient' to build and store a client on your @App@ when you
--   run it
--
--   @
--   'withStatsClient' appStatsSettings $ \client -> do
--     app <- App
--       <$> ...
--       <*> pure client
--
--     'runApp' app $ ...
--   @
--
-- - Throughout your application code, emit metrics as desired
--
--   @
--   import qualified Freckle.App.Stats as Stats
--
--   myFunction :: (MonadIO m, MonadReader env m, 'HasStatsClient' env) => m ()
--   myFunction = do
--     start <- liftIO getCurrentTime
--     result <- myAction
--
--     Stats.'increment' \"action.attempt\"
--     Stats.'histogramSinceMs' \"action.duration\" start
--
--     case result of
--       Left err -> do
--         Stats.'increment' \"action.failure\"
--         -- ...
--       Right x -. do
--         Stats.'increment' \"action.success\"
--         -- ...
--   @
--
