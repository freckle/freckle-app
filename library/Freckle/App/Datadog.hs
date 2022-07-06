{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Datadog access for your @App@
module Freckle.App.Datadog
  ( HasDogStatsClient(..)
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

  -- * Reading settings at startup
  , DogStatsSettings(..)
  , envParseDogStatsEnabled
  , envParseDogStatsSettings
  , envParseDogStatsTags
  , getEcsMetadataTags
  , withTagsAsThreadContext
  , mkStatsClient

  -- * To be removed in next major bump
  , guage
  ) where

import Freckle.App.Prelude

import Blammo.Logging (MonadMask, Pair, withThreadContext)
import Control.Lens (set)
import Control.Monad.Reader
import Data.Aeson (Value(..))
import Data.String
import Data.Time (diffUTCTime)
import Freckle.App.Ecs
import qualified Freckle.App.Env as Env
import Network.StatsD.Datadog hiding (Tag, metric, name, tag, tags)
import qualified Network.StatsD.Datadog as Datadog
import Yesod.Core.Types (HandlerData, handlerEnv, rheSite)

data Tag = Tag
  { tagKey :: Text
  , tagValue :: Text
  }

tagToDatadogTag :: Tag -> Datadog.Tag
tagToDatadogTag Tag {..} = Datadog.tag tagKey tagValue

tagToPair :: Tag -> Pair
tagToPair Tag {..} = (fromString $ unpack tagKey, String tagValue)

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
counter name tags = sendAppMetricWithTags name tags Counter

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
gauge name tags = sendAppMetricWithTags name tags Gauge

{-# DEPRECATED guage "Use gauge instead" #-}
-- | Deprecated typo version of 'gauge'
guage
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasDogStatsClient env
     , HasDogStatsTags env
     )
  => Text
  -> [(Text, Text)]
  -> Double
  -> m ()
guage = gauge

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
  sendAppMetricWithTags name tags Histogram metricValue

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
  sendAppMetricWithTags name tags Histogram delta

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
      ddTags = map tagToDatadogTag $ appTags <> map (uncurry Tag) tags
      ddMetric = set Datadog.tags ddTags
        $ Datadog.metric (MetricName name) metricType metricValue

    send client ddMetric

envParseDogStatsEnabled :: Env.Parser Env.Error Bool
envParseDogStatsEnabled = Env.switch "DOGSTATSD_ENABLED" mempty

envParseDogStatsSettings :: Env.Parser Env.Error DogStatsSettings
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

envParseDogStatsTags :: Env.Parser Env.Error [Tag]
envParseDogStatsTags =
  build
    <$> optional (Env.var Env.nonempty "DD_ENV" mempty)
    <*> optional (Env.var Env.nonempty "DD_SERVICE" mempty)
    <*> optional (Env.var Env.nonempty "DD_VERSION" mempty)
    <*> Env.var Env.keyValues "DOGSTATSD_TAGS" (Env.def [])
 where
  build mEnv mService mVersion tags =
    catMaybes
        [ Tag "env" <$> mEnv
        , Tag "environment" <$> mEnv -- Legacy
        , Tag "service" <$> mService
        , Tag "version" <$> mVersion
        ]
      <> map (uncurry Tag) tags

getEcsMetadataTags :: MonadIO m => m [Tag]
getEcsMetadataTags = maybe [] toTags <$> getEcsMetadata
 where
  toTags (EcsMetadata EcsContainerMetadata {..} EcsContainerTaskMetadata {..})
    = [ Tag "container_id" ecmDockerId
      , Tag "container_name" ecmDockerName
      , Tag "docker_image" ecmImage
      , Tag "image_tag" ecmImageID
      , Tag "cluster_name" ectmCluster
      , Tag "task_arn" ectmTaskARN
      , Tag "task_family" ectmFamily
      , Tag "task_version" ectmRevision
      ]

withTagsAsThreadContext
  :: (MonadMask m, MonadIO m, MonadReader env m, HasDogStatsTags env)
  => m a
  -> m a
withTagsAsThreadContext f = do
  tags <- asks getDogStatsTags
  withThreadContext (map tagToPair tags) f
