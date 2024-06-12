{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Freckle.App.Kafka.Producer
  ( envKafkaBrokerAddresses
  , KafkaProducerPoolConfig (..)
  , envKafkaProducerPoolConfig
  , KafkaProducerPool (..)
  , HasKafkaProducerPool (..)
  , createKafkaProducerPool
  , produceKeyedOn
  , produceKeyedOnAsync
  ) where

import Freckle.App.Prelude

import Blammo.Logging
import Control.Lens (Lens', view)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy (toStrict)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty qualified as NE
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Text qualified as T
import Freckle.App.Async (async)
import Freckle.App.Env qualified as Env
import Freckle.App.OpenTelemetry
import Kafka.Producer
import OpenTelemetry.Trace qualified as Trace
import UnliftIO (withRunInIO)
import Yesod.Core.Lens
import Yesod.Core.Types (HandlerData)

envKafkaBrokerAddresses
  :: Env.Parser Env.Error (NonEmpty BrokerAddress)
envKafkaBrokerAddresses =
  Env.var
    (Env.eitherReader readKafkaBrokerAddresses)
    "KAFKA_BROKER_ADDRESSES"
    mempty

readKafkaBrokerAddresses :: String -> Either String (NonEmpty BrokerAddress)
readKafkaBrokerAddresses t = case NE.nonEmpty $ T.splitOn "," $ T.pack t of
  Just xs@(x NE.:| _)
    | x /= "" -> Right $ BrokerAddress <$> xs
  _ -> Left "Broker Address cannot be empty"

data KafkaProducerPoolConfig = KafkaProducerPoolConfig
  { kafkaProducerPoolConfigStripes :: Int
  -- ^ The number of stripes (distinct sub-pools) to maintain.
  -- The smallest acceptable value is 1.
  , kafkaProducerPoolConfigIdleTimeout :: NominalDiffTime
  -- ^ Amount of time for which an unused resource is kept open.
  -- The smallest acceptable value is 0.5 seconds.
  --
  -- The elapsed time before destroying a resource may be a little
  -- longer than requested, as the reaper thread wakes at 1-second
  -- intervals.
  , kafkaProducerPoolConfigSize :: Int
  -- ^ Maximum number of resources to keep open per stripe.  The
  -- smallest acceptable value is 1.
  --
  -- Requests for resources will block if this limit is reached on a
  -- single stripe, even if other stripes have idle resources
  -- available.
  }
  deriving stock (Show)

-- | Same defaults as 'Database.Persist.Sql.ConnectionPoolConfig'
defaultKafkaProducerPoolConfig :: KafkaProducerPoolConfig
defaultKafkaProducerPoolConfig = KafkaProducerPoolConfig 1 600 10

envKafkaProducerPoolConfig
  :: Env.Parser Env.Error KafkaProducerPoolConfig
envKafkaProducerPoolConfig = do
  poolSize <- Env.var Env.auto "KAFKA_PRODUCER_POOL_SIZE" $ Env.def 10
  pure $ defaultKafkaProducerPoolConfig {kafkaProducerPoolConfigSize = poolSize}

data KafkaProducerPool
  = NullKafkaProducerPool
  | KafkaProducerPool (Pool KafkaProducer)

class HasKafkaProducerPool env where
  kafkaProducerPoolL :: Lens' env KafkaProducerPool

instance HasKafkaProducerPool site => HasKafkaProducerPool (HandlerData child site) where
  kafkaProducerPoolL = envL . siteL . kafkaProducerPoolL

createKafkaProducerPool
  :: NonEmpty BrokerAddress
  -> KafkaProducerPoolConfig
  -> IO (Pool KafkaProducer)
createKafkaProducerPool addresses KafkaProducerPoolConfig {..} =
  Pool.newPool $
    Pool.setNumStripes (Just kafkaProducerPoolConfigStripes) $
      Pool.defaultPoolConfig
        mkProducer
        closeProducer
        (realToFrac kafkaProducerPoolConfigIdleTimeout)
        kafkaProducerPoolConfigSize
 where
  mkProducer =
    either
      (\err -> throwString ("Failed to open kafka producer: " <> show err))
      pure
      =<< newProducer (brokersList $ toList addresses)

produceKeyedOn
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadTracer m
     , MonadReader env m
     , HasKafkaProducerPool env
     , ToJSON key
     , ToJSON value
     )
  => TopicName
  -> NonEmpty value
  -> (value -> key)
  -> m ()
produceKeyedOn prTopic values keyF = traced $ do
  logDebugNS "kafka" $ "Producing Kafka events" :# ["events" .= values]
  view kafkaProducerPoolL >>= \case
    NullKafkaProducerPool -> pure ()
    KafkaProducerPool producerPool ->
      withRunInIO $ \run ->
        Pool.withResource producerPool $ \producer ->
          for_ @NonEmpty values $ \value -> do
            mError <- liftIO $ produceMessage producer $ mkProducerRecord value
            for_ @Maybe mError $ \e ->
              run $ logErrorNS "kafka" $ "Failed to send event" :# ["error" .= tshow e]
 where
  mkProducerRecord value =
    ProducerRecord
      { prTopic
      , prPartition = UnassignedPartition
      , prKey = Just $ toStrict $ encode $ keyF value
      , prValue = Just $ toStrict $ encode value
      , prHeaders = mempty
      }

  traced =
    inSpan
      "kafka.produce"
      producerSpanArguments
        { Trace.attributes =
            HashMap.fromList
              [ ("service.name", "kafka")
              , ("topic", Trace.toAttribute $ unTopicName prTopic)
              ]
        }

produceKeyedOnAsync
  :: ( MonadMask m
     , MonadUnliftIO m
     , MonadLogger m
     , MonadTracer m
     , MonadReader env m
     , HasKafkaProducerPool env
     , ToJSON key
     , ToJSON value
     )
  => TopicName
  -> NonEmpty value
  -> (value -> key)
  -> m ()
produceKeyedOnAsync prTopic values = void . async . produceKeyedOn prTopic values
