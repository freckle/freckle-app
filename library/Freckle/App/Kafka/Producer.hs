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
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NE
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Data.Text as T
import Freckle.App.Async (async)
import qualified Freckle.App.Env as Env
import Freckle.App.OpenTelemetry
import Kafka.Producer
import qualified OpenTelemetry.Trace as Trace
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
    KafkaProducerPool producerPool -> do
      errors <-
        liftIO $
          Pool.withResource producerPool $ \producer ->
            produceMessageBatch producer $
              toList $
                mkProducerRecord <$> values
      unless (null errors) $
        logErrorNS "kafka" $
          "Failed to send events" :# ["errors" .= fmap (tshow . snd) errors]
 where
  mkProducerRecord value =
    ProducerRecord
      { prTopic
      , prPartition = UnassignedPartition
      , prKey = Just $ toStrict $ encode $ keyF value
      , prValue =
          Just $
            toStrict $
              encode value
      }

  traced =
    inSpan
      "kafka.produce"
      producerSpanArguments
        { Trace.attributes =
            HashMap.fromList
              [("topic", Trace.toAttribute $ unTopicName prTopic)]
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
