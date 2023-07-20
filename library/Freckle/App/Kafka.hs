{-# LANGUAGE NamedFieldPuns #-}

module Freckle.App.Kafka
  ( BrokerAddress(..)
  , HasKafkaBrokerAddress (..)
  , envKafkaBrokerAddress
  
  , KafkaProducer (..)
  , HasKafkaProducer (..)
  , kafkaProducer

  , produceAsync

  ) where

import Freckle.App.Prelude

import Blammo.Logging
import Control.Lens (Lens', view)
import Data.Aeson (ToJSON, encode)
import Data.ByteString (toStrict)
import qualified Data.List.NonEmpty as NE
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Data.Text as T
import qualified Freckle.App.Env as Env
import qualified Kafka.Producer as Kafka
import UnliftIO.Async (async)
import UnliftIO.Exception (throwString)
import Yesod.Core.Lens
import Yesod.Core.Types (HandlerData)

newtype BrokerAddress = BrokerAddress { getBrokerAddress :: Maybe (NonEmpty Kafka.BrokerAddress)}

class HasKafkaBrokerAddress env where
  kafkaBrokerAddressL :: Lens' env BrokerAddress

instance HasKafkaBrokerAddress site => HasKafkaBrokerAddress (HandlerData child site) where
  kafkaBrokerAddressL = envL . siteL . kafkaBrokerAddressL

envKafkaBrokerAddress
  :: Env.Parser Env.Error BrokerAddress
envKafkaBrokerAddress = BrokerAddress <$> addressOrNothing
  
 where
  addressOrNothing = (parseKafkaBrokerAddress <$> parseKey) <|> pure Nothing
  parseKey = Env.var Env.nonempty "KAFKA_BROKER_ADDRESS" mempty
  parseKafkaBrokerAddress = NE.nonEmpty . fmap Kafka.BrokerAddress . T.splitOn ","

data KafkaProducer
  = NullKafkaProducer
  | KafkaProducerPool (Pool Kafka.KafkaProducer)

class HasKafkaProducer env where
  kafkaProducerL :: Lens' env KafkaProducer

instance HasKafkaProducer site => HasKafkaProducer (HandlerData child site) where
  kafkaProducerL = envL . siteL . kafkaProducerL

kafkaProducer
  :: ( MonadReader env m
     , HasKafkaBrokerAddress env
     , MonadUnliftIO m
     )
  => Int
  -- ^ The number of stripes (distinct sub-pools) to maintain.
  -- The smallest acceptable value is 1.
  -> NominalDiffTime
  -- ^ Amount of time for which an unused resource is kept open.
  -- The smallest acceptable value is 0.5 seconds.
  --
  -- The elapsed time before destroying a resource may be a little
  -- longer than requested, as the reaper thread wakes at 1-second
  -- intervals.
  -> Int
  -- ^ Maximum number of resources to keep open per stripe.  The
  -- smallest acceptable value is 1.
  --
  -- Requests for resources will block if this limit is reached on a
  -- single stripe, even if other stripes have idle resources
  -- available.
  -> m KafkaProducer
kafkaProducer numStripes idleTime maxResources =
  view kafkaBrokerAddressL >>= \case
    BrokerAddress Nothing -> pure NullKafkaProducer
    BrokerAddress (Just addresses) -> do
      let
        pool =
          Pool.createPool mkProducer Kafka.closeProducer numStripes idleTime maxResources
        mkProducer =
          either throw pure =<< Kafka.newProducer (Kafka.brokersList $ toList addresses)
        throw err = throwString $ "Failed to open kafka producer: " <> show err
      KafkaProducerPool
        <$> liftIO pool

produceAsync
  :: ( ToJSON value
     , ToJSON key
     , MonadLogger m
     , MonadReader env m
     , HasKafkaProducer env
     , MonadUnliftIO m
     )
  => Kafka.TopicName
  -> NonEmpty value
  -> (value -> key)
  -> m ()
produceAsync prTopic values keyF = void $ async $ do
  logDebugNS "kafka" $ "Producing Kafka events" :# ["events" .= values]
  view kafkaProducerL >>= \case
    NullKafkaProducer -> pure ()
    KafkaProducerPool producerPool -> do
      errors <-
        liftIO $
          Pool.withResource producerPool $ \producer ->
            Kafka.produceMessageBatch producer $
              toList $
                mkProducerRecord <$> values
      unless (null errors) $
        logErrorNS "kafka" $
          "Failed to send events" :# ["errors" .= fmap (tshow . snd) errors]
 where
  mkProducerRecord value =
    Kafka.ProducerRecord
      { prTopic
      , prPartition = Kafka.UnassignedPartition
      , prKey = Just $ toStrict $ encode $ keyF value
      , prValue =
          Just $
            toStrict $
              encode value
      }
