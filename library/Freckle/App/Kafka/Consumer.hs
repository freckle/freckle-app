{-# LANGUAGE ApplicativeDo #-}

module Freckle.App.Kafka.Consumer
  ( HasKafkaConsumer (..)
  , withKafkaConsumer
  , KafkaConsumerConfig (..)
  , envKafkaConsumerConfig
  , runConsumer
  ) where

import Freckle.App.Prelude

import Blammo.Logging
import qualified Control.Immortal as Immortal
import Control.Lens (Lens', view)
import Data.Aeson
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Env
import Freckle.App.Env
import Freckle.App.Kafka (envKafkaBrokerAddresses)
import Kafka.Consumer hiding
  ( Timeout
  , closeConsumer
  , newConsumer
  , runConsumer
  , subscription
  )
import qualified Kafka.Consumer as Kafka
import UnliftIO.Exception (bracket, displayException, throwIO)

data KafkaConsumerConfig = KafkaConsumerConfig
  { kafkaConsumerConfigBrokerAddresses :: NonEmpty BrokerAddress
  -- ^ The list of host/port pairs for establishing the initial connection
  -- to the Kafka cluster.
  --
  -- This is the `bootstrap.servers` Kafka consumer configuration property.
  , kafkaConsumerConfigGroupId :: ConsumerGroupId
  -- ^ The consumer group id to which the consumer belongs.
  --
  -- This is the `group.id` Kafka consumer configuration property.
  , kafkaConsumerConfigTopics :: NonEmpty TopicName
  -- ^ The list of topic names that for which the consumer will subscribe.
  , kafkaConsumerConfigOffsetReset :: OffsetReset
  -- ^ The offset reset parameter used when there is no initial offset in Kafka.
  --
  -- This is the `auto.offset.reset` Kafka consumer configuration property.
  , kafkaConsumerConfigExtraSubscriptionProps :: Map Text Text
  -- ^ Extra properties used to configure the Kafka consumer.
  }
  deriving stock (Show)

envKafkaTopics
  :: Env.Parser Env.Error (NonEmpty TopicName)
envKafkaTopics =
  Env.var
    (eitherReader readKafkaTopics)
    "KAFKA_TOPICS"
    mempty

readKafkaTopics :: String -> Either String (NonEmpty TopicName)
readKafkaTopics t = case NE.nonEmpty $ T.splitOn "," $ T.pack t of
  Just xs@(x NE.:| _)
    | x /= "" -> Right $ TopicName <$> xs
  _ -> Left "Kafka topics cannot be empty"

envKafkaOffsetReset
  :: Env.Parser Env.Error OffsetReset
envKafkaOffsetReset =
  Env.var
    (eitherReader readKafkaOffsetReset)
    "KAFKA_OFFSET_RESET"
    $ Env.def Earliest

readKafkaOffsetReset :: String -> Either String OffsetReset
readKafkaOffsetReset t = case T.pack t of
  "earliest" -> Right Earliest
  "latest" -> Right Latest
  _ -> Left "Kafka offset reset must be one of earliest or latest"

envKafkaConsumerConfig
  :: Env.Parser Env.Error KafkaConsumerConfig
envKafkaConsumerConfig = do
  brokerAddresses <- envKafkaBrokerAddresses
  consumerGroupId <- Env.var Env.nonempty "KAFKA_CONSUMER_GROUP_ID" mempty
  kafkaTopics <- envKafkaTopics
  kafkaOffsetReset <- envKafkaOffsetReset
  kafkaExtraProps <-
    Env.var
      (fmap Map.fromList . keyValues)
      "KAFKA_EXTRA_SUBSCRIPTION_PROPS"
      (Env.def mempty)
  pure $
    KafkaConsumerConfig
      brokerAddresses
      consumerGroupId
      kafkaTopics
      kafkaOffsetReset
      kafkaExtraProps

class HasKafkaConsumer env where
  kafkaConsumerL :: Lens' env KafkaConsumer

consumerProps :: KafkaConsumerConfig -> ConsumerProperties
consumerProps KafkaConsumerConfig {..} =
  brokersList brokers
    <> groupId kafkaConsumerConfigGroupId
    <> noAutoCommit
    <> logLevel KafkaLogInfo
 where
  brokers = NE.toList kafkaConsumerConfigBrokerAddresses

subscription :: KafkaConsumerConfig -> Subscription
subscription KafkaConsumerConfig {..} =
  topics (NE.toList kafkaConsumerConfigTopics)
    <> offsetReset kafkaConsumerConfigOffsetReset
    <> extraSubscriptionProps kafkaConsumerConfigExtraSubscriptionProps

withKafkaConsumer
  :: MonadUnliftIO m
  => KafkaConsumerConfig
  -> (KafkaConsumer -> m a)
  -> m a
withKafkaConsumer config = bracket newConsumer closeConsumer
 where
  (props, sub) = (consumerProps &&& subscription) config
  newConsumer = either throwIO pure =<< Kafka.newConsumer props sub
  closeConsumer = maybe (pure ()) throwIO <=< Kafka.closeConsumer

timeoutMs :: Timeout -> Int
timeoutMs = \case
  TimeoutSeconds s -> s * 1000
  TimeoutMilliseconds ms -> ms

runConsumer
  :: ( MonadUnliftIO m
     , MonadReader env m
     , MonadLogger m
     , HasKafkaConsumer env
     , FromJSON a
     )
  => Timeout
  -> (a -> m ())
  -> m ()
runConsumer pollTimeout onMessage = void $ Immortal.create $ \thread -> Immortal.onUnexpectedFinish thread handleException $ do
  consumer <- view kafkaConsumerL
  eMessage <-
    pollMessage consumer $ Kafka.Timeout $ timeoutMs pollTimeout
  case eMessage of
    Left (KafkaResponseError RdKafkaRespErrTimedOut) -> logInfo $ "Polling timeout"
    Left err -> logError $ "Error polling for message from Kafka" :# ["error" .= show err]
    Right (ConsumerRecord {..}) -> for_ crValue $ \bs -> do
      case eitherDecodeStrict bs of
        Left err -> logError $ "Could not decode message value" :# ["error" .= err]
        Right a -> onMessage a
  maybe
    (logInfo "Committed offsets")
    (\err -> logError $ "Error committing offsets" :# ["error" .= show err])
    =<< commitAllOffsets OffsetCommit consumer
 where
  handleException = \case
    Left ex ->
      logError $
        "Exception occurred in runConsumer" :# ["exception" .= displayException ex]
    Right () -> pure ()
