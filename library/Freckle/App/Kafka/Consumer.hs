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
import Control.Lens (Lens', view)
import Control.Monad (forever)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Env qualified
import Freckle.App.Async
import Freckle.App.Env
import Freckle.App.Exception (annotatedExceptionMessageFrom)
import Freckle.App.Kafka.Producer (envKafkaBrokerAddresses)
import Freckle.App.OpenTelemetry
import Freckle.App.OpenTelemetry.ThreadContext
import Kafka.Consumer hiding
  ( Timeout
  , closeConsumer
  , newConsumer
  , runConsumer
  , subscription
  )
import Kafka.Consumer qualified as Kafka
import UnliftIO.Exception (bracket)

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
  , kafkaConsumerConfigTopic :: TopicName
  -- ^ The topic name polled for messages by the Kafka consumer.
  , kafkaConsumerConfigOffsetReset :: OffsetReset
  -- ^ The offset reset parameter used when there is no initial offset in Kafka.
  --
  -- This is the `auto.offset.reset` Kafka consumer configuration property.
  , kafkaConsumerConfigAutoCommitInterval :: Millis
  -- ^ The interval that offsets are auto-committed to Kafka.
  --
  -- This sets the `auto.commit.interval.ms` and `enable.auto.commit` Kafka
  -- consumer configuration properties.
  , kafkaConsumerConfigExtraSubscriptionProps :: Map Text Text
  -- ^ Extra properties used to configure the Kafka consumer.
  }
  deriving stock (Show)

envKafkaTopic
  :: Env.Parser Env.Error TopicName
envKafkaTopic =
  Env.var
    (eitherReader readKafkaTopic)
    "KAFKA_TOPIC"
    mempty

readKafkaTopic :: String -> Either String TopicName
readKafkaTopic t = case T.pack t of
  "" -> Left "Kafka topics cannot be empty"
  x -> Right $ TopicName x

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
  kafkaTopic <- envKafkaTopic
  kafkaOffsetReset <- envKafkaOffsetReset
  kafkaAutoOffsetInterval <-
    fromIntegral . timeoutMs <$$> Env.var timeout "KAFKA_AUTO_COMMIT_INTERVAL" $
      Env.def $
        TimeoutMilliseconds 5000
  kafkaExtraProps <-
    Env.var
      (fmap Map.fromList . keyValues)
      "KAFKA_EXTRA_SUBSCRIPTION_PROPS"
      (Env.def mempty)
  pure $
    KafkaConsumerConfig
      brokerAddresses
      consumerGroupId
      kafkaTopic
      kafkaOffsetReset
      kafkaAutoOffsetInterval
      kafkaExtraProps

class HasKafkaConsumer env where
  kafkaConsumerL :: Lens' env KafkaConsumer

consumerProps :: KafkaConsumerConfig -> ConsumerProperties
consumerProps KafkaConsumerConfig {..} =
  brokersList brokers
    <> groupId kafkaConsumerConfigGroupId
    <> autoCommit kafkaConsumerConfigAutoCommitInterval
    <> logLevel KafkaLogInfo
 where
  brokers = NE.toList kafkaConsumerConfigBrokerAddresses

subscription :: KafkaConsumerConfig -> Subscription
subscription KafkaConsumerConfig {..} =
  topics [kafkaConsumerConfigTopic]
    <> offsetReset kafkaConsumerConfigOffsetReset
    <> extraSubscriptionProps kafkaConsumerConfigExtraSubscriptionProps

withKafkaConsumer
  :: (MonadUnliftIO m, HasCallStack)
  => KafkaConsumerConfig
  -> (KafkaConsumer -> m a)
  -> m a
withKafkaConsumer config = bracket newConsumer closeConsumer
 where
  (props, sub) = (consumerProps &&& subscription) config
  newConsumer = either throwM pure =<< Kafka.newConsumer props sub
  closeConsumer = maybe (pure ()) throwM <=< Kafka.closeConsumer

timeoutMs :: Timeout -> Int
timeoutMs = \case
  TimeoutSeconds s -> s * 1000
  TimeoutMilliseconds ms -> ms

data KafkaMessageDecodeError = KafkaMessageDecodeError
  { input :: ByteString
  , errors :: String
  }
  deriving stock (Show)

instance Exception KafkaMessageDecodeError where
  displayException KafkaMessageDecodeError {..} =
    mconcat
      [ "Unable to decode JSON"
      , "\n  input:  " <> unpack (decodeUtf8 input)
      , "\n  errors: " <> errors
      ]

runConsumer
  :: ( MonadMask m
     , MonadUnliftIO m
     , MonadReader env m
     , MonadLogger m
     , MonadTracer m
     , HasKafkaConsumer env
     , FromJSON a
     , HasCallStack
     )
  => Timeout
  -> (a -> m ())
  -> m ()
runConsumer pollTimeout onMessage =
  withTraceContext $ immortalCreateLogged $ forever $ do
    consumer <- view kafkaConsumerL

    flip catches handlers $ inSpan "kafka.consumer" consumerSpanArguments $ do
      mRecord <- fromKafkaError =<< pollMessage consumer kTimeout

      for_ (crValue =<< mRecord) $ \bs -> do
        a <-
          inSpan "kafka.consumer.message.decode" defaultSpanArguments $
            either (throwM . KafkaMessageDecodeError bs) pure $
              eitherDecodeStrict bs
        inSpan "kafka.consumer.message.handle" defaultSpanArguments $ onMessage a
 where
  kTimeout = Kafka.Timeout $ timeoutMs pollTimeout

  handlers =
    [ ExceptionHandler $
        logErrorNS "kafka"
          . annotatedExceptionMessageFrom @KafkaError
            (const "Error polling for message from Kafka")
    , ExceptionHandler $
        logErrorNS "kafka"
          . annotatedExceptionMessageFrom @KafkaMessageDecodeError
            (const "Could not decode message value")
    ]

fromKafkaError
  :: (MonadIO m, MonadLogger m, HasCallStack)
  => Either KafkaError a
  -> m (Maybe a)
fromKafkaError =
  either
    ( \case
        KafkaResponseError RdKafkaRespErrTimedOut ->
          Nothing <$ logDebug "Polling timeout"
        err -> throwM err
    )
    $ pure . Just
