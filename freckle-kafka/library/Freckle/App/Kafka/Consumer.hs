{-# LANGUAGE ApplicativeDo #-}

module Freckle.App.Kafka.Consumer
  ( HasKafkaConsumer (..)
  , withKafkaConsumer
  , KafkaConsumerConfig (..)
  , envKafkaConsumerConfig
  , runConsumer
  ) where

import Relude

import Blammo.Logging
import Control.Exception.Annotated.UnliftIO (AnnotatedException)
import Control.Exception.Annotated.UnliftIO qualified as Annotated
import Control.Lens (Lens', view)
import Data.Aeson
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Freckle.App.Env (Timeout (..))
import Freckle.App.Env qualified as Env
import Freckle.App.Kafka.Producer (envKafkaBrokerAddresses)
import Kafka.Consumer hiding
  ( Timeout
  , closeConsumer
  , newConsumer
  , runConsumer
  , subscription
  )
import Kafka.Consumer qualified as Kafka
import OpenTelemetry.Trace (SpanKind (..), defaultSpanArguments)
import OpenTelemetry.Trace qualified as Trace
import OpenTelemetry.Trace.Monad (MonadTracer, inSpan)
import UnliftIO (MonadUnliftIO)
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
    (Env.eitherReader readKafkaTopic)
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
    (Env.eitherReader readKafkaOffsetReset)
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
    fmap (fromIntegral . timeoutMs)
      <$> Env.var Env.timeout "KAFKA_AUTO_COMMIT_INTERVAL"
      $ Env.def
      $ TimeoutMilliseconds 5000
  kafkaExtraProps <-
    Env.var
      (fmap Map.fromList . Env.keyValues)
      "KAFKA_EXTRA_SUBSCRIPTION_PROPS"
      (Env.def mempty)
  pure
    $ KafkaConsumerConfig
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
  newConsumer = either Annotated.throw pure =<< Kafka.newConsumer props sub
  closeConsumer = maybe (pure ()) Annotated.throw <=< Kafka.closeConsumer

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
      , "\n  input:  " <> decodeUtf8 input
      , "\n  errors: " <> errors
      ]

runConsumer
  :: ( MonadUnliftIO m
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
  forever $ do
    consumer <- view kafkaConsumerL

    flip Annotated.catches handlers
      $ inSpan
        "kafka.consumer"
        (defaultSpanArguments {Trace.kind = Consumer})
      $ do
        mRecord <- fromKafkaError =<< pollMessage consumer kTimeout

        for_ (crValue =<< mRecord) $ \bs -> do
          a <-
            inSpan "kafka.consumer.message.decode" defaultSpanArguments
              $ either (Annotated.throw . KafkaMessageDecodeError bs) pure
              $ eitherDecodeStrict bs
          inSpan "kafka.consumer.message.handle" defaultSpanArguments $ onMessage a
 where
  kTimeout = Kafka.Timeout $ timeoutMs pollTimeout

  handlers =
    [ Annotated.Handler
        $ logErrorNS "kafka"
        . annotatedExceptionMessageFrom @KafkaError
          (const "Error polling for message from Kafka")
    , Annotated.Handler
        $ logErrorNS "kafka"
        . annotatedExceptionMessageFrom @KafkaMessageDecodeError
          (const "Could not decode message value")
    ]

-- | Like 'annotatedExceptionMessage', but use the supplied function to
--   construct an initial 'Message' that it will augment.
annotatedExceptionMessageFrom
  :: Exception ex => (ex -> Message) -> AnnotatedException ex -> Message
annotatedExceptionMessageFrom f ann = case f ex of
  msg :# series -> msg :# series <> ["error" .= errorObject]
 where
  ex = Annotated.exception ann
  errorObject =
    object
      [ "message" .= displayException ex
      , "stack" .= (prettyCallStack <$> Annotated.annotatedExceptionCallStack ann)
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
        err -> Annotated.throw err
    )
    $ pure
    . Just
