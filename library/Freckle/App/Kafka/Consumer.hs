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
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Env
import Freckle.App.Async
import Freckle.App.Env
import Freckle.App.Exception
  ( AnnotatedException (..)
  , annotatedExceptionMessageFrom
  )
import Freckle.App.Kafka.Consumer.Offsets
import Freckle.App.Kafka.Producer (envKafkaBrokerAddresses)
import Freckle.App.OpenTelemetry
import Kafka.Consumer hiding
  ( Timeout
  , closeConsumer
  , newConsumer
  , runConsumer
  , subscription
  )
import qualified Kafka.Consumer as Kafka
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
      kafkaExtraProps

class HasKafkaConsumer env where
  kafkaConsumerL :: Lens' env KafkaConsumer

consumerProps :: KafkaConsumerConfig -> ConsumerProperties
consumerProps KafkaConsumerConfig {..} =
  brokersList brokers
    <> groupId kafkaConsumerConfigGroupId
    <> noAutoCommit -- we handle commits ourselves
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
runConsumer pollTimeout onMessage = do
  ref <- newLastMessageByPartition
  consumer <- view kafkaConsumerL

  let
    onFinish finishResult = do
      commitResult <- commitOffsetsSync ref consumer
      either (logEx "Unexpected finish") pure finishResult
      maybe (pure ()) (logEx "Unable to commit offsets") commitResult

    logEx msg =
      logError
        . annotatedExceptionMessageFrom (const msg)
        . AnnotatedException []

  withTraceIdContext $ immortalCreate onFinish $ do
    flip catches handlers $ inSpan "kafka.consumer" consumerSpanArguments $ do
      mRecord <- fromKafkaError =<< pollMessage consumer kTimeout

      for_ mRecord $ \r -> do
        for_ (crValue r) $ \bs -> do
          a <-
            inSpan "kafka.consumer.message.decode" defaultSpanArguments $
              either (throwM . KafkaMessageDecodeError bs) pure $
                eitherDecodeStrict bs
          inSpan "kafka.consumer.message.handle" defaultSpanArguments $ onMessage a

        -- Commit this message's offset async
        void $ commitOffsetMessage OffsetCommitAsync consumer r

        -- But also track it for the synchronous commit at shutdown
        updateLastMessageByPartition ref r
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
