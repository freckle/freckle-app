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
  , produceKeyedOnWithProducerRecords
  ) where

import Prelude

import Blammo.Logging
  ( Message ((:#))
  , MonadLogger
  , logDebugNS
  , logErrorNS
  , (.=)
  )
import Control.Exception.Annotated.UnliftIO qualified as Annotated
import Control.Lens (Lens', lens, view)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (for_, toList)
import Data.HashMap.Strict qualified as HashMap
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Text qualified as T
import Data.Time (NominalDiffTime)
import Freckle.App.Env qualified as Env
import Kafka.Producer
import OpenTelemetry.Trace (SpanKind (..), defaultSpanArguments)
import OpenTelemetry.Trace qualified as Trace
import OpenTelemetry.Trace.Monad (MonadTracer, inSpan)
import UnliftIO (MonadUnliftIO, withRunInIO)
import Yesod.Core.Types (HandlerData (..), RunHandlerEnv (..))

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

envL :: Lens' (HandlerData child site) (RunHandlerEnv child site)
envL = lens handlerEnv $ \x y -> x {handlerEnv = y}

siteL :: Lens' (RunHandlerEnv child site) site
siteL = lens rheSite $ \x y -> x {rheSite = y}

createKafkaProducerPool
  :: NonEmpty BrokerAddress
  -> KafkaProducerPoolConfig
  -> IO (Pool KafkaProducer)
createKafkaProducerPool addresses config =
  Pool.newPool $
    Pool.setNumStripes (Just $ kafkaProducerPoolConfigStripes config) $
      Pool.defaultPoolConfig
        mkProducer
        closeProducer
        (realToFrac $ kafkaProducerPoolConfigIdleTimeout config)
        (kafkaProducerPoolConfigSize config)
 where
  mkProducer =
    either
      ( \err -> Annotated.throw $ userError ("Failed to open kafka producer: " <> show err)
      )
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
              run $
                logErrorNS "kafka" $
                  "Failed to send event"
                    :# ["error" .= T.pack (show e)]
 where
  mkProducerRecord value =
    ProducerRecord
      { prTopic
      , prPartition = UnassignedPartition
      , prKey = Just $ BSL.toStrict $ encode $ keyF value
      , prValue = Just $ BSL.toStrict $ encode value
      , prHeaders = mempty
      }

  traced =
    inSpan
      "kafka.produce"
      defaultSpanArguments
        { Trace.kind = Producer
        , Trace.attributes =
            HashMap.fromList
              [ ("service.name", "kafka")
              , ("topic", Trace.toAttribute $ unTopicName prTopic)
              ]
        }

-- | Produce/Emit a message to a Kafka topic, accepts a list of ProducerRecords (Kafka messages) instead of a list of event payloads
produceKeyedOnWithProducerRecords
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadTracer m
     , MonadReader env m
     , HasKafkaProducerPool env
     )
  => TopicName
  -> NonEmpty ProducerRecord
  -> m ()
produceKeyedOnWithProducerRecords prTopic events = traced $ do
  logDebugNS "kafka" $
    "Producing Kafka events" :# ["events" .= NE.map show events]
  view kafkaProducerPoolL >>= \case
    NullKafkaProducerPool -> pure ()
    KafkaProducerPool producerPool ->
      withRunInIO $ \run ->
        Pool.withResource producerPool $ \producer ->
          for_ @NonEmpty events $ \event -> do
            mError <- liftIO $ produceMessage producer event
            for_ @Maybe mError $ \e ->
              run $
                logErrorNS "kafka" $
                  "Failed to send event"
                    :# ["error" .= T.pack (show e)]
 where
  traced =
    inSpan
      "kafka.produce"
      defaultSpanArguments
        { Trace.kind = Producer
        , Trace.attributes =
            HashMap.fromList
              [ ("service.name", "kafka")
              , ("topic", Trace.toAttribute $ unTopicName prTopic)
              ]
        }
