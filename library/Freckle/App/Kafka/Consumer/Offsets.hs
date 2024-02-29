{-# LANGUAGE TupleSections #-}

module Freckle.App.Kafka.Consumer.Offsets
  ( LastMessageByPartition
  , newLastMessageByPartition
  , updateLastMessageByPartition
  , commitOffsetsSync
  ) where

import Freckle.App.Prelude

import qualified Data.Map.Strict as Map
import Kafka.Consumer
import UnliftIO.IORef

newtype LastMessageByPartition = LastMessageByPartition
  { _unLastMessageByPartition :: IORef (Map PartitionId (ConsumerRecord () ()))
  }

newLastMessageByPartition :: MonadIO m => m LastMessageByPartition
newLastMessageByPartition = LastMessageByPartition <$> newIORef Map.empty

updateLastMessageByPartition
  :: MonadIO m
  => LastMessageByPartition
  -> ConsumerRecord k v
  -> m ()
updateLastMessageByPartition (LastMessageByPartition ref) record =
  atomicModifyIORef' ref $ (,()) . Map.insert (crPartition record') record'
 where
  record' :: ConsumerRecord () ()
  record' = bimap (const ()) (const ()) record

commitOffsetsSync
  :: MonadIO m
  => LastMessageByPartition
  -> KafkaConsumer
  -> m (Maybe KafkaError)
commitOffsetsSync (LastMessageByPartition ref) consumer = do
  m <- readIORef ref
  commitPartitionsOffsets OffsetCommit consumer
    . map topicPartitionFromMessageForCommit
    $ Map.elems m

-- | Creates a topic partition message for use with the offset commit message.
--
-- We increment the offset by 1 here because when we commit, the offset is the
-- position the consumer reads from to process the next message.
--
-- /Adapted from the hidden "Kafka.Consumer.Convert" module/
topicPartitionFromMessageForCommit :: ConsumerRecord k v -> TopicPartition
topicPartitionFromMessageForCommit m =
  TopicPartition
    { tpTopicName = crTopic m
    , tpPartition = crPartition m
    , tpOffset = PartitionOffset $ unOffset (crOffset m) + 1
    }
