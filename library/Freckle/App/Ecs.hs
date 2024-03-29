module Freckle.App.Ecs
  ( EcsMetadata (..)
  , EcsMetadataError (..)
  , EcsContainerMetadata (..)
  , EcsContainerTaskMetadata (..)
  , getEcsMetadata
  ) where

import Freckle.App.Prelude

import Control.Monad.Except (MonadError (..))
import Data.Aeson
import Data.List.Extra (dropPrefix)
import Freckle.App.Http
import System.Environment (lookupEnv)

data EcsMetadata = EcsMetadata
  { emContainerMetadata :: EcsContainerMetadata
  , emContainerTaskMetadata :: EcsContainerTaskMetadata
  }

data EcsMetadataError
  = EcsMetadataErrorNotEnabled
  | EcsMetadataErrorInvalidURI String
  | EcsMetadataErrorUnexpectedStatus Request Status
  | EcsMetadataErrorInvalidJSON Request HttpDecodeError
  deriving stock (Show)

-- | Parsing for the @/@ response
--
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-metadata-endpoint-v4.html#task-metadata-endpoint-v4-examples>
data EcsContainerMetadata = EcsContainerMetadata
  { ecmDockerId :: Text
  , ecmDockerName :: Text
  , ecmImage :: Text
  , ecmImageID :: Text
  }
  deriving stock (Generic)

instance FromJSON EcsContainerMetadata where
  parseJSON = genericParseJSON $ aesonDropPrefix "ecm"

-- | Parsing of the @/task@ response
--
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-metadata-endpoint-v4.html#task-metadata-endpoint-v4-response>
data EcsContainerTaskMetadata = EcsContainerTaskMetadata
  { ectmCluster :: Text
  , ectmTaskARN :: Text
  , ectmFamily :: Text
  , ectmRevision :: Text
  }
  deriving stock (Generic)

instance FromJSON EcsContainerTaskMetadata where
  parseJSON = genericParseJSON $ aesonDropPrefix "ectm"

aesonDropPrefix :: String -> Options
aesonDropPrefix x = defaultOptions {fieldLabelModifier = dropPrefix x}

getEcsMetadata :: (MonadIO m, MonadError EcsMetadataError m) => m EcsMetadata
getEcsMetadata = do
  mURI <-
    liftIO $
      (<|>)
        <$> lookupEnv "ECS_CONTAINER_METADATA_URI_V4"
        <*> lookupEnv
          "ECS_CONTAINER_METADATA_URI"

  uri <- maybe (throwError EcsMetadataErrorNotEnabled) pure mURI

  EcsMetadata
    <$> makeContainerMetadataRequest uri
    <*> makeContainerMetadataRequest (uri <> "/task")

makeContainerMetadataRequest
  :: (MonadIO m, MonadError EcsMetadataError m, FromJSON a) => String -> m a
makeContainerMetadataRequest uri = do
  req <-
    mapEither (EcsMetadataErrorInvalidURI . displayException) $
      parseRequest uri
  resp <- liftIO $ httpJson req

  let status = getResponseStatus resp

  unless (statusIsSuccessful status) $
    throwError $
      EcsMetadataErrorUnexpectedStatus req status

  mapEither (EcsMetadataErrorInvalidJSON req) $ getResponseBody resp

mapEither :: MonadError e m => (x -> e) -> Either x a -> m a
mapEither f = either (throwError . f) pure
