module Freckle.App.Ecs
  ( EcsMetadata(..)
  , EcsContainerMetadata(..)
  , EcsContainerTaskMetadata(..)
  , getEcsMetadata
  ) where

import Freckle.App.Prelude

import Control.Error.Util (hush)
import Data.Aeson
import Data.List.Extra (dropPrefix)
import Freckle.App.Http
import System.Environment (lookupEnv)

data EcsMetadata = EcsMetadata
  { emContainerMetadata :: EcsContainerMetadata
  , emContainerTaskMetadata :: EcsContainerTaskMetadata
  }

data EcsContainerMetadata = EcsContainerMetadata
  { ecmDockerId :: Text
  , ecmDockerName :: Text
  , ecmImage :: Text
  , ecmImageID :: Text
  }
  deriving stock Generic

instance FromJSON EcsContainerMetadata where
  parseJSON = genericParseJSON $ aesonDropPrefix "ecm"

data EcsContainerTaskMetadata = EcsContainerTaskMetadata
  { ectmCluster :: Text
  , ectmTaskARN :: Text
  , ectmFamily :: Text
  , ectmRevision :: Text
  }
  deriving stock Generic

instance FromJSON EcsContainerTaskMetadata where
  parseJSON = genericParseJSON $ aesonDropPrefix "ectm"

aesonDropPrefix :: String -> Options
aesonDropPrefix x = defaultOptions { fieldLabelModifier = dropPrefix x }

getEcsMetadata :: MonadIO m => m (Maybe EcsMetadata)
getEcsMetadata =
  liftA2 EcsMetadata
    <$> makeContainerMetadataRequest "/"
    <*> makeContainerMetadataRequest "/task"

makeContainerMetadataRequest :: (MonadIO m, FromJSON a) => Text -> m (Maybe a)
makeContainerMetadataRequest path = do
  mURI <- liftIO $ lookupEnv "ECS_CONTAINER_METADATA_URI"
  meMetadata <- for mURI $ \uri -> do
    httpJson $ parseRequest_ $ uri <> unpack path
  pure $ hush . getResponseBody =<< meMetadata
