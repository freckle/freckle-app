module Freckle.App.Ecs
  ( EcsMetadata(..)
  , EcsMetadataError(..)
  , EcsContainerMetadata(..)
  , EcsContainerTaskMetadata(..)
  , getEcsMetadata
  ) where

import Freckle.App.Prelude

import Control.Monad.Except (MonadError(..))
import Data.Aeson
import Data.List.Extra (dropPrefix)
import Freckle.App.Http
import System.Environment (lookupEnv)
import UnliftIO.Exception (Exception(..))

data EcsMetadata = EcsMetadata
  { emContainerMetadata :: EcsContainerMetadata
  , emContainerTaskMetadata :: EcsContainerTaskMetadata
  }

data EcsMetadataError
  = EcsMetadataErrorInvalidURI String
  | EcsMetadataErrorUnexpectedStatus Status Request
  | EcsMetadataErrorInvalidJSON HttpDecodeError
  deriving stock Show

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

getEcsMetadata
  :: (MonadIO m, MonadError EcsMetadataError m) => m (Maybe EcsMetadata)
getEcsMetadata = do
  mURI <- liftIO $ lookupEnv "ECS_CONTAINER_METADATA_URI"

  for mURI $ \uri ->
    EcsMetadata
      <$> makeContainerMetadataRequest (uri <> "/")
      <*> makeContainerMetadataRequest (uri <> "/task")

makeContainerMetadataRequest
  :: (MonadIO m, MonadError EcsMetadataError m, FromJSON a) => String -> m a
makeContainerMetadataRequest uri = do
  req <- mapEither (EcsMetadataErrorInvalidURI . displayException)
    $ parseRequest uri
  resp <- httpJson req

  let status = getResponseStatus resp

  unless (statusIsSuccessful status)
    $ throwError
    $ EcsMetadataErrorUnexpectedStatus status req

  mapEither EcsMetadataErrorInvalidJSON $ getResponseBody resp

mapEither :: MonadError e m => (x -> e) -> Either x a -> m a
mapEither f = either (throwError . f) pure
