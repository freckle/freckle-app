-- | Working with Bugsnag's 'event_metaData' field
module Freckle.App.Bugsnag.MetaData
  ( MetaData (..)
  , metaData
  , metaDataL

    -- * Collecting ambient data
  , collectMetaData
  , collectMetaDataFromStatsClient
  , collectMetaDataFromThreadContext

    -- * 'BeforeNotify'
  , mergeMetaData
  ) where

import Freckle.App.Prelude

import Blammo.Logging (myThreadContext)
import Control.Lens (Lens', lens, to, view, (<>~))
import Data.Aeson (Value (..))
import Data.Bugsnag (Event (..))
import Data.String (fromString)
import qualified Freckle.App.Aeson as Aeson
import Freckle.App.Stats (HasStatsClient (..), tagsL)
import Network.Bugsnag (BeforeNotify, updateEvent)
import Network.Bugsnag.MetaData

metaDataL :: Lens' Event MetaData
metaDataL = lens get set
 where
  get event = maybe mempty MetaData $ event_metaData event
  set event md = event {event_metaData = Just $ unMetaData md}

-- | Collect 'MetaData' from a 'StatsClient' and 'myThreadContext'
--
-- Using this (and then 'mergeMetaData') will unify exception metadata with
-- metrics tags and the logging context.
collectMetaData
  :: (MonadIO m, MonadReader env m, HasStatsClient env) => m MetaData
collectMetaData =
  (<>) <$> collectMetaDataFromStatsClient <*> collectMetaDataFromThreadContext

collectMetaDataFromStatsClient
  :: (MonadReader env m, HasStatsClient env) => m MetaData
collectMetaDataFromStatsClient = view $ statsClientL . tagsL . to toMetaData
 where
  toMetaData = metaData "tags" . map (bimap (fromString . unpack) String)

collectMetaDataFromThreadContext :: MonadIO m => m MetaData
collectMetaDataFromThreadContext =
  liftIO $ metaData "context" . Aeson.toList <$> myThreadContext

-- | Merge 'MetaData' into the 'Event'
--
-- The given metadata will be combined with what already exists using '(<>)',
-- preserving the incoming values on collisions.
mergeMetaData :: MetaData -> BeforeNotify
mergeMetaData md = updateEvent $ metaDataL <>~ md
