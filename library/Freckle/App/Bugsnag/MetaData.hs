-- | Working with Bugsnag's 'event_metaData' field
--
-- $details
--
module Freckle.App.Bugsnag.MetaData
  ( MetaData(..)
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

import Blammo.Logging (Pair, myThreadContext)
import Control.Lens (Lens', lens, to, view, (<>~))
import Data.Aeson
import Data.Bugsnag (Event(..))
import Data.String (fromString)
import qualified Freckle.App.Aeson as Aeson
import Freckle.App.Bugsnag
import Freckle.App.Stats (HasStatsClient(..), tagsL)

newtype MetaData = MetaData
  { unMetaData :: Object
  }
  deriving stock (Eq, Show)

instance Semigroup MetaData where
  -- | /Right/-biased, recursive union
  --
  -- The chosen bias ensures that adding metadata in smaller scopes (later)
  -- overrides values from larger scopes.
  --
  MetaData x <> MetaData y = MetaData $ unionObjects y x
   where
    unionObjects :: Object -> Object -> Object
    unionObjects = Aeson.unionWith unionValues

    unionValues (Object a) (Object b) = Object $ unionObjects a b
    unionValues a _ = a

instance Monoid MetaData where
  mempty = MetaData mempty

-- | Construct 'MetaData' from 'Pair's
metaData
  :: Aeson.Key
  -- ^ The Tab within which the values will display
  -> [Pair]
  -- ^ The Key-Values themselves
  -> MetaData
metaData key = MetaData . Aeson.fromList . pure . (key .=) . object

metaDataL :: Lens' Event MetaData
metaDataL = lens get set
 where
  get event = maybe mempty MetaData $ event_metaData event
  set event md = event { event_metaData = Just $ unMetaData md }

-- | Collect 'MetaData' from a 'StatsClient' and 'myThreadContext'
--
-- Using this (and then 'mergeMetaData') will unify exception metadata with
-- metrics tags and the logging context.
--
collectMetaData
  :: (MonadIO m, MonadReader env m, HasStatsClient env) => m MetaData
collectMetaData =
  (<>) <$> collectMetaDataFromStatsClient <*> collectMetaDataFromThreadContext

collectMetaDataFromStatsClient
  :: (MonadReader env m, HasStatsClient env) => m MetaData
collectMetaDataFromStatsClient = view $ statsClientL . tagsL . to toMetaData
  where toMetaData = metaData "tags" . map (bimap (fromString . unpack) String)

collectMetaDataFromThreadContext :: MonadIO m => m MetaData
collectMetaDataFromThreadContext =
  liftIO $ metaData "context" . Aeson.toList <$> myThreadContext

-- | Merge 'MetaData' into the 'Event'
--
-- The given metadata will be combined with what already exists using '(<>)',
-- preserving the incoming values on collisions.
--
mergeMetaData :: MetaData -> BeforeNotify
mergeMetaData md = updateEvent $ metaDataL <>~ md

-- $details
--
-- From <https://bugsnagerrorreportingapi.docs.apiary.io/#reference/0/notify/send-error-reports>
--
-- @events[].metaData@
--
-- > An object containing any further data you wish to attach to this error
-- > event. This should contain one or more objects, with each object being
-- > displayed in its own tab on the event details on Bugsnag.
-- >
-- > {
-- >     // Custom user data to be displayed in the User tab along with standard
-- >     // user fields on the Bugsnag website.
-- >     "user": {
-- >        ...
-- >     },
-- >
-- >     // Custom app data to be displayed in the App tab along with standard
-- >     // app fields on the Bugsnag website.
-- >     "app": {
-- >        ...
-- >     },
-- >
-- >     // Custom device data to be displayed in the Device tab along with
-- >     //standard device fields on the Bugsnag website.
-- >     "device": {
-- >        ...
-- >     },
-- >
-- >     Custom request data to be displayed in the Request tab along with
-- >     standard request fields on the Bugsnag website.
-- >     "request": {
-- >        ...
-- >     },
-- >
-- >     // This will be displayed as an extra tab on the Bugsnag website.
-- >     "Some data": {
-- >
-- >         // A key value pair that will be displayed in the first tab.
-- >         "key": "value",
-- >
-- >         // Key value pairs can be contained in nested objects which helps
-- >         // to organise the information presented in the tab.
-- >         "setOfKeys": {
-- >             "key": "value",
-- >             "key2": "value"
-- >         }
-- >     },
-- >
-- >     // This would be the second extra tab on the Bugsnag website.
-- >     "Some more data": {
-- >         ...
-- >     }
-- > }
