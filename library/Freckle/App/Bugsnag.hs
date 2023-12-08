module Freckle.App.Bugsnag
  ( Settings
  , HasBugsnagSettings (..)
  , notifyBugsnag
  , notifyBugsnagWith

    -- * 'AppVersion'
  , HasAppVersion (..)
  , setAppVersion

    -- * Loading settings
  , envParseBugsnagSettings

    -- * Re-exports
  , MonadReader
  , runReaderT
  , module Network.Bugsnag
  ) where

import Freckle.App.Prelude

import qualified Control.Exception as Base (Exception)
import Control.Lens (Lens', view)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Reader (runReaderT)
import Data.Bugsnag (App (..), Event (..), defaultApp)
import Data.Bugsnag.Settings (Settings (..), defaultSettings)
import Data.List (isInfixOf)
import Freckle.App.Async (async)
import Freckle.App.Bugsnag.CallStack (callStackBeforeNotify)
import Freckle.App.Bugsnag.ErrorClass (errorClassBeforeNotify)
import Freckle.App.Bugsnag.HttpException (httpExceptionBeforeNotify)
import Freckle.App.Bugsnag.Message (messageBeforeNotify)
import Freckle.App.Bugsnag.MetaData (metaDataAnnotationsBeforeNotify)
import Freckle.App.Bugsnag.SqlError (sqlErrorBeforeNotify)
import qualified Freckle.App.Env as Env
import Network.Bugsnag hiding (notifyBugsnag, notifyBugsnagWith)
import qualified Network.Bugsnag as Bugsnag
import Yesod.Core.Lens (envL, siteL)
import Yesod.Core.Types (HandlerData)

class HasAppVersion env where
  appVersionL :: Lens' env Text

instance HasAppVersion site => HasAppVersion (HandlerData child site) where
  appVersionL = envL . siteL . appVersionL

setAppVersion :: Text -> BeforeNotify
setAppVersion version = updateEvent $ \event ->
  event
    { event_app = Just $ updateApp $ fromMaybe defaultApp $ event_app event
    }
 where
  updateApp app = app {app_version = Just version}

class HasBugsnagSettings env where
  bugsnagSettingsL :: Lens' env Settings

instance HasBugsnagSettings Settings where
  bugsnagSettingsL = id

instance HasBugsnagSettings site => HasBugsnagSettings (HandlerData child site) where
  bugsnagSettingsL = envL . siteL . bugsnagSettingsL

-- | Notify Bugsnag of an exception
--
-- The notification is made asynchronously via a simple @'async'@. This is
-- best-effort and we don't care to keep track of the spawned threads.
notifyBugsnag
  :: ( MonadMask m
     , MonadUnliftIO m
     , MonadReader env m
     , HasBugsnagSettings env
     , Base.Exception e
     )
  => e
  -> m ()
notifyBugsnag = notifyBugsnagWith mempty

-- | 'notifyBugsnag' with a 'BeforeNotify'
notifyBugsnagWith
  :: ( MonadMask m
     , MonadUnliftIO m
     , MonadReader env m
     , HasBugsnagSettings env
     , Base.Exception e
     )
  => BeforeNotify
  -> e
  -> m ()
notifyBugsnagWith f ex = do
  settings <- view bugsnagSettingsL
  void $ async $ liftIO $ Bugsnag.notifyBugsnagWith f settings ex

-- | Set StackFrame's InProject to @'False'@ for Error Helper modules
--
-- We want exceptions grouped by the the first stack-frame that is /not/ them.
-- Marking them as not in-project does this, with little downside.
maskErrorHelpers :: BeforeNotify
maskErrorHelpers = setStackFramesInProjectByFile (`isInfixOf` "Exceptions")

envParseBugsnagSettings :: Env.Parser Env.Error Settings
envParseBugsnagSettings =
  build
    <$> Env.var Env.nonempty "BUGSNAG_API_KEY" mempty
    <*> Env.var Env.nonempty "BUGSNAG_RELEASE_STAGE" (Env.def "development")
 where
  build key stage =
    (defaultSettings key)
      { settings_releaseStage = stage
      , settings_beforeNotify = globalBeforeNotify
      }

globalBeforeNotify :: BeforeNotify
globalBeforeNotify =
  callStackBeforeNotify
    <> metaDataAnnotationsBeforeNotify
    <> sqlErrorBeforeNotify
    <> httpExceptionBeforeNotify
    <> maskErrorHelpers
    <> errorClassBeforeNotify
    <> messageBeforeNotify
