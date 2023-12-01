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

    -- * Exported for testing
  , sqlErrorGroupingHash

    -- * Re-exports
  , MonadReader
  , runReaderT
  , module Network.Bugsnag
  ) where

import Freckle.App.Prelude

import Control.Lens (Lens', view)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Reader (runReaderT)
import Data.Bugsnag
import Data.Bugsnag.Settings
import qualified Data.ByteString.Char8 as BS8
import Data.List (isInfixOf)
import Database.PostgreSQL.Simple (SqlError (..))
import Database.PostgreSQL.Simple.Errors
import Freckle.App.Async (async)
import qualified Freckle.App.Env as Env
import qualified Freckle.App.Exception.MonadUnliftIO as Exception
import Freckle.App.Exception.Types (AnnotatedException)
import qualified Freckle.App.Exception.Types as Annotated
import Freckle.App.Exception.Utilities (fromExceptionAnnotated)
import Network.Bugsnag hiding (notifyBugsnag, notifyBugsnagWith)
import qualified Network.Bugsnag as Bugsnag
import Network.HTTP.Client (HttpException (..), host, method)
import Yesod.Core.Lens
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
     , Exception.Exception e
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
     , Exception.Exception e
     )
  => BeforeNotify
  -> e
  -> m ()
notifyBugsnagWith f ex = do
  settings <- view bugsnagSettingsL
  void $ async $ liftIO $ Bugsnag.notifyBugsnagWith f settings ex

asSqlError :: SqlError -> BeforeNotify
asSqlError err@SqlError {..} = toSqlGrouping <> toSqlException
 where
  toSqlGrouping = maybe mempty setGroupingHash (sqlErrorGroupingHash err)
  toSqlException = updateExceptions $ \ex ->
    ex
      { exception_errorClass = decodeUtf8 $ "SqlError-" <> sqlState
      , exception_message =
          Just $
            decodeUtf8 $
              sqlErrorMsg
                <> ": "
                <> sqlErrorDetail
                <> " ("
                <> sqlErrorHint
                <> ")"
      }

sqlErrorGroupingHash :: SqlError -> Maybe Text
sqlErrorGroupingHash err = do
  violation <- constraintViolation err
  decodeUtf8 <$> case violation of
    ForeignKeyViolation table constraint -> pure $ table <> "." <> constraint
    UniqueViolation constraint -> pure constraint
    _ -> Nothing

asHttpException :: HttpException -> BeforeNotify
asHttpException (HttpExceptionRequest req content) =
  setGroupingHash (decodeUtf8 $ host req) <> update
 where
  update = updateExceptions $ \ex ->
    ex
      { exception_errorClass = "HttpExceptionRequest"
      , exception_message =
          Just
            . decodeUtf8
            $ method req
              <> " request to "
              <> host req
              <> " failed: "
              <> BS8.pack (show content)
      }
asHttpException (InvalidUrlException url msg) = updateExceptions $ \ex ->
  ex
    { exception_errorClass = "InvalidUrlException"
    , exception_message = Just $ pack $ url <> " is invalid: " <> msg
    }

-- | Set StackFrame's InProject to @'False'@ for Error Helper modules
--
-- We want exceptions grouped by the the first stack-frame that is /not/ them.
-- Marking them as not in-project does this, with little downside.
maskErrorHelpers :: BeforeNotify
maskErrorHelpers = setStackFramesInProjectByFile (`isInfixOf` "Exceptions")

-- brittany-disable-next-binding

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
  updateEventFromOriginalExceptionAnnotated (asSqlError . Annotated.exception)
    <> updateEventFromOriginalExceptionAnnotated
      (asHttpException . Annotated.exception)
    <> maskErrorHelpers

-- | Like 'updateEventFromOriginalException', but can handle either
--   @e@ or @'AnnotatedException' e@
updateEventFromOriginalExceptionAnnotated
  :: forall e
   . Exception.Exception e
  => (AnnotatedException e -> BeforeNotify)
  -> BeforeNotify
updateEventFromOriginalExceptionAnnotated f =
  beforeNotify $ \e event ->
    let bn = maybe mempty f $ fromExceptionAnnotated @e $ Exception.toException e
    in  runBeforeNotify bn e event
