module Freckle.App.Async
  ( async
  , foldConcurrently
  , immortalCreate
  , immortalCreateLogged
  , ThreadContext (..)
  , getThreadContext
  , withThreadContext
  ) where

import Freckle.App.Prelude

import Blammo.Logging (Message (..), MonadLogger, logError, (.=))
import Blammo.Logging.ThreadContext (MonadMask)
import Blammo.Logging.ThreadContext qualified as Blammo
import Control.Immortal qualified as Immortal
import Control.Monad (forever)
import Data.Aeson (Value)
import Data.Aeson.Compat (KeyMap)
import Data.Aeson.Compat qualified as KeyMap
import OpenTelemetry.Context qualified as OpenTelemetry
import OpenTelemetry.Context.ThreadLocal qualified as OpenTelemetry
import UnliftIO.Async (Async, conc, runConc)
import UnliftIO.Async qualified as UnliftIO
import UnliftIO.Concurrent (threadDelay)

-- | 'UnliftIO.Async.async' but passing the thread context along
async :: (MonadMask m, MonadUnliftIO m) => m a -> m (Async a)
async f = do
  context <- getThreadContext
  UnliftIO.async $ withThreadContext context f

-- | Run a list of actions concurrently
--
-- The forked threads will have the current thread context copied to them.
foldConcurrently
  :: (MonadUnliftIO m, MonadMask m, Monoid a, Foldable t) => t (m a) -> m a
foldConcurrently xs = do
  context <- getThreadContext
  runConc $ foldMap (conc . withThreadContext context) xs

-- | Wrapper around creating "Control.Immortal" processes
--
-- Features:
--
-- - Ensures the thread context is correctly passed to both your spawned action
--   and your error handler
-- - Blocks forever after spawning your thread.
immortalCreate
  :: (MonadMask m, MonadUnliftIO m)
  => (Either SomeException () -> m ())
  -- ^ How to handle unexpected finish
  -> m ()
  -- ^ The action to run persistently
  -> m a
immortalCreate onUnexpected act = do
  context <- getThreadContext

  let
    act' = withThreadContext context act
    onUnexpected' = withThreadContext context . onUnexpected

  void $ Immortal.create $ \thread -> do
    Immortal.onUnexpectedFinish thread onUnexpected' act'

  forever $ threadDelay maxBound

-- | 'immortalCreate' with logging of unexpected finishes
immortalCreateLogged
  :: (MonadMask m, MonadUnliftIO m, MonadLogger m) => m () -> m a
immortalCreateLogged = immortalCreate $ either logEx pure
 where
  logEx ex = logError $ "Unexpected Finish" :# ["exception" .= displayException ex]

data ThreadContext = ThreadContext
  { blammoContext :: KeyMap Value
  , openTelemetryContext :: Maybe OpenTelemetry.Context
  }

getThreadContext :: MonadIO m => m ThreadContext
getThreadContext =
  ThreadContext
    <$> liftIO Blammo.myThreadContext
    <*> OpenTelemetry.lookupContext

withThreadContext :: (MonadIO m, MonadMask m) => ThreadContext -> m a -> m a
withThreadContext ThreadContext {blammoContext, openTelemetryContext} continue =
  Blammo.withThreadContext (KeyMap.toList blammoContext) $ do
    traverse_ @Maybe OpenTelemetry.attachContext openTelemetryContext
    continue
