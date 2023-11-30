module Freckle.App.Async
  ( async
  , immortalCreate
  , immortalCreateLogged
  )
where

import Freckle.App.Prelude

import Blammo.Logging
import qualified Control.Immortal as Immortal
import Control.Monad (forever)
import qualified Data.Aeson.Compat as KeyMap
import UnliftIO.Async (Async)
import qualified UnliftIO.Async as UnliftIO
import UnliftIO.Concurrent (threadDelay)

-- | 'UnliftIO.Async.async' but passing the thread context along
async :: (MonadMask m, MonadUnliftIO m) => m a -> m (Async a)
async f = do
  tc <- liftIO $ KeyMap.toList <$> myThreadContext
  UnliftIO.async $ withThreadContext tc f

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
  tc <- liftIO $ KeyMap.toList <$> myThreadContext

  let
    act' = withThreadContext tc act
    onUnexpected' = withThreadContext tc . onUnexpected

  void $ Immortal.create $ \thread -> do
    Immortal.onUnexpectedFinish thread onUnexpected' act'

  forever $ threadDelay maxBound

-- | 'immortalCreate' with logging of unexpected finishes
immortalCreateLogged
  :: (MonadMask m, MonadUnliftIO m, MonadLogger m) => m () -> m a
immortalCreateLogged = immortalCreate $ either logEx pure
 where
  logEx ex = logError $ "Unexpected Finish" :# ["exception" .= displayException ex]
