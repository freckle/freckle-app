module Freckle.App.Async
  ( -- * The 'ThreadContext' these functions preserve
    ThreadContext (..)
  , getThreadContext
  , withThreadContext

    -- * "UnliftIO.Async" functions that preserve context
  , async
  , pooledMapConcurrentlyN
  , pooledMapConcurrently
  , pooledMapConcurrentlyN_
  , pooledMapConcurrently_
  , pooledForConcurrentlyN
  , pooledForConcurrently
  , pooledForConcurrentlyN_
  , pooledForConcurrently_
  , mapConcurrently
  , forConcurrently
  , mapConcurrently_
  , forConcurrently_

    -- ** Related extensions
  , foldConcurrently

    -- * "Control.Immortal" functions that preserve context
  , immortalCreate

    -- ** Related extensions
  , immortalCreateLogged
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

-- | 'UnliftIO.Async.pooledMapConcurrentlyN' but passing the thread context along
pooledMapConcurrentlyN
  :: (MonadUnliftIO m, MonadMask m, Traversable t)
  => Int -> (a -> m b) -> t a -> m (t b)
pooledMapConcurrentlyN n f xs = do
  context <- getThreadContext
  UnliftIO.pooledMapConcurrentlyN n (withThreadContext context . f) xs

-- | 'UnliftIO.Async.pooledMapConcurrently' but passing the thread context along
pooledMapConcurrently
  :: (MonadUnliftIO m, MonadMask m, Traversable t) => (a -> m b) -> t a -> m (t b)
pooledMapConcurrently f xs = do
  context <- getThreadContext
  UnliftIO.pooledMapConcurrently (withThreadContext context . f) xs

-- | 'UnliftIO.Async.pooledMapConcurrentlyN_' but passing the thread context along
pooledMapConcurrentlyN_
  :: (MonadUnliftIO m, MonadMask m, Traversable t)
  => Int -> (a -> m b) -> t a -> m ()
pooledMapConcurrentlyN_ n f xs = do
  context <- getThreadContext
  UnliftIO.pooledMapConcurrentlyN_ n (withThreadContext context . f) xs

-- | 'UnliftIO.Async.pooledMapConcurrently_' but passing the thread context along
pooledMapConcurrently_
  :: (MonadUnliftIO m, MonadMask m, Traversable t) => (a -> m b) -> t a -> m ()
pooledMapConcurrently_ f xs = do
  context <- getThreadContext
  UnliftIO.pooledMapConcurrently_ (withThreadContext context . f) xs

-- | 'UnliftIO.Async.pooledForConcurrentlyN' but passing the thread context along
pooledForConcurrentlyN
  :: (MonadUnliftIO m, MonadMask m, Traversable t)
  => Int -> t a -> (a -> m b) -> m (t b)
pooledForConcurrentlyN n = flip $ pooledMapConcurrentlyN n

-- | 'UnliftIO.Async.pooledForConcurrently' but passing the thread context along
pooledForConcurrently
  :: (MonadUnliftIO m, MonadMask m, Traversable t) => t a -> (a -> m b) -> m (t b)
pooledForConcurrently = flip pooledMapConcurrently

-- | 'UnliftIO.Async.pooledForConcurrentlyN_' but passing the thread context along
pooledForConcurrentlyN_
  :: (MonadUnliftIO m, MonadMask m, Traversable t)
  => Int -> t a -> (a -> m b) -> m ()
pooledForConcurrentlyN_ n = flip $ pooledMapConcurrentlyN_ n

-- | 'UnliftIO.Async.pooledForConcurrently_' but passing the thread context along
pooledForConcurrently_
  :: (MonadUnliftIO m, MonadMask m, Traversable t) => t a -> (a -> m b) -> m ()
pooledForConcurrently_ = flip pooledMapConcurrently_

-- | 'UnliftIO.Async.mapConcurrently' but passing the thread context along
mapConcurrently
  :: (MonadUnliftIO m, MonadMask m, Traversable t) => (a -> m b) -> t a -> m (t b)
mapConcurrently f xs = do
  context <- getThreadContext
  UnliftIO.mapConcurrently (withThreadContext context . f) xs

-- | 'UnliftIO.Async.forConcurrently' but passing the thread context along
forConcurrently
  :: (MonadUnliftIO m, MonadMask m, Traversable t) => t a -> (a -> m b) -> m (t b)
forConcurrently = flip mapConcurrently

-- | 'UnliftIO.Async.mapConcurrently_' but passing the thread context along
mapConcurrently_
  :: (MonadUnliftIO m, MonadMask m, Traversable t) => (a -> m b) -> t a -> m ()
mapConcurrently_ f xs = do
  context <- getThreadContext
  UnliftIO.mapConcurrently_ (withThreadContext context . f) xs

-- | 'UnliftIO.Async.forConcurrently_' but passing the thread context along
forConcurrently_
  :: (MonadUnliftIO m, MonadMask m, Traversable t) => t a -> (a -> m b) -> m ()
forConcurrently_ = flip mapConcurrently_

-- | Run a list of actions concurrently, combining the results monoidally
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
