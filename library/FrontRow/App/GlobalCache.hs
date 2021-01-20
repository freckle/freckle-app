-- | Facility for purely creating an 'IORef' in which to stash a value
--
-- Storing a truly global value can be useful for performance (e.g. to speed up
-- tests by caching a constructed App) or necessary to prevent contention (e.g.
-- by caching a single 'LoggerSet' which holds access to a log file).
--
-- In some cases, it's not possible to create an 'IORef' safely in an 'IO'
-- context to use for this purpose. Either because a library prevents it (e.g.
-- the test runner provides no such hook before triggering its parallel
-- execution) or because the current application architecture cannot allow it
-- without a high effort re-organization.
--
-- For these cases, we use this module.
--
-- == Usage
--
-- Given some function,
--
-- @
-- makeLogger :: HasLogging a => a -> IO Logger
-- makeLogger app = makeYesodLogger =<< newLoggerSet defaultBufSize
--  where
--   newLoggerSet = case getLogLocation app of
--     LogStdout -> newStdoutLoggerSet
--     LogStderr -> newStderrLoggerSet
--     LogFile f -> flip newFileLoggerSet f
-- @
--
-- Update it to cache the construction in a new top-level value,
--
-- @
-- loggerSetVar :: GlobalCache LoggerSet
-- loggerSetVar = unsafePerformIO newGlobalCache
-- {-# NOINLINE loggerSetVar #-}
--
-- makeLogger :: HasLogging a => a -> IO Logger
-- makeLogger app = makeYesodLogger
--   =<< globallyCache loggerSetVar (newLoggerSet defaultBufSize)
--  where
--   newLoggerSet = case getLogLocation app of
--     LogStdout -> newStdoutLoggerSet
--     LogStderr -> newStderrLoggerSet
--     LogFile f -> flip newFileLoggerSet f
-- @
--
module FrontRow.App.GlobalCache
  ( GlobalCache
  , newGlobalCache
  , globallyCache
  , withGlobalCacheCleanup
  )
where

import Prelude

import Control.Concurrent.MVar (mkWeakMVar, modifyMVar_, newMVar)
import Control.Monad (void)
import Data.IORef

newtype GlobalCache a = GlobalCache
  { _unGlobalCache :: IORef (Maybe a)
  }

newGlobalCache :: IO (GlobalCache a)
newGlobalCache = GlobalCache <$> newIORef Nothing

globallyCache :: GlobalCache a -> IO a -> IO a
globallyCache (GlobalCache var) construct = do
  mv <- readIORef var
  maybe (cache =<< construct) pure mv
  where cache v = atomicModifyIORef' var $ const (Just v, v)

-- | Garbage collect one of our 'IORef's after an action has run
--
-- Global 'IORef's are problematic for @ghci@. @ghci@ cannot garbage collect
-- them, so any state they hold will persist for an entire @ghci@ session. This
-- causes a varietyof issues.
--
-- To avoid garbage collection issues, we can leverage a "System.Mem.Weak" to
-- add a finalizer. When that 'MVar' gets garbage collected we can clear the
-- global 'IORef'. This maintains the status quo, with minimal plumbing.
--
withGlobalCacheCleanup :: GlobalCache a -> IO b -> IO ()
withGlobalCacheCleanup (GlobalCache var) action = do
  cleanup <- newMVar ()
  void $ mkWeakMVar cleanup $ writeIORef var Nothing
  void action
  modifyMVar_ cleanup (const $ pure ())
