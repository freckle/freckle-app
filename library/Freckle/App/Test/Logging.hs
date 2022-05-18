module Freckle.App.Test.Logging
  ( runCapturedLoggingT
  ) where

import Freckle.App.Prelude

import Control.Concurrent.Chan
import Control.Monad (forever)
import Control.Monad.Logger
import UnliftIO.Async
import UnliftIO.IORef

-- | Run a 'LoggingT', capturing and returning any logged messages alongside
--
-- I do not know why 'runChanLoggingT' exists presumably for this purpose, but
-- requires so much more effort to ultimately accomplish.
--
runCapturedLoggingT :: MonadUnliftIO m => LoggingT m a -> m (a, [Text])
runCapturedLoggingT f = do
  chan <- liftIO newChan
  ref <- newIORef []
  x <- async $ forever $ do
    (_, _, _, str) <- liftIO $ readChan chan
    modifyIORef' ref (<> [decodeUtf8 $ fromLogStr str])

  a <- runChanLoggingT chan f

  cancel x
  msgs <- readIORef ref
  pure (a, msgs)
