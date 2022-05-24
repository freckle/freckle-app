module Freckle.App.Test.Logging
  ( runCapturedLoggingT
  ) where

import Freckle.App.Prelude

import Control.Concurrent.Chan
import Control.Monad.Logger
import UnliftIO.Async
import UnliftIO.Exception (finally)
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
  x <- async $ drainLog ref chan
  a <- runChanLoggingT chan $ f `finally` logInfoN doneMessage
  wait x
  msgs <- readIORef ref
  pure (a, msgs)

drainLog :: MonadIO m => IORef [Text] -> Chan (a, b, c, LogStr) -> m ()
drainLog ref chan = do
  (_, _, _, str) <- liftIO $ readChan chan
  let txt = decodeUtf8 $ fromLogStr str

  unless (txt == doneMessage) $ do
    modifyIORef' ref (<> [txt])
    drainLog ref chan

doneMessage :: Text
doneMessage = "%DONE%"
