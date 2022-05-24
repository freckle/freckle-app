module Freckle.App.Test.Logging
  ( runCapturedLoggingT
  ) where

import Freckle.App.Prelude

import Control.Concurrent.Chan
import Control.Monad.Logger
import UnliftIO.Async
import UnliftIO.Exception (finally)

-- | Run a 'LoggingT', capturing and returning any logged messages alongside
--
-- I do not know why 'runChanLoggingT' exists presumably for this purpose, but
-- requires so much more effort to ultimately accomplish.
--
runCapturedLoggingT :: MonadUnliftIO m => LoggingT m a -> m (a, [Text])
runCapturedLoggingT f = do
  chan <- liftIO newChan
  x <- async $ captureLog [] chan
  a <- runChanLoggingT chan $ f `finally` logInfoN doneMessage
  msgs <- wait x
  pure (a, reverse msgs)

captureLog :: MonadIO m => [Text] -> Chan (a, b, c, LogStr) -> m [Text]
captureLog acc chan = do
  (_, _, _, str) <- liftIO $ readChan chan
  let txt = decodeUtf8 $ fromLogStr str
  if txt == doneMessage then pure acc else captureLog (txt : acc) chan

doneMessage :: Text
doneMessage = "%DONE%"
