module Freckle.App.Test.Logging
  ( MonadLogger
  , LoggingT
  , runCapturedLoggingT
  , logLineToText
  ) where

import Freckle.App.Prelude

import Control.Concurrent.Chan
import Control.Monad.Logger
import Data.DList (DList)
import qualified Data.DList as DList
import UnliftIO.Async
import UnliftIO.Exception (finally)

-- | Run a 'LoggingT', capturing and returning any logged messages alongside
--
-- This is 'runWriterLoggingT', but we're not able to supply a 'MonadUnliftIO'
-- instance when using that.
--
runCapturedLoggingT :: MonadUnliftIO m => LoggingT m a -> m (a, [LogLine])
runCapturedLoggingT f = do
  chan <- liftIO newChan
  x <- async $ captureLog DList.empty chan
  a <- runChanLoggingT chan $ f `finally` logInfoN doneMessage
  msgs <- wait x
  pure (a, DList.toList msgs)

captureLog :: MonadIO m => DList LogLine -> Chan LogLine -> m (DList LogLine)
captureLog acc chan = do
  ll <- liftIO $ readChan chan
  let txt = logLineToText ll
  if txt == doneMessage then pure acc else captureLog (DList.snoc acc ll) chan

doneMessage :: Text
doneMessage = "%DONE%"

logLineToText :: LogLine -> Text
logLineToText (_, _, _, str) = decodeUtf8 $ fromLogStr str
