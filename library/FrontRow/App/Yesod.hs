-- | Integration of "FrontRow.App" tooling with "Yesod"
module FrontRow.App.Yesod
  ( makeLogger
  , messageLoggerSource
  )
where

import Prelude

import Control.Monad (when)
import Control.Monad.Logger
import FrontRow.App.GlobalCache
import FrontRow.App.Logging
import System.IO.Unsafe (unsafePerformIO)
import System.Log.FastLogger
  ( LoggerSet
  , defaultBufSize
  , newFileLoggerSet
  , newStderrLoggerSet
  , newStdoutLoggerSet
  )
import Yesod.Core.Types (Logger, loggerPutStr)
import Yesod.Default.Config2 (makeYesodLogger)

loggerSetVar :: GlobalCache LoggerSet
loggerSetVar = unsafePerformIO newGlobalCache
{-# NOINLINE loggerSetVar #-}
{-# ANN loggerSetVar ("HLint: ignore Avoid restricted function" :: String) #-}

makeLogger :: HasLogging a => a -> IO Logger
makeLogger app = makeYesodLogger
  =<< globallyCache loggerSetVar (newLoggerSet defaultBufSize)
 where
  newLoggerSet = case getLogLocation app of
    LogStdout -> newStdoutLoggerSet
    LogStderr -> newStderrLoggerSet
    LogFile f -> flip newFileLoggerSet f

messageLoggerSource
  :: HasLogging a
  => a
  -> Logger
  -> Loc
  -> LogSource
  -> LogLevel
  -> LogStr
  -> IO ()
messageLoggerSource app logger loc src level str =
  when (level >= getLogLevel app)
    $ loggerPutStr logger
    $ toLogStr
    $ case getLogFormat app of
        FormatJSON -> formatJsonLogStr loc src level str
        FormatTerminal ->
          formatTerminal (getLogDefaultANSI app) loc src level str
