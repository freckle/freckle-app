-- | Integration of "Freckle.App" tooling with "Yesod"
module Freckle.App.Yesod
  ( makeLogger
  , messageLoggerSource

  -- * Functions for use as 'yesodMiddleware'
  , respondQueryCanceled
  , respondQueryCanceledHeaders
  ) where

import Freckle.App.Prelude

import Control.Monad.Logger
import Database.PostgreSQL.Simple (SqlError(..))
import Freckle.App.Datadog (HasDogStatsClient, HasDogStatsTags)
import qualified Freckle.App.Datadog as Datadog
import Freckle.App.GlobalCache
import Freckle.App.Logging
import Network.HTTP.Types (ResponseHeaders, status503)
import qualified Network.Wai as W
import System.IO.Unsafe (unsafePerformIO)
import System.Log.FastLogger
  ( LoggerSet
  , defaultBufSize
  , newFileLoggerSet
  , newStderrLoggerSet
  , newStdoutLoggerSet
  )
import UnliftIO.Exception (handleJust)
import Yesod.Core.Handler (sendWaiResponse)
import Yesod.Core.Types (HandlerFor, Logger, loggerPutStr)
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

-- | Catch 'SqlError' when queries are canceled due to timeout and respond 503
--
-- Also logs and increments a metric.
--
respondQueryCanceled
  :: (HasDogStatsClient site, HasDogStatsTags site)
  => HandlerFor site res
  -> HandlerFor site res
respondQueryCanceled = respondQueryCanceledHeaders []

-- | 'respondQueryCanceledHeaders' but adding headers to the 503 response
respondQueryCanceledHeaders
  :: (HasDogStatsClient site, HasDogStatsTags site)
  => ResponseHeaders
  -> HandlerFor site res
  -> HandlerFor site res
respondQueryCanceledHeaders headers = handleJust queryCanceled $ \ex -> do
  logErrorN $ pack $ show ex
  Datadog.increment "query_canceled" []
  sendWaiResponse $ W.responseLBS status503 headers "Query canceled"

queryCanceled :: SqlError -> Maybe SqlError
queryCanceled ex = ex <$ guard (sqlState ex == "57014")
