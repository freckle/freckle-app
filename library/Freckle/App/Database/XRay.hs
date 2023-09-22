{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeOperators #-}

-- | Legacy version of "Freckle.App.Database" that still uses XRay
module Freckle.App.Database.XRay
  ( MonadTracer (..)
  , HasStatsClient
  , HasSqlPool (..)
  , SqlPool
  , makePostgresPool
  , makePostgresPoolWith
  , runDB
  , runDBSimple
  , PostgresConnectionConf (..)
  , PostgresPasswordSource (..)
  , PostgresPassword (..)
  , PostgresStatementTimeout
  , postgresStatementTimeoutMilliseconds
  , envParseDatabaseConf
  , envPostgresPasswordSource
  ) where

import Freckle.App.Prelude

import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader
import Data.Pool
import Database.Persist.Postgresql
  ( SqlBackend
  , SqlPersistT
  , runSqlConn
  , runSqlPool
  )
import Freckle.App.Database hiding (MonadTracer, runDB)
import qualified Freckle.App.Stats as Stats
import Network.AWS.XRayClient.Persistent
import Network.AWS.XRayClient.WAI
  ( XRayVaultData
  , atomicallyAddVaultDataSubsegment
  , traceXRaySubsegment'
  , vaultDataFromRequest
  , xrayVaultDataStdGen
  )
import Yesod.Core (HandlerFor, waiRequest)

-- | Class for reading 'XRayVaultData'
--
-- This is named the same as the OpenTelemetry class we'll use once we move to
-- that tracing system
class MonadTracer m where
  getVaultData :: m (Maybe XRayVaultData)

instance MonadTracer (HandlerFor app) where
  getVaultData = vaultDataFromRequest <$> waiRequest

-- | Run a Database action with connection stats and tracing
runDB
  :: ( MonadUnliftIO m
     , MonadTracer m
     , MonadReader app m
     , HasSqlPool app
     , HasStatsClient app
     )
  => SqlPersistT m a
  -> m a
runDB action = do
  pool <- asks getSqlPool
  mVaultData <- getVaultData
  Stats.withGauge Stats.dbConnections $
    maybe runSqlPool (runSqlPoolXRay "runDB") mVaultData action pool

-- | @'runSqlPool'@ but with XRay tracing
runSqlPoolXRay
  :: (backend ~ SqlBackend, MonadUnliftIO m)
  => Text
  -- ^ Subsegment name
  --
  -- The top-level subsegment will be named @\"<this> runSqlPool\"@ and the,
  -- with a lower-level subsegment named @\"<this> query\"@.
  -> XRayVaultData
  -- ^ Vault data to trace with
  -> ReaderT backend m a
  -> Pool backend
  -> m a
runSqlPoolXRay name vaultData action pool =
  traceXRaySubsegment' vaultData (name <> " runSqlPool") id $
    withRunInIO $
      \run -> withResource pool $ \backend -> do
        let
          sendTrace = atomicallyAddVaultDataSubsegment vaultData
          stdGenIORef = xrayVaultDataStdGen vaultData
          subsegmentName = name <> " query"
        run . runSqlConn action
          =<< liftIO
            (xraySqlBackend sendTrace stdGenIORef subsegmentName backend)
