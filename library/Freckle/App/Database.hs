{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | Database access for your @App@
module Freckle.App.Database
  ( HasSqlPool (..)
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

    -- * Re-exported constraints required to use 'runDB'
  , MonadTracer
  , HasStatsClient
  ) where

import Freckle.App.Prelude

import Blammo.Logging
import qualified Control.Immortal as Immortal
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Pool
import qualified Data.Text as T
import Database.Persist.Postgresql
  ( SqlBackend
  , SqlPersistT
  , createPostgresqlPoolModified
  , createSqlPool
  , openSimpleConn
  , runSqlPool
  , runSqlPoolWithExtensibleHooks
  )
import Database.Persist.SqlBackend.SqlPoolHooks
import Database.PostgreSQL.Simple
  ( Connection
  , Only (..)
  , connectPostgreSQL
  , execute
  )
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Freckle.App.Env (Timeout (..))
import qualified Freckle.App.Env as Env
import Freckle.App.OpenTelemetry
import Freckle.App.Stats (HasStatsClient)
import qualified Freckle.App.Stats as Stats
import OpenTelemetry.Instrumentation.Persistent
import System.Process.Typed (proc, readProcessStdout_)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (displayException)
import UnliftIO.IORef
import Yesod.Core.Types (HandlerData (..), RunHandlerEnv (..))

type SqlPool = Pool SqlBackend

class HasSqlPool app where
  getSqlPool :: app -> SqlPool

instance HasSqlPool SqlPool where
  getSqlPool = id

instance HasSqlPool site => HasSqlPool (HandlerData child site) where
  getSqlPool = getSqlPool . rheSite . handlerEnv

makePostgresPool :: (MonadUnliftIO m, MonadLoggerIO m) => m SqlPool
makePostgresPool = do
  conf <- liftIO $ do
    postgresPasswordSource <- Env.parse id $ Env.kept envPostgresPasswordSource
    Env.parse id $ Env.kept $ envParseDatabaseConf postgresPasswordSource
  makePostgresPoolWith conf

-- | Run a Database action with connection stats and tracing
--
-- This uses OpenTelemetry and 'MonadTracer'. For callstacks in traces to be
-- useful, ensure you have 'HasCallStack' on functions that call this (and
-- functions that call those, for as far as you require to get to a useful
-- source location).
runDB
  :: ( MonadUnliftIO m
     , MonadTracer m
     , MonadReader app m
     , HasSqlPool app
     , HasStatsClient app
     , HasCallStack
     )
  => SqlPersistT m a
  -> m a
runDB action = do
  pool <- asks getSqlPool
  Stats.withGauge Stats.dbConnections $
    inSpan "runDB" defaultSpanArguments $
      runSqlPoolWithExtensibleHooks action pool Nothing $
        setAlterBackend defaultSqlPoolHooks $
          wrapSqlBackend mempty

runDBSimple
  :: (HasSqlPool app, MonadUnliftIO m, MonadReader app m)
  => SqlPersistT m a
  -> m a
runDBSimple action = do
  pool <- asks getSqlPool
  runSqlPool action pool

data PostgresConnectionConf = PostgresConnectionConf
  { pccHost :: String
  , pccPort :: Int
  , pccUser :: String
  , pccPassword :: PostgresPassword
  , pccDatabase :: String
  , pccPoolSize :: Int
  , pccStatementTimeout :: PostgresStatementTimeout
  , pccSchema :: Maybe String
  }
  deriving stock (Show, Eq)

data PostgresPasswordSource
  = PostgresPasswordSourceIamAuth
  | PostgresPasswordSourceEnv
  deriving stock (Show, Eq)

data PostgresPassword
  = PostgresPasswordIamAuth
  | PostgresPasswordStatic String
  deriving stock (Show, Eq)

type PostgresStatementTimeout = Timeout

postgresStatementTimeoutMilliseconds :: PostgresStatementTimeout -> Int
postgresStatementTimeoutMilliseconds = \case
  TimeoutSeconds s -> s * 1000
  TimeoutMilliseconds ms -> ms

envPostgresPasswordSource :: Env.Parser Env.Error PostgresPasswordSource
envPostgresPasswordSource =
  Env.flag
    (Env.Off PostgresPasswordSourceEnv)
    (Env.On PostgresPasswordSourceIamAuth)
    "USE_RDS_IAM_AUTH"
    mempty

envParseDatabaseConf
  :: PostgresPasswordSource -> Env.Parser Env.Error PostgresConnectionConf
envParseDatabaseConf source = do
  user <- Env.var Env.nonempty "PGUSER" mempty
  password <- case source of
    PostgresPasswordSourceIamAuth -> pure PostgresPasswordIamAuth
    PostgresPasswordSourceEnv ->
      PostgresPasswordStatic <$> Env.var Env.nonempty "PGPASSWORD" mempty
  host <- Env.var Env.nonempty "PGHOST" mempty
  database <- Env.var Env.nonempty "PGDATABASE" mempty
  port <- Env.var Env.auto "PGPORT" mempty
  poolSize <- Env.var Env.auto "PGPOOLSIZE" $ Env.def 10
  schema <- optional $ Env.var Env.nonempty "PGSCHEMA" mempty
  statementTimeout <-
    Env.var Env.timeout "PGSTATEMENTTIMEOUT" $ Env.def (TimeoutSeconds 120)
  pure
    PostgresConnectionConf
      { pccHost = host
      , pccPort = port
      , pccUser = user
      , pccPassword = password
      , pccDatabase = database
      , pccPoolSize = poolSize
      , pccStatementTimeout = statementTimeout
      , pccSchema = schema
      }

data AuroraIamToken = AuroraIamToken
  { aitToken :: Text
  , aitCreatedAt :: UTCTime
  , aitPostgresConnectionConf :: PostgresConnectionConf
  }
  deriving stock (Show, Eq)

createAuroraIamToken :: MonadIO m => PostgresConnectionConf -> m AuroraIamToken
createAuroraIamToken aitPostgresConnectionConf@PostgresConnectionConf {..} = do
  aitToken <-
    T.strip . decodeUtf8 . BSL.toStrict
      <$> readProcessStdout_
        ( proc
            "aws"
            [ "rds"
            , "generate-db-auth-token"
            , "--hostname"
            , pccHost
            , "--port"
            , show pccPort
            , "--username"
            , pccUser
            ]
        )
  aitCreatedAt <- liftIO getCurrentTime
  pure AuroraIamToken {..}

-- | Spawns a thread that refreshes the IAM auth token every minute
--
-- The IAM auth token lasts 15 minutes, but we refresh it every minute just to
-- be super safe.
spawnIamTokenRefreshThread
  :: (MonadUnliftIO m, MonadLogger m)
  => PostgresConnectionConf
  -> m (IORef AuroraIamToken)
spawnIamTokenRefreshThread conf = do
  logInfo "Spawning thread to refresh IAM auth token"
  tokenIORef <- newIORef =<< createAuroraIamToken conf
  void $ Immortal.create $ \_ -> Immortal.onFinish onFinishCallback $ do
    logDebug "Refreshing IAM auth token"
    refreshIamToken conf tokenIORef
    threadDelay oneMinuteInMicroseconds
  pure tokenIORef
 where
  oneMinuteInMicroseconds = 60 * 1000000

  onFinishCallback = \case
    Left ex ->
      logError $
        "Error refreshing IAM auth token"
          :# ["exception" .= displayException ex]
    Right () -> pure ()

refreshIamToken
  :: MonadIO m => PostgresConnectionConf -> IORef AuroraIamToken -> m ()
refreshIamToken conf tokenIORef = do
  token' <- createAuroraIamToken conf
  writeIORef tokenIORef token'

setStartupOptions :: MonadIO m => PostgresConnectionConf -> Connection -> m ()
setStartupOptions PostgresConnectionConf {..} conn = do
  let timeoutMillis = postgresStatementTimeoutMilliseconds pccStatementTimeout
  liftIO $ do
    void $
      execute
        conn
        [sql| SET statement_timeout = ? |]
        (Only timeoutMillis)
    for_ pccSchema $ \schema -> execute conn [sql| SET search_path TO ? |] (Only schema)

makePostgresPoolWith
  :: (MonadUnliftIO m, MonadLoggerIO m) => PostgresConnectionConf -> m SqlPool
makePostgresPoolWith conf@PostgresConnectionConf {..} = case pccPassword of
  PostgresPasswordIamAuth -> makePostgresPoolWithIamAuth conf
  PostgresPasswordStatic password ->
    createPostgresqlPoolModified
      (setStartupOptions conf)
      (postgresConnectionString conf password)
      pccPoolSize

-- | Creates a PostgreSQL pool using IAM auth for the password
makePostgresPoolWithIamAuth
  :: (MonadUnliftIO m, MonadLoggerIO m) => PostgresConnectionConf -> m SqlPool
makePostgresPoolWithIamAuth conf@PostgresConnectionConf {..} = do
  tokenIORef <- spawnIamTokenRefreshThread conf
  createSqlPool (mkConn tokenIORef) pccPoolSize
 where
  mkConn tokenIORef logFunc = do
    token <- readIORef tokenIORef
    let connStr = postgresConnectionString conf (unpack $ aitToken token)
    conn <- connectPostgreSQL connStr
    setStartupOptions conf conn
    openSimpleConn logFunc conn

postgresConnectionString :: PostgresConnectionConf -> String -> ByteString
postgresConnectionString PostgresConnectionConf {..} password =
  BS8.pack $
    unwords
      [ "host=" <> pccHost
      , "port=" <> show pccPort
      , "user=" <> pccUser
      , "password=" <> password
      , "dbname=" <> pccDatabase
      ]
