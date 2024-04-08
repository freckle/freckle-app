{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | Database access for your @App@
module Freckle.App.Database
  ( -- * Running transactions
    MonadSqlTx (..)
  , runDB
  , runDBSimple

    -- * Running queries
  , SqlBackend
  , HasSqlBackend (..)
  , MonadSqlBackend (..)
  , liftSql

    -- * Telemetry
  , MonadTracer
  , HasStatsClient

    -- * Connection pools
  , HasSqlPool (..)
  , SqlPool
  , makePostgresPool
  , makePostgresPoolWith

    -- * Setup
  , PostgresConnectionConf (..)
  , PostgresPasswordSource (..)
  , PostgresPassword (..)
  , PostgresStatementTimeout
  , postgresStatementTimeoutMilliseconds
  , envParseDatabaseConf
  , envPostgresPasswordSource
  ) where

import Freckle.App.Prelude

import Blammo.Logging
import qualified Control.Immortal as Immortal
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
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
import Database.Persist.SqlBackend.Internal.SqlPoolHooks (SqlPoolHooks (..))
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
import Freckle.App.Exception.MonadUnliftIO
import Freckle.App.OpenTelemetry
import Freckle.App.Stats (HasStatsClient)
import qualified Freckle.App.Stats as Stats
import OpenTelemetry.Instrumentation.Persistent
import qualified OpenTelemetry.Trace as Trace
import System.Process.Typed (proc, readProcessStdout_)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.IORef
import Yesod.Core.Types (HandlerData (..), RunHandlerEnv (..))

-- | A monadic context in which a SQL backend is available
--   for running database queries
class MonadUnliftIO m => MonadSqlBackend m where
  getSqlBackendM :: m SqlBackend

instance (HasSqlBackend r, MonadUnliftIO m) => MonadSqlBackend (ReaderT r m) where
  getSqlBackendM = asks getSqlBackend

instance MonadSqlBackend m => MonadSqlBackend (ExceptT e m) where
  getSqlBackendM = lift getSqlBackendM

-- | Generalize from 'SqlPersistT' to 'MonadSqlBackend'
liftSql :: (MonadSqlBackend m, HasCallStack) => ReaderT SqlBackend m a -> m a
liftSql (ReaderT f) = checkpointCallStack $ getSqlBackendM >>= f

-- | The constraint @'MonadSqlTx' db m@ indicates that @m@ is a monadic
--   context that can run @db@ actions, usually as a SQL transaction.
--   Typically, this means that @db@ needs a connection and @m@ can
--   provide one, e.g. from a connection pool.
class (MonadSqlBackend db, MonadUnliftIO m) => MonadSqlTx db m | m -> db where
  -- | Runs the action in a SQL transaction
  runSqlTx :: HasCallStack => db a -> m a

class HasSqlBackend a where
  getSqlBackend :: a -> SqlBackend

instance HasSqlBackend SqlBackend where
  getSqlBackend = id

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
    postgresPasswordSource <- Env.parse id envPostgresPasswordSource
    Env.parse id $ envParseDatabaseConf postgresPasswordSource
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
  gauge <- Stats.lookupGauge Stats.dbConnections
  let
    hooks = setAlterBackend defaultSqlPoolHooks $ wrapSqlBackend dbAttributes
    -- Setting the SqlPoolHooks for metrics collection:
    -- You may be wondering if this code contains a "double-decrement" bug, because
    -- perhaps when an exception occurs, both runAfter and runOnException are
    -- executed. The documentation isn't terribly clear to me on this point, but
    -- notice:
    -- https://hackage.haskell.org/package/persistent-2.14.6.0/docs/Database-Persist-SqlBackend-Internal-SqlPoolHooks.html#t:SqlPoolHooks
    -- especially this part regarding runOnException:
    -- > This action is performed when an exception is received. The exception is
    -- > provided as a convenience - it is rethrown once this cleanup function is
    -- > complete.
    -- Given that the exception is rethrown, then then typical runAfter handler
    -- is very likely to not run. Looking at the code to validate:
    -- https://hackage.haskell.org/package/persistent-2.14.6.0/docs/src/Database.Persist.Sql.Run.html#runSqlPoolWithExtensibleHooks
    -- we can see that this is indeed how the code operates today -- the
    -- exception is rethrown, and there aren't any other spots where other code
    -- might catch the exception that we need to worry about, so in the case of
    -- an exception, runAfter would not be executed.
    -- So, this appears to be the intended interpretation.
    hooks' =
      hooks
        { runBefore = \conn mi -> do
            Stats.incGauge gauge
            runBefore hooks conn mi
        , runAfter = \conn mi -> do
            Stats.decGauge gauge
            runAfter hooks conn mi
        , runOnException = \conn mi e -> do
            Stats.decGauge gauge
            runOnException hooks conn mi e
        }
  Stats.withGauge Stats.dbEnqueuedAndProcessing $
    inSpan "runDB" (clientSpanArguments {Trace.attributes = dbAttributes}) $
      runSqlPoolWithExtensibleHooks action pool Nothing hooks'
 where
  dbAttributes = HashMap.fromList [("service.name", "database")]

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
