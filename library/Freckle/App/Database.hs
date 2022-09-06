{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StrictData #-}

-- | Database access for your @App@
module Freckle.App.Database
  ( HasSqlPool(..)
  , SqlPool
  , makePostgresPool
  , makePostgresPoolWith
  , runDB
  , PostgresConnectionConf(..)
  , PostgresPasswordSource(..)
  , PostgresPassword(..)
  , PostgresStatementTimeout(..)
  , postgresStatementTimeoutMilliseconds
  , envParseDatabaseConf
  , envPostgresPasswordSource
  ) where

import Blammo.Logging
import qualified Control.Immortal as Immortal
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isDigit)
import Data.Pool
import qualified Data.Text as T
import Database.Persist.Postgresql
  ( SqlBackend
  , SqlPersistT
  , createPostgresqlPoolModified
  , createSqlPool
  , openSimpleConn
  , runSqlConn
  , runSqlPool
  )
import Database.PostgreSQL.Simple
  (Connection, Only(..), connectPostgreSQL, execute)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified Freckle.App.Env as Env
import Freckle.App.Prelude
import qualified Freckle.App.Stats as Stats
import qualified Freckle.App.Stats.Gauge as Gauge
import Network.AWS.XRayClient.Persistent
import Network.AWS.XRayClient.WAI
import qualified Prelude as Unsafe (read)
import System.Process.Typed (proc, readProcessStdout_)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket_, displayException)
import UnliftIO.IORef
import Yesod.Core (MonadHandler, MonadUnliftIO(withRunInIO), waiRequest)

type SqlPool = Pool SqlBackend

class HasSqlPool app where
  getSqlPool :: app -> SqlPool

instance HasSqlPool SqlPool where
  getSqlPool = id

class HasActiveConnectionGauge app where
  getActiveConnectionGauge :: app -> Gauge.Gauge

makePostgresPool :: (MonadUnliftIO m, MonadLoggerIO m) => m SqlPool
makePostgresPool = do
  conf <- liftIO $ do
    postgresPasswordSource <- Env.parse id $ Env.kept envPostgresPasswordSource
    Env.parse id $ Env.kept $ envParseDatabaseConf postgresPasswordSource
  makePostgresPoolWith conf

runDB
  :: ( HasActiveConnectionGauge app
     , HasSqlPool app
     , Stats.HasStatsClient app
     , MonadHandler m
     , MonadUnliftIO m
     , MonadReader app m
     )
  => SqlPersistT m a
  -> m a
runDB action = do
  app <- ask
  pool <- asks getSqlPool
  mVaultData <- vaultDataFromRequest <$> waiRequest
  maybe
    runSqlPool
    (runSqlPoolXRay "runDB")
    mVaultData
    (bracket_ (increment app) (decrement app) action)
    pool
  where
    increment = withGauge Gauge.increment
    decrement = withGauge Gauge.decrement
    withGauge f = runReaderT $ f =<< asks getActiveConnectionGauge

-- | @'runSqlPool'@ but with XRay tracing
--
-- Arguments are ordered to promote composition:
--
-- 1. Name before vault data, since Vault data could be @'Maybe'@
-- 2. Action than pool, to match @'runSqlPool'@
--
-- Together, this leads to the nice composition:
--
-- @
-- mVaultData <- getVaultData
-- maybe 'runSqlPool' ('runSqlPoolXRay' "name") mVaultData action pool
-- @
--
-- N.B. This could make sense in an aws-xray-client-persistent package.
--
runSqlPoolXRay
  :: (backend ~ SqlBackend, MonadUnliftIO m)
  => Text
  -- ^ Subsegment name
  --
  -- The top-level subsegment will be named @\"<this> runSqlPool\"@ and the,
  -- with a lower-level subsegment named @\"<this> query\"@.
  --
  -> XRayVaultData -- ^ Vault data to trace with
  -> ReaderT backend m a
  -> Pool backend
  -> m a
runSqlPoolXRay name vaultData action pool =
  traceXRaySubsegment' vaultData (name <> " runSqlPool") id
    $ withRunInIO
    $ \run -> withResource pool $ \backend -> do
        let
          sendTrace = atomicallyAddVaultDataSubsegment vaultData
          stdGenIORef = xrayVaultDataStdGen vaultData
          subsegmentName = name <> " query"
        run . runSqlConn action =<< liftIO
          (xraySqlBackend sendTrace stdGenIORef subsegmentName backend)

data PostgresConnectionConf = PostgresConnectionConf
  { pccHost :: String
  , pccPort :: Int
  , pccUser :: String
  , pccPassword :: PostgresPassword
  , pccDatabase :: String
  , pccPoolSize :: Int
  , pccStatementTimeout :: PostgresStatementTimeout
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

data PostgresStatementTimeout
  = PostgresStatementTimeoutSeconds Int
  | PostgresStatementTimeoutMilliseconds Int
  deriving stock (Show, Eq)

postgresStatementTimeoutMilliseconds :: PostgresStatementTimeout -> Int
postgresStatementTimeoutMilliseconds = \case
  PostgresStatementTimeoutSeconds s -> s * 1000
  PostgresStatementTimeoutMilliseconds ms -> ms

-- | Read @PGSTATEMENTTIMEOUT@ as seconds or milliseconds
--
-- >>> readPostgresStatementTimeout "10"
-- Right (PostgresStatementTimeoutSeconds 10)
--
-- >>> readPostgresStatementTimeout "10s"
-- Right (PostgresStatementTimeoutSeconds 10)
--
-- >>> readPostgresStatementTimeout "10ms"
-- Right (PostgresStatementTimeoutMilliseconds 10)
--
-- >>> readPostgresStatementTimeout "20m"
-- Left "..."
--
-- >>> readPostgresStatementTimeout "2m0"
-- Left "..."
--
readPostgresStatementTimeout
  :: String -> Either String PostgresStatementTimeout
readPostgresStatementTimeout x = case span isDigit x of
  ("", _) -> Left "must be {digits}(s|ms)"
  (digits, "") -> Right $ PostgresStatementTimeoutSeconds $ Unsafe.read digits
  (digits, "s") -> Right $ PostgresStatementTimeoutSeconds $ Unsafe.read digits
  (digits, "ms") ->
    Right $ PostgresStatementTimeoutMilliseconds $ Unsafe.read digits
  _ -> Left "must be {digits}(s|ms)"

envPostgresPasswordSource :: Env.Parser Env.Error PostgresPasswordSource
envPostgresPasswordSource = Env.flag
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
  statementTimeout <-
    Env.var (Env.eitherReader readPostgresStatementTimeout) "PGSTATEMENTTIMEOUT"
      $ Env.def (PostgresStatementTimeoutSeconds 120)
  pure PostgresConnectionConf
    { pccHost = host
    , pccPort = port
    , pccUser = user
    , pccPassword = password
    , pccDatabase = database
    , pccPoolSize = poolSize
    , pccStatementTimeout = statementTimeout
    }

data AuroraIamToken = AuroraIamToken
  { aitToken :: Text
  , aitCreatedAt :: UTCTime
  , aitPostgresConnectionConf :: PostgresConnectionConf
  }
  deriving stock (Show, Eq)

createAuroraIamToken :: MonadIO m => PostgresConnectionConf -> m AuroraIamToken
createAuroraIamToken aitPostgresConnectionConf@PostgresConnectionConf {..} = do
  -- TODO: Consider recording how long creating an auth token takes
  -- somewhere, even if it is just in the logs, so we get an idea of how long
  -- it takes in prod.
  aitToken <- T.strip . decodeUtf8 . BSL.toStrict <$> readProcessStdout_
    (proc
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
  pure AuroraIamToken { .. }

-- | Spawns a thread that refreshes the IAM auth token every minute
--
-- The IAM auth token lasts 15 minutes, but we refresh it every minute just to
-- be super safe.
--
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
      logError
        $ "Error refreshing IAM auth token"
        :# ["exception" .= displayException ex]
    Right () -> pure ()

refreshIamToken
  :: MonadIO m => PostgresConnectionConf -> IORef AuroraIamToken -> m ()
refreshIamToken conf tokenIORef = do
  token' <- createAuroraIamToken conf
  writeIORef tokenIORef token'

setTimeout :: MonadIO m => PostgresConnectionConf -> Connection -> m ()
setTimeout PostgresConnectionConf {..} conn = do
  let timeoutMillis = postgresStatementTimeoutMilliseconds pccStatementTimeout
  void $ liftIO $ execute
    conn
    [sql| SET statement_timeout = ? |]
    (Only timeoutMillis)

makePostgresPoolWith
  :: (MonadUnliftIO m, MonadLoggerIO m) => PostgresConnectionConf -> m SqlPool
makePostgresPoolWith conf@PostgresConnectionConf {..} = case pccPassword of
  PostgresPasswordIamAuth -> makePostgresPoolWithIamAuth conf
  PostgresPasswordStatic password -> createPostgresqlPoolModified
    (setTimeout conf)
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
    setTimeout conf conn
    openSimpleConn logFunc conn

postgresConnectionString :: PostgresConnectionConf -> String -> ByteString
postgresConnectionString PostgresConnectionConf {..} password =
  BS8.pack $ unwords
    [ "host=" <> pccHost
    , "port=" <> show pccPort
    , "user=" <> pccUser
    , "password=" <> password
    , "dbname=" <> pccDatabase
    ]
