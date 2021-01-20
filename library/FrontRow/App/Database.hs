{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}

-- | Database access for your @App@
module FrontRow.App.Database
  ( HasDbPool(..)
  , HasAnalyticsDbPool(..)
  , makePostgresPool
  , makePostgresPoolWith
  , runDB
  , PostgresConnectionConf(..)
  , PostgresPasswordSource(..)
  , PostgresPassword(..)
  , envParseDatabaseConf
  , envPostgresPasswordSource
  )
where

import Prelude

import Control.Concurrent
import qualified Control.Immortal as Immortal
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Data.Pool
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist.Postgresql
import Database.PostgreSQL.Simple (connectPostgreSQL)
import qualified FrontRow.App.Env as Env
import System.Process (readProcess)

class HasDbPool app where
  getDbPool :: app -> Pool SqlBackend

instance HasDbPool (Pool SqlBackend) where
  getDbPool = id

class HasAnalyticsDbPool app where
  getAnalyticsDbPool :: app -> Pool SqlBackend

instance HasAnalyticsDbPool (Pool SqlBackend) where
  getAnalyticsDbPool = id

makePostgresPool :: IO (Pool SqlBackend)
makePostgresPool = do
  postgresPasswordSource <- Env.parse envPostgresPasswordSource
  conf <- Env.parse (envParseDatabaseConf postgresPasswordSource)
  makePostgresPoolWith conf

runDB
  :: (HasDbPool app, MonadUnliftIO m, MonadReader app m)
  => SqlPersistT m a
  -> m a
runDB action = do
  pool <- asks getDbPool
  runSqlPool action pool

data PostgresConnectionConf
  = PostgresConnectionConf
  { pccHost :: String
  , pccPort :: Int
  , pccUser :: String
  , pccPassword :: PostgresPassword
  , pccDatabase :: String
  , pccPoolSize :: Int
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

envPostgresPasswordSource :: Env.Parser PostgresPasswordSource
envPostgresPasswordSource = do
  useIam <- Env.switch "USE_RDS_IAM_AUTH" $ Env.def False
  pure $ if useIam
    then PostgresPasswordSourceIamAuth
    else PostgresPasswordSourceEnv

envParseDatabaseConf
  :: PostgresPasswordSource -> Env.Parser PostgresConnectionConf
envParseDatabaseConf source = do
  user <- Env.var Env.str "PGUSER" Env.nonEmpty
  password <- case source of
    PostgresPasswordSourceIamAuth -> pure PostgresPasswordIamAuth
    PostgresPasswordSourceEnv ->
      PostgresPasswordStatic <$> Env.var Env.str "PGPASSWORD" Env.nonEmpty
  host <- Env.var Env.str "PGHOST" Env.nonEmpty
  database <- Env.var Env.str "PGDATABASE" Env.nonEmpty
  port <- Env.var Env.auto "PGPORT" Env.nonEmpty
  poolSize <- Env.var Env.auto "PGPOOLSIZE" $ Env.def 1
  pure PostgresConnectionConf
    { pccHost = host
    , pccPort = port
    , pccUser = user
    , pccPassword = password
    , pccDatabase = database
    , pccPoolSize = poolSize
    }

data AuroraIamToken
  = AuroraIamToken
  { aitToken :: String
  , aitCreatedAt :: UTCTime
  , aitPostgresConnectionConf :: PostgresConnectionConf
  } deriving stock (Show, Eq)

createAuroraIamToken :: PostgresConnectionConf -> IO AuroraIamToken
createAuroraIamToken aitPostgresConnectionConf@PostgresConnectionConf {..} = do
  -- TODO: Consider recording how long creating an auth token takes
  -- somewhere, even if it is just in the logs, so we get an idea of how long
  -- it takes in prod.
  aitToken <- T.unpack . T.strip . T.pack <$> readProcess
    "aws"
    [ "rds"
    , "generate-db-auth-token"
    , "--hostname"
    , pccHost
    , "--port"
    , show pccPort
    , "--region"
    , "us-east-1"
    , "--username"
    , pccUser
    ]
    ""
  aitCreatedAt <- getCurrentTime
  pure AuroraIamToken { .. }

-- | Spawns a thread that refreshes the IAM auth token every minute
--
-- The IAM auth token lasts 15 minutes, but we refresh it every minute just to
-- be super safe.
--
spawnIamTokenRefreshThread
  :: PostgresConnectionConf -> IO (IORef AuroraIamToken)
spawnIamTokenRefreshThread conf = do
  tokenIORef <- newIORef =<< createAuroraIamToken conf
  void $ Immortal.create $ \_ -> Immortal.onFinish onFinishCallback $ do
    refreshIamToken conf tokenIORef
    threadDelay oneMinuteInMicroseconds
  pure tokenIORef
 where
  oneMinuteInMicroseconds = 60 * 1000000

  onFinishCallback (Left ex) =
    -- TODO: Somehow get MonadLogger-style error log message in here
    putStrLn $ "Error refreshing IAM auth token: " ++ show ex
  onFinishCallback (Right ()) = pure ()

refreshIamToken :: PostgresConnectionConf -> IORef AuroraIamToken -> IO ()
refreshIamToken conf tokenIORef = do
  token' <- createAuroraIamToken conf
  writeIORef tokenIORef token'

-- isAuroraIamTokenExpired :: AuroraIamToken -> IO Bool
-- isAuroraIamTokenExpired AuroraIamToken {..} = do
--   now <- getCurrentTime
--   let tenMinutesInSeconds = 60 * 15
--   pure $ now `diffUTCTime` aitCreatedAt > tenMinutesInSeconds

makePostgresPoolWith :: PostgresConnectionConf -> IO (Pool SqlBackend)
makePostgresPoolWith conf@PostgresConnectionConf {..} = case pccPassword of
  PostgresPasswordIamAuth -> makePostgresPoolWithIamAuth conf
  PostgresPasswordStatic password -> runNoLoggingT $ createPostgresqlPool
    (postgresConnectionString conf password)
    pccPoolSize

-- | Creates a PostgreSQL pool using IAM auth for the password.
makePostgresPoolWithIamAuth :: PostgresConnectionConf -> IO (Pool SqlBackend)
makePostgresPoolWithIamAuth conf@PostgresConnectionConf {..} = do
  tokenIORef <- spawnIamTokenRefreshThread conf
  runNoLoggingT $ createSqlPool (mkConn tokenIORef) pccPoolSize
 where
  -- TODO: Instead of refreshing the token before creating a connection, we
  -- could spawn a separate thread to refresh it on a timer. That way we don't
  -- waste time refreshing it when we want to make a new connection.
  mkConn tokenIORef logFunc = do
    token <- readIORef tokenIORef
    let connStr = postgresConnectionString conf (aitToken token)
    connectPostgreSQL connStr >>= openSimpleConn logFunc

postgresConnectionString :: PostgresConnectionConf -> String -> ByteString
postgresConnectionString PostgresConnectionConf {..} password =
  BS8.pack $ unwords
    [ "host=" <> pccHost
    , "port=" <> show pccPort
    , "user=" <> pccUser
    , "password=" <> password
    , "dbname=" <> pccDatabase
    ]
