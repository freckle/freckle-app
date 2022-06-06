module Freckle.App.Ghci
  ( runDB
  , runDB'
  , loadEnv
  , loadEnvTest
  ) where

import Freckle.App.Prelude

import Blammo.Logging.Simple
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlBackend)
import Freckle.App.Database (makePostgresPool)
import LoadEnv (loadEnv, loadEnvFrom)

-- | Run a db action against .env
runDB :: ReaderT SqlBackend IO b -> IO b
runDB f = loadEnv *> runDB' f

-- | Run a db action
runDB' :: ReaderT SqlBackend IO b -> IO b
runDB' f = do
  pool <- runSimpleLoggingT makePostgresPool
  runSqlPool f pool

loadEnvTest :: IO ()
loadEnvTest = loadEnvFrom ".env.test"
