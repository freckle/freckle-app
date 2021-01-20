module FrontRow.App.Ghci
  ( runDB
  , runDB'
  , loadEnv
  , loadEnvTest
  )
where

import Prelude

import Control.Monad.Reader (ReaderT)
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlBackend)
import FrontRow.App.Database (makePostgresPool)
import LoadEnv (loadEnv, loadEnvFrom)

-- | Run a db action against .env
runDB :: ReaderT SqlBackend IO b -> IO b
runDB f = loadEnv *> runDB' f

-- | Run a db action
runDB' :: ReaderT SqlBackend IO b -> IO b
runDB' f = do
  pool <- makePostgresPool
  runSqlPool f pool

loadEnvTest :: IO ()
loadEnvTest = loadEnvFrom ".env.test"
