module Freckle.App.Ghci
  ( runDB
  , runDB'
  ) where

import Freckle.App.Prelude

import Blammo.Logging.Simple
import Database.Persist.Postgresql (runSqlPool)
import Database.Persist.Sql (SqlBackend)
import Freckle.App.Database (makePostgresPool)
import qualified Freckle.App.Dotenv as Dotenv

-- | Run a db action against .env
runDB :: ReaderT SqlBackend IO b -> IO b
runDB f = Dotenv.load *> runDB' f

-- | Run a db action
runDB' :: ReaderT SqlBackend IO b -> IO b
runDB' f = do
  pool <- runSimpleLoggingT makePostgresPool
  runSqlPool f pool
