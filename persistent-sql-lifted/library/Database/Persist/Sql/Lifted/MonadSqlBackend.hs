module Database.Persist.Sql.Lifted.MonadSqlBackend
  ( MonadSqlBackend (..)
  , liftSql
  ) where

import Prelude

import Control.Exception.Annotated.UnliftIO (checkpointCallStack)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (ReaderT (..), asks)
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Sql.Lifted.HasSqlBackend (HasSqlBackend, getSqlBackend)
import GHC.Stack (HasCallStack)

-- | A monadic context in which a SQL backend is available
--   for running database queries
class MonadUnliftIO m => MonadSqlBackend m where
  getSqlBackendM :: m SqlBackend

instance (HasSqlBackend r, MonadUnliftIO m) => MonadSqlBackend (ReaderT r m) where
  getSqlBackendM = asks getSqlBackend

-- | Generalize from 'SqlPersistT' to 'MonadSqlBackend'
liftSql :: (MonadSqlBackend m, HasCallStack) => ReaderT SqlBackend m a -> m a
liftSql (ReaderT f) = checkpointCallStack $ getSqlBackendM >>= f
