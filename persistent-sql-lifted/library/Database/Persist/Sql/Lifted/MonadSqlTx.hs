module Database.Persist.Sql.Lifted.MonadSqlTx
  ( MonadSqlTx (..)
  ) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Database.Persist.Sql.Lifted.MonadSqlBackend (MonadSqlBackend)
import GHC.Stack (HasCallStack)

-- | The constraint @'MonadSqlTx' db m@ indicates that @m@ is a monadic
--   context that can run @db@ actions, usually as a SQL transaction.
--   Typically, this means that @db@ needs a connection and @m@ can
--   provide one, e.g. from a connection pool.
class (MonadSqlBackend db, MonadUnliftIO m) => MonadSqlTx db m | m -> db where
  -- | Runs the action in a SQL transaction
  runSqlTx :: HasCallStack => db a -> m a
