{-# LANGUAGE CPP #-}

-- | Wrappers that apply 'liftSql' to Esqueleto utilities of the same name.
module Database.Persist.Sql.Lifted.Esqueleto
  ( delete
  , deleteCount
  , deleteKey
  , insertSelect
  , insertSelectCount
  , renderQueryDelete
  , renderQueryInsertInto
  , renderQuerySelect
  , renderQueryToText
  , renderQueryUpdate
  , select
  , selectOne
  , update
  , updateCount
  ) where

import Data.Function (($))
import Data.Int (Int64)
import Data.Maybe (Maybe)
import Data.Text (Text)
#if MIN_VERSION_base(4,17,0)
import Data.Type.Equality (type (~))
#endif
import Database.Esqueleto.Experimental
  ( Entity
  , PersistEntity (Key, PersistEntityBackend)
  , PersistValue
  , SqlExpr
  , SqlQuery
  )
import Database.Esqueleto.Experimental qualified as E
import Database.Esqueleto.Internal.Internal (Insertion, Mode, SqlSelect)
import Database.Persist.Sql.Lifted.Core (MonadSqlBackend, SqlBackend, liftSql)
import GHC.Stack (HasCallStack)

-- | Execute an Esqueleto DELETE query
delete :: forall m. (MonadSqlBackend m, HasCallStack) => SqlQuery () -> m ()
delete q = liftSql $ E.delete q

-- | Execute an Esqueleto DELETE query
deleteCount
  :: forall m
   . (MonadSqlBackend m, HasCallStack)
  => SqlQuery ()
  -> m Int64
  -- ^ The number of rows affected
deleteCount q = liftSql $ E.deleteCount q

-- | Delete a specific record by identifier
--
-- Does nothing if record does not exist.
deleteKey
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => Key a
  -> m ()
deleteKey k = liftSql $ E.deleteKey k

-- | Insert a 'E.PersistField' for every selected value
insertSelect
  :: forall a m
   . ( PersistEntity a
     , MonadSqlBackend m
     , HasCallStack
     )
  => SqlQuery (SqlExpr (Insertion a))
  -> m ()
insertSelect q = liftSql $ E.insertSelect q

-- | Insert a 'PersistField' for every selected value, returning the count
insertSelectCount
  :: forall a m
   . ( PersistEntity a
     , MonadSqlBackend m
     , HasCallStack
     )
  => SqlQuery (SqlExpr (Insertion a))
  -> m Int64
  -- ^ The number of inserted rows
insertSelectCount q = liftSql $ E.insertSelectCount q

-- | Renders a 'SqlQuery' to 'Text' along with the list of 'PersistValue's
--   that would be supplied to the database for @?@ placeholders
renderQueryDelete
  :: forall a r m
   . ( SqlSelect a r
     , MonadSqlBackend m
     , HasCallStack
     )
  => SqlQuery a
  -- ^ SQL query to render
  -> m (Text, [PersistValue])
renderQueryDelete q = liftSql $ E.renderQueryDelete q

-- | Renders a 'SqlQuery' to 'Text' along with the list of 'PersistValue's
--   that would be supplied to the database for @?@ placeholders
renderQueryInsertInto
  :: forall a r m
   . ( SqlSelect a r
     , MonadSqlBackend m
     , HasCallStack
     )
  => SqlQuery a
  -- ^ SQL query to render
  -> m (Text, [PersistValue])
renderQueryInsertInto q = liftSql $ E.renderQueryInsertInto q

-- | Renders a 'SqlQuery' to 'Text' along with the list of 'PersistValue's
--   that would be supplied to the database for @?@ placeholders
renderQuerySelect
  :: forall a r m
   . ( SqlSelect a r
     , MonadSqlBackend m
     , HasCallStack
     )
  => SqlQuery a
  -- ^ SQL query to render
  -> m (Text, [PersistValue])
renderQuerySelect q = liftSql $ E.renderQuerySelect q

-- | Renders a 'SqlQuery' to 'Text' along with the list of 'PersistValue's
--   that would be supplied to the database for @?@ placeholders
renderQueryToText
  :: forall a r m
   . ( SqlSelect a r
     , MonadSqlBackend m
     , HasCallStack
     )
  => Mode
  -- ^ Whether to render as an SELECT, DELETE, etc.
  --   You must ensure that the Mode you pass to this function corresponds
  --   with the actual SqlQuery. If you pass a query that uses incompatible
  --   features (like an INSERT statement with a SELECT mode) then you'll
  --   get a weird result.
  -> SqlQuery a
  -- ^ SQL query to render
  -> m (Text, [PersistValue])
renderQueryToText m q = liftSql $ E.renderQueryToText m q

-- | Renders a 'SqlQuery' to 'Text' along with the list of 'PersistValue's
--   that would be supplied to the database for @?@ placeholders
renderQueryUpdate
  :: forall a r m
   . ( SqlSelect a r
     , MonadSqlBackend m
     , HasCallStack
     )
  => SqlQuery a
  -- ^ SQL query to render
  -> m (Text, [PersistValue])
renderQueryUpdate q = liftSql $ E.renderQueryUpdate q

-- | Execute an Esqueleto SELECT query
select
  :: forall a r m
   . (SqlSelect a r, MonadSqlBackend m, HasCallStack)
  => SqlQuery a
  -> m [r]
  -- ^ A list of rows
select q = liftSql $ E.select q

-- | Execute an Esqueleto SELECT query, getting only the first row
selectOne
  :: forall a r m
   . (SqlSelect a r, MonadSqlBackend m, HasCallStack)
  => SqlQuery a
  -> m (Maybe r)
  -- ^ The first row, or 'Nothing' if no rows are selected
selectOne q = liftSql $ E.selectOne q

-- | Execute an Esqueleto UPDATE query
update
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => (SqlExpr (Entity a) -> SqlQuery ())
  -> m ()
update q = liftSql $ E.update q

-- | Execute an Esqueleto UPDATE query, returning the count
updateCount
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => (SqlExpr (Entity a) -> SqlQuery ())
  -> m Int64
  -- ^ The number of inserted rows
updateCount q = liftSql $ E.updateCount q
