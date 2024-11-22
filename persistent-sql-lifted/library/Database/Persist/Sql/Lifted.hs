{-# LANGUAGE CPP #-}

-- |
--
-- Re-exports from:
--
-- * "Database.Persist.Sql.Lifted.Core"
-- * "Database.Persist.Sql.Lifted.Persistent"
-- * "Database.Persist.Sql.Lifted.Esqueleto"
--
-- There are a few name conflicts between Persistent and Esqueleto. Where conflicts occur, this
-- module gives preference to Esqueleto. The following Persistent definitions are renamed:
--
-- * 'Database.Persist.Sql.Lifted.Persistent.delete' -> 'deleteKey'
-- * 'Database.Persist.Sql.Lifted.Persistent.update' -> 'update''
module Database.Persist.Sql.Lifted
  ( -- * Core concepts
    MonadSqlTx (..)
  , HasSqlBackend (..)
  , SqlBackend
  , MonadSqlBackend (..)
  , liftSql

    -- * Getting by key
  , get
  , getBy
  , getByValue
  , getEntity
  , getJust
  , getJustEntity
  , getMany

    -- * Selecting by filter
  , select
  , selectOne
  , selectFirst
  , selectKeys
  , selectKeysList
  , selectList

    -- * Selecting counts/existence
  , count
  , exists

    -- * Inserting
  , insertSelect
  , insertSelectCount
  , insert
  , insert_
  , insertBy
  , insertEntity
  , insertEntityMany
  , insertKey
  , insertMany
  , insertMany_
  , insertRecord
  , insertUnique
  , insertUniqueEntity

    -- * Updating
  , update
  , updateCount
  , update'
  , updateGet
  , updateWhere

    -- * Insert/update combinations
  , replace
  , replaceUnique
  , repsert
  , repsertMany
  , upsert
  , upsertBy
  , putMany

    -- * Working with unique constraints
  , checkUnique
  , checkUniqueUpdateable
  , onlyUnique

    -- * Deleting
  , delete
  , deleteKey
  , deleteBy
  , deleteWhere
  , deleteCount

    -- * Rendering queries to text
  , renderQueryDelete
  , renderQueryInsertInto
  , renderQuerySelect
  , renderQueryToText
  , renderQueryUpdate
  ) where

#if MIN_VERSION_base(4,17,0)
import Data.Type.Equality (type (~))
#endif
import Database.Persist (Key, PersistEntity (PersistEntityBackend), Update)
import Database.Persist.Sql.Lifted.Core
import Database.Persist.Sql.Lifted.Esqueleto
import Database.Persist.Sql.Lifted.Persistent hiding (delete, update)
import Database.Persist.Sql.Lifted.Persistent qualified as Persistent
import GHC.Stack (HasCallStack)

-- | Update individual fields on a specific record
update'
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => Key a
  -> [Update a]
  -> m ()
update' = Persistent.update
