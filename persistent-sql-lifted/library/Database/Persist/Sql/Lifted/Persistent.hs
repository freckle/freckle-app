{-# LANGUAGE CPP #-}

-- | Wrappers that apply 'liftSql' to Persistent utilities of the same name.
module Database.Persist.Sql.Lifted.Persistent
  ( checkUnique
  , checkUniqueUpdateable
  , count
  , delete
  , deleteBy
  , deleteWhere
  , exists
  , get
  , getBy
  , getByValue
  , getEntity
  , getJust
  , getJustEntity
  , getMany
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
  , onlyUnique
  , putMany
  , replace
  , replaceUnique
  , repsert
  , repsertMany
  , selectFirst
  , selectKeys
  , selectKeysList
  , selectList
  , update
  , updateGet
  , updateWhere
  , upsert
  , upsertBy
  ) where

import Conduit (ConduitT, MonadResource, transPipe)
import Data.Bool (Bool)
import Data.Either (Either)
import Data.Eq (Eq)
import Data.Function (($))
import Data.Int (Int)
import Data.Map.Strict (Map)
import Data.Maybe (Maybe)
#if MIN_VERSION_base(4,17,0)
import Data.Type.Equality (type (~))
#endif
import Database.Persist
  ( AtLeastOneUniqueKey
  , Entity
  , Filter
  , OnlyOneUniqueKey
  , PersistEntity (..)
  , SelectOpt
  , Update
  )
import Database.Persist.Class qualified as P
import Database.Persist.Class.PersistEntity (SafeToInsert)
import Database.Persist.Sql.Lifted.Core (MonadSqlBackend, SqlBackend, liftSql)
import GHC.Stack (HasCallStack)

-- | Check whether there are any conflicts for unique keys with this entity
--   and existing entities in the database
checkUnique
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => a
  -> m (Maybe (Unique a))
  -- ^ 'Nothing' if the entity would be unique, and could thus safely
  --   be inserted. On a conflict, 'Just' the conflicting key.
checkUnique a = liftSql $ P.checkUnique a

-- | Check whether there are any conflicts for unique keys with this entity and existing entities in the database
--
-- This is useful for updating because it ignores conflicts when the particular entity already exists.
checkUniqueUpdateable
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => Entity a
  -> m (Maybe (Unique a))
  -- ^ 'Nothing' if the entity would stay unique, and could thus safely be updated.
  --   On a conflict, 'Just' the conflicting key.
checkUniqueUpdateable e = liftSql $ P.checkUniqueUpdateable e

-- | The total number of records fulfilling the given criteria
count
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => [Filter a]
  -- ^ If you provide multiple values in the list, the conditions are ANDed together.
  -> m Int
count fs = liftSql $ P.count fs

-- | Delete a specific record by identifier
--
-- Does nothing if record does not exist.
delete
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => Key a
  -> m ()
delete k = liftSql $ P.delete k

-- | Delete a specific record by unique key
--
-- Does nothing if no record matches.
deleteBy
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => Unique a
  -> m ()
deleteBy u = liftSql $ P.deleteBy u

-- | Delete all records matching the given criteria
deleteWhere
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => [Filter a]
  -- ^ If you provide multiple values in the list, the conditions are ANDed together.
  -> m ()
deleteWhere fs = liftSql $ P.deleteWhere fs

-- | Check if there is at least one record fulfilling the given criteria
exists
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => [Filter a]
  -- ^ If you provide multiple values in the list, the conditions are ANDed together.
  -> m Bool
exists fs = liftSql $ P.exists fs

-- | Get a record by identifier, if available
get
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => Key a
  -> m (Maybe a)
get k = liftSql $ P.get k

-- | Get a record by unique key, if available, returning both the identifier and the record
getBy
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => Unique a
  -> m (Maybe (Entity a))
getBy u = liftSql $ P.getBy u

-- Get a record by unique key, if available, returning both the identifier and the record
--
-- This function makes the most sense on entities with a single 'Unique' constructor.
getByValue
  :: forall a m
   . ( PersistEntityBackend a ~ SqlBackend
     , AtLeastOneUniqueKey a
     , MonadSqlBackend m
     , HasCallStack
     )
  => a
  -> m (Maybe (Entity a))
  -- ^ A record matching one of the unique keys.
getByValue a = liftSql $ P.getByValue a

-- | Get a record by identifier, if available
getEntity
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => Key a
  -> m (Maybe (Entity a))
getEntity k = liftSql $ P.getEntity k

-- | Get a record by identifier, if available, for a non-null (not 'Maybe') foreign key
--
-- Unsafe unless your database is enforcing that the foreign key is valid.
getJust
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => Key a
  -> m a
getJust k = liftSql $ P.getJust k

-- | Get a record by identifier, if available, for a non-null (not 'Maybe') foreign key
--
-- Unsafe unless your database is enforcing that the foreign key is valid.
getJustEntity
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => Key a
  -> m (Entity a)
getJustEntity k = liftSql $ P.getJustEntity k

-- | Get many records by their respective identifiers, if available
getMany
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => [Key a]
  -> m (Map (Key a) a)
getMany ks = liftSql $ P.getMany ks

-- | Create a new record in the database
insert
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     , MonadSqlBackend m
     , HasCallStack
     )
  => a
  -> m (Key a)
  -- ^ The auto-increment ID that was generated
insert a = liftSql $ P.insert a

-- | Create a new record in the database
insert_
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     , MonadSqlBackend m
     , HasCallStack
     )
  => a
  -> m ()
insert_ a = liftSql $ P.insert_ a

-- | Insert a value, checking for conflicts with any unique constraints
insertBy
  :: forall a m
   . ( PersistEntityBackend a ~ SqlBackend
     , AtLeastOneUniqueKey a
     , SafeToInsert a
     , MonadSqlBackend m
     , HasCallStack
     )
  => a
  -> m (Either (Entity a) (Key a))
  -- ^ If a duplicate exists in the database, it is returned as 'Left'.
  --   Otherwise, the new 'Key' is returned as 'Right'.
insertBy a = liftSql $ P.insertBy a

-- | Create a new record in the database, returning an auto-increment ID and the inserted record
insertEntity
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     , MonadSqlBackend m
     , HasCallStack
     )
  => a
  -> m (Entity a)
insertEntity a = liftSql $ P.insertEntity a

-- | Create multiple records in the database, with specified keys
insertEntityMany
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => [Entity a]
  -> m ()
insertEntityMany es = liftSql $ P.insertEntityMany es

-- | Create a new record in the database using the given key
insertKey
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => Key a
  -> a
  -> m ()
insertKey k a = liftSql $ P.insertKey k a

-- | Create multiple records in the database and return their 'Key's
insertMany
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     , MonadSqlBackend m
     , HasCallStack
     )
  => [a]
  -> m [Key a]
insertMany as = liftSql $ P.insertMany as

-- | Create multiple records in the database
insertMany_
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     , MonadSqlBackend m
     , HasCallStack
     )
  => [a]
  -> m ()
insertMany_ as = liftSql $ P.insertMany_ as

-- | Create a new record in the database
insertRecord
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     , MonadSqlBackend m
     , HasCallStack
     )
  => a
  -> m a
  -- ^ The record that was inserted
insertRecord a = liftSql $ P.insertRecord a

-- | Create a new record in the database
insertUnique
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     , MonadSqlBackend m
     , HasCallStack
     )
  => a
  -> m (Maybe (Key a))
  -- ^ An auto-increment ID, or 'Nothing' when the record couldn't be
  --   inserted because of a uniqueness constraint
insertUnique a = liftSql $ P.insertUnique a

-- | Create a new record in the database
insertUniqueEntity
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     , MonadSqlBackend m
     , HasCallStack
     )
  => a
  -> m (Maybe (Entity a))
  -- ^ An auto-increment ID and the inserted record, or 'Nothing' when the record
  --   couldn't be inserted because of a uniqueness constraint.
insertUniqueEntity a = liftSql $ P.insertUniqueEntity a

-- | Return the single unique key for a record
onlyUnique
  :: forall a m
   . ( PersistEntityBackend a ~ SqlBackend
     , OnlyOneUniqueKey a
     , MonadSqlBackend m
     , HasCallStack
     )
  => a
  -> m (Unique a)
onlyUnique a = liftSql $ P.onlyUnique a

-- | Put many records into the database
--
-- * Insert new records that do not exist (or violate any unique constraints);
-- * Replace existing records (matching any unique constraint).
putMany
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     , MonadSqlBackend m
     , HasCallStack
     )
  => [a]
  -- ^ A list of the records you want to insert or replace.
  -> m ()
putMany as = liftSql $ P.putMany as

-- | Replace the record in the database with the given key
--
-- The result is undefined if such record does not exist.
replace
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => Key a
  -> a
  -> m ()
replace k a = liftSql $ P.replace k a

-- | Attempt to replace the record of the given key with the given new record
--
-- First query the unique fields to make sure the replacement maintains uniqueness constraints.
replaceUnique
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , Eq (Unique a)
     , MonadSqlBackend m
     , HasCallStack
     )
  => Key a
  -> a
  -> m (Maybe (Unique a))
  -- ^ 'Nothing' if the replacement was made. If uniqueness is violated,
  --   'Just' the 'Unique' violation.
replaceUnique k a = liftSql $ P.replaceUnique k a

-- | Put the record in the database with the given key
--
-- If a record with the given key does not exist then a new record will be inserted.
repsert
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => Key a
  -> a
  -> m ()
repsert k a = liftSql $ P.repsert k a

-- | Put many entities into the database
--
-- For each item, if a record with the given key does not exist then a new record will be inserted.
repsertMany
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => [(Key a, a)]
  -> m ()
repsertMany kas = liftSql $ P.repsertMany kas

-- | Get just the first record for the criteria
selectFirst
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => [Filter a]
  -- ^ If you provide multiple values in the list, the conditions are ANDed together.
  -> [SelectOpt a]
  -> m (Maybe (Entity a))
selectFirst fs os = liftSql $ P.selectFirst fs os

-- | Get the 'Key's of all records matching the given criteria
selectKeys
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , MonadResource m
     , HasCallStack
     )
  => [Filter a]
  -- ^ If you provide multiple values in the list, the conditions are ANDed together.
  -> [SelectOpt a]
  -> ConduitT () (Key a) m ()
  -- ^ Keys corresponding to the filters and options provided
selectKeys fs os = transPipe liftSql $ P.selectKeys fs os

-- | Get the 'Key's of all records matching the given criteria
selectKeysList
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => [Filter a]
  -- ^ If you provide multiple values in the list, the conditions are ANDed together.
  -> [SelectOpt a]
  -> m [Key a]
selectKeysList fs os = liftSql $ P.selectKeysList fs os

selectList
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => [Filter a]
  -- ^ If you provide multiple values in the list, the conditions are ANDed together.
  -> [SelectOpt a]
  -> m [Entity a]
  -- ^ Entities corresponding to the filters and options provided
selectList fs os = liftSql $ P.selectList fs os

-- | Update individual fields on a specific record
update
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => Key a
  -> [Update a]
  -> m ()
update k us = liftSql $ P.update k us

-- | Update individual fields on a specific record, and retrieve the updated value from the database
--
-- This function will throw an exception if the given key is not found in the database.
updateGet
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => Key a
  -> [Update a]
  -> m a
updateGet k us = liftSql $ P.updateGet k us

-- | Update individual fields on any record matching the given criteria
updateWhere
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , MonadSqlBackend m
     , HasCallStack
     )
  => [Filter a]
  -- ^ If you provide multiple values in the list, the conditions are ANDed together.
  -> [Update a]
  -> m ()
updateWhere fs us = liftSql $ P.updateWhere fs us

-- | Update based on a uniqueness constraint or insert:
--
-- * Unsert the new record if it does not exist;
-- * If the record exists (matched via it's uniqueness constraint), then update the
--   existing record with the parameters which is passed on as list to the function.
upsert
  :: forall a m
   . ( PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     , OnlyOneUniqueKey a
     , MonadSqlBackend m
     , HasCallStack
     )
  => a
  -- ^ New record to insert
  -> [Update a]
  -- ^ Updates to perform if the record already exists
  -> m (Entity a)
  -- ^ The record in the database after the operation
upsert a us = liftSql $ P.upsert a us

-- | Update based on a given uniqueness constraint or insert:
--
-- * Insert the new record if it does not exist;
-- * Update the existing record that matches the given uniqueness constraint.
upsertBy
  :: forall a m
   . ( PersistEntity a
     , PersistEntityBackend a ~ SqlBackend
     , SafeToInsert a
     , MonadSqlBackend m
     , HasCallStack
     )
  => Unique a
  -- ^ Uniqueness constraint to find by
  -> a
  -- ^ New record to insert
  -> [Update a]
  -- ^ Updates to perform if the record already exists
  -> m (Entity a)
  -- ^ The record in the database after the operation
upsertBy u a us = liftSql $ P.upsertBy u a us
