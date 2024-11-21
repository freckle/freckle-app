module Database.Persist.Sql.Lifted.HasSqlBackend
  ( HasSqlBackend (..)
  ) where

import Prelude

import Database.Persist.Sql (SqlBackend)

class HasSqlBackend a where
  getSqlBackend :: a -> SqlBackend

instance HasSqlBackend SqlBackend where
  getSqlBackend = id
