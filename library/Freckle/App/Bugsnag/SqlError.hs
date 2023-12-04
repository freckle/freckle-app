module Freckle.App.Bugsnag.SqlError
  ( sqlErrorBeforeNotify

    -- * Re-exports
  , SqlError

    -- * Exported for testing
  , sqlErrorGroupingHash
  ) where

import Freckle.App.Prelude

import Data.Bugsnag (Exception (..))
import Database.PostgreSQL.Simple (SqlError (..))
import Database.PostgreSQL.Simple.Errors
  ( ConstraintViolation (..)
  , constraintViolation
  )
import Freckle.App.Exception.Types (AnnotatedException)
import qualified Freckle.App.Exception.Types as Annotated
import Network.Bugsnag
  ( BeforeNotify
  , setGroupingHash
  , updateEventFromOriginalException
  , updateExceptions
  )

sqlErrorBeforeNotify :: BeforeNotify
sqlErrorBeforeNotify =
  updateEventFromOriginalException @(AnnotatedException SqlError)
    (asSqlError . Annotated.exception)

asSqlError :: SqlError -> BeforeNotify
asSqlError err@SqlError {..} = toSqlGrouping <> toSqlException
 where
  toSqlGrouping = maybe mempty setGroupingHash (sqlErrorGroupingHash err)
  toSqlException = updateExceptions $ \ex ->
    ex
      { exception_errorClass = decodeUtf8 $ "SqlError-" <> sqlState
      , exception_message =
          Just $
            decodeUtf8 $
              sqlErrorMsg
                <> ": "
                <> sqlErrorDetail
                <> " ("
                <> sqlErrorHint
                <> ")"
      }

sqlErrorGroupingHash :: SqlError -> Maybe Text
sqlErrorGroupingHash err = do
  violation <- constraintViolation err
  decodeUtf8 <$> case violation of
    ForeignKeyViolation table constraint -> pure $ table <> "." <> constraint
    UniqueViolation constraint -> pure constraint
    _ -> Nothing
