module Freckle.App.BugsnagSpec
  ( spec
  ) where

import Freckle.App.Prelude

import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple (ExecStatus(..), SqlError(..))
import Freckle.App.Bugsnag
import Test.Hspec

spec :: Spec
spec = do
  describe "sqlErrorGroupingHash" $ do
    it "groups duplicate key errors by constraint name" $ do
      let
        err1 = sqlError
          { sqlState = duplicateKeyState
          , sqlErrorMsg = duplicateKeyErrorMsg "table_a_id_key"
          , sqlErrorDetail = "Key (a, b)=(1, 2) already exists"
          }
        err2 = sqlError
          { sqlState = duplicateKeyState
          , sqlErrorMsg = duplicateKeyErrorMsg "table_a_id_key"
          , sqlErrorDetail = "Key (a, b)=(3, 2) already exists"
          }
        err3 = sqlError
          { sqlState = duplicateKeyState
          , sqlErrorMsg = duplicateKeyErrorMsg "table_b_id_key"
          , sqlErrorDetail = "Key (a, b)=(1, 2) already exists"
          }

      -- All duplicate keys hashed
      sqlErrorGroupingHash err1 `shouldSatisfy` isJust
      sqlErrorGroupingHash err2 `shouldSatisfy` isJust
      sqlErrorGroupingHash err3 `shouldSatisfy` isJust

      -- 1 and 2 are grouped, 3 is not (different constraint)
      sqlErrorGroupingHash err1 `shouldBe` sqlErrorGroupingHash err2
      sqlErrorGroupingHash err1 `shouldNotBe` sqlErrorGroupingHash err3
      sqlErrorGroupingHash err2 `shouldNotBe` sqlErrorGroupingHash err3

    it "groups foreign key errors by constraint name" $ do
      let
        err1 = sqlError
          { sqlState = foreignKeyState
          , sqlErrorMsg = foreignKeyErrorMsg "table_a" "table_b_id_fkey"
          , sqlErrorDetail = "Key (a, b)=(1, 2) is still referenced"
          }
        err2 = sqlError
          { sqlState = foreignKeyState
          , sqlErrorMsg = foreignKeyErrorMsg "table_a" "table_b_id_fkey"
          , sqlErrorDetail = "Key (a, b)=(3, 2) is still referenced"
          }
        err3 = sqlError
          { sqlState = foreignKeyState
          , sqlErrorMsg = foreignKeyErrorMsg "table_b" "table_b_id_key"
          , sqlErrorDetail = "Key (a, b)=(1, 2) is still referenced"
          }
        err4 = sqlError
          { sqlState = foreignKeyState
          , sqlErrorMsg = foreignKeyErrorMsg "table_a" "table_a_id_key"
          , sqlErrorDetail = "Key (a, b)=(1, 2) is still referenced"
          }

      -- All errors hashed
      sqlErrorGroupingHash err1 `shouldSatisfy` isJust
      sqlErrorGroupingHash err2 `shouldSatisfy` isJust
      sqlErrorGroupingHash err3 `shouldSatisfy` isJust
      sqlErrorGroupingHash err4 `shouldSatisfy` isJust

      -- 1 and 2 are grouped, 3 and 4 are not (different table or constraint)
      sqlErrorGroupingHash err1 `shouldBe` sqlErrorGroupingHash err2
      sqlErrorGroupingHash err1 `shouldNotBe` sqlErrorGroupingHash err3
      sqlErrorGroupingHash err2 `shouldNotBe` sqlErrorGroupingHash err3
      sqlErrorGroupingHash err1 `shouldNotBe` sqlErrorGroupingHash err4
      sqlErrorGroupingHash err2 `shouldNotBe` sqlErrorGroupingHash err4
      sqlErrorGroupingHash err3 `shouldNotBe` sqlErrorGroupingHash err4

    it "does not group other SqlErrors" $ do
      let
        err = sqlError
          { sqlState = "9999"
          , sqlErrorMsg = duplicateKeyErrorMsg "table_a_id_key"
          , sqlErrorDetail = "Key (a, b)=(1, 2) already exists"
          }

      sqlErrorGroupingHash err `shouldBe` Nothing

    it "does not group other states" $ do
      let
        err = sqlError
          { sqlState = "12345"
          , sqlErrorMsg = "State is wrong, \"table_a_id_key\""
          , sqlErrorDetail = "Key (a, b)=(1, 2) already exists"
          }

      sqlErrorGroupingHash err `shouldBe` Nothing

    it "does not group when unable to parse" $ do
      let
        err = sqlError
          { sqlState = foreignKeyState
          , sqlErrorMsg = "FK needs two quoted values, \"table_a_id_fkey\""
          , sqlErrorDetail = "Key (a, b)=(1, 2) already exists"
          }

      sqlErrorGroupingHash err `shouldBe` Nothing

sqlError :: SqlError
sqlError = SqlError
  { sqlState = ""
  , sqlExecStatus = FatalError
  , sqlErrorMsg = ""
  , sqlErrorDetail = ""
  , sqlErrorHint = ""
  }

duplicateKeyState :: ByteString
duplicateKeyState = "23505"

duplicateKeyErrorMsg :: ByteString -> ByteString
duplicateKeyErrorMsg constraint =
  "duplicate key value violates unique constraint \"" <> constraint <> "\""

foreignKeyState :: ByteString
foreignKeyState = "23503"

foreignKeyErrorMsg :: ByteString -> ByteString -> ByteString
foreignKeyErrorMsg table constraint =
  "update or delete on table \""
    <> table
    <> "\" violates foreign key constraint \""
    <> constraint
    <> "\" on table \"unused\""
