module Freckle.Test.Yesod.HeaderAssertions
  ( assertHeader
  , assertHeaderContains
  , assertHeaderSatisfies
  , assertNoHeader
  )
where

import Freckle.App.Prelude
import Freckle.Test.Yesod.MonadYesodExample
import Freckle.Test.Yesod.SResponse

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.CaseInsensitive (CI)
import Test.Hspec.Expectations.Lifted (expectationFailure)
import qualified Yesod.Test

-- | Assert that the given header field's value satisfied some predicate
assertHeaderSatisfies
  :: forall m site
   . MonadYesodExample site m
  => CI ByteString
  -- ^ Field name
  -> String
  -- ^ Some description of the predicate; this is included
  --   in the error message if the assertion fails
  -> (ByteString -> Bool)
  -- ^ Predicate applied to the field value which is expected
  --   to return 'True'
  -> m ()
assertHeaderSatisfies header predicateDesc predicate = liftYesodExample $ withResponse $ \res ->
  case lookup header $ simpleHeaders res of
    Just value | predicate value -> pure ()
    Just value ->
      expectationFailure $
        concat
          [ "Expected header "
          , show header
          , " "
          , predicateDesc
          , ", but received "
          , show value
          ]
    Nothing ->
      expectationFailure $
        concat
          [ "Expected header "
          , show header
          , predicateDesc
          , ", but it was not present"
          ]

-- | Assert that the given header field's value contains
--   some particular byte string within it
assertHeaderContains
  :: MonadYesodExample site m
  => CI ByteString
  -- ^ Field name
  -> ByteString
  -- ^ Substring that we expect to find anywhere within the field value
  -> m ()
assertHeaderContains header substring =
  assertHeaderSatisfies
    header
    ("to contain " <> show substring)
    (substring `BS.isInfixOf`)

-- | Assert the given header key/value pair was returned
assertHeader
  :: forall m site
   . MonadYesodExample site m
  => CI ByteString
  -- ^ Field name
  -> ByteString
  -- ^ Expected field value
  -> m ()
assertHeader k v = liftYesodExample $ Yesod.Test.assertHeader k v

-- | Assert the given header was __not__ included in the response
assertNoHeader
  :: forall m site
   . MonadYesodExample site m
  => CI ByteString
  -- ^ Field name
  -> m ()
assertNoHeader = liftYesodExample . Yesod.Test.assertNoHeader
