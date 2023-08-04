module Freckle.Test.Yesod.JsonAssertions
  ( requireJSONResponse
  )
where

import Freckle.Test.Yesod.MonadYesodExample

import Yesod.Core (FromJSON)
import qualified Yesod.Test

-- | Parses the response body from JSON into a Haskell value,
--   throwing an error if parsing fails
--
-- This function also checks that the @Content-Type@ of the response
-- is @application/json@.
requireJSONResponse
  :: forall a m site. (MonadYesodExample site m, FromJSON a) => m a
requireJSONResponse = liftYesodExample Yesod.Test.requireJSONResponse
