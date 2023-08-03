module Freckle.Test.Yesod.GetBody
  ( getRawBody
  , getCsvBody
  , getJsonBody
  )
where

import Freckle.App.Prelude
import Freckle.Test.Yesod.MonadYesodExample
import Freckle.Test.Yesod.SResponse

import Data.Aeson (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BSL
import Data.Csv (FromNamedRecord, decodeByName)
import qualified Data.Vector as V
import UnliftIO.Exception (throwString)

-- | Get the body of the most recent response and decode it as JSON
getJsonBody :: forall a m site. (MonadYesodExample site m, FromJSON a) => m a
getJsonBody = either err pure . eitherDecode =<< getRawBody
 where
  err e = throwString $ "Error decoding JSON response body: " <> e

-- | Get the body of the most recent response and decode it as CSV
getCsvBody
  :: forall a m site. (MonadYesodExample site m, FromNamedRecord a) => m [a]
getCsvBody = either err (pure . V.toList . snd) . decodeByName =<< getRawBody
 where
  err e = throwString $ "Error decoding CSV response body: " <> e

-- | Get the body of the most recent response as a byte string
getRawBody :: forall m site. MonadYesodExample site m => m BSL.ByteString
getRawBody =
  fmap simpleBody . maybe (throwString "Test response had no body") pure
    =<< getResponse
