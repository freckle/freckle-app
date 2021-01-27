module FrontRow.App.Http.Retry
  ( rateLimited
  )
where

import Prelude

import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO)
import Control.Retry
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (listToMaybe)
import Network.HTTP.Simple
import Network.HTTP.Types.Status (status429)
import Text.Read (readMaybe)

rateLimited
  :: MonadIO m => (Request -> m (Response body)) -> Request -> m (Response body)
rateLimited f req = retryingDynamic
  (limitRetries 10)
  (\_ ->
    pure
      . maybe DontRetry (ConsultPolicyOverrideDelay . microseconds)
      . getRetryAfter
  )
  (\_ -> f req)

getRetryAfter :: Response body -> Maybe Int
getRetryAfter resp = do
  guard $ getResponseStatus resp == status429
  header <- listToMaybe $ getResponseHeader "Retry-After" resp
  readMaybe $ BS8.unpack header

microseconds :: Int -> Int
microseconds = (* 1000000)
