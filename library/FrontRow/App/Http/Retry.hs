module FrontRow.App.Http.Retry
  ( RetriesExhausted(..)
  , rateLimited
  , rateLimited'
  ) where

import Prelude

import Control.Monad (guard, unless, void)
import Control.Monad.IO.Class (MonadIO)
import Control.Retry
import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (listToMaybe)
import Network.HTTP.Client (Request(..))
import Network.HTTP.Simple
import Network.HTTP.Types.Status (status429)
import Text.Read (readMaybe)
import UnliftIO.Exception (Exception(..), throwIO)

-- | Thrown if we exhaust our retries limit and still see a @429@
--
-- This typically means the API is not sending back accurate @Retry-In@ values
-- with 429 responses.
--
-- __Rationale__:
--
-- In order for 'rateLimited' to function in the case when the 'Request' is
-- using 'throwErrorStatusCodes' for 'checkResponse', we have to modify it to
-- not throw on 429s specifically. Otherwise, the first response would just
-- throw due to 4XX and never retry. However, in that case of someone expecting
-- invalid statuses to throw an exception, if we exhaust our retries and still
-- see a 429 at the end, an exception should be thrown.
--
-- Unfortunately, it's not possible to reuse the user-defined 'checkResponse' in
-- order to throw a uniform 'HttpException' in this case; so we throw this
-- ourselves instead.
--
data RetriesExhausted = RetriesExhausted
  { reLimit :: Int
  , reResponse :: Response ()
  }
  deriving stock Show

instance Exception RetriesExhausted where
  displayException RetriesExhausted {..} =
    "Retries exhaused after "
      <> show reLimit
      <> " attempts. Final response:\n"
      <> show reResponse

rateLimited
  :: MonadIO m => (Request -> m (Response body)) -> Request -> m (Response body)
rateLimited = rateLimited' 10

-- | 'rateLimited' but with configurable retry limit
rateLimited'
  :: MonadIO m
  => Int
  -> (Request -> m (Response body))
  -> Request
  -> m (Response body)
rateLimited' retryLimit f req = do
  resp <- retryingDynamic
    (limitRetries retryLimit)
    (\_ ->
      pure
        . maybe DontRetry (ConsultPolicyOverrideDelay . microseconds)
        . getRetryAfter
    )
    (\_ -> f $ suppressRetryStatusError req)

  checkRetriesExhausted retryLimit resp

suppressRetryStatusError :: Request -> Request
suppressRetryStatusError req = req
  { checkResponse = \req' resp ->
    unless (getResponseStatus resp == status429)
      $ originalCheckResponse req' resp
  }
  where originalCheckResponse = checkResponse req

checkRetriesExhausted :: MonadIO m => Int -> Response body -> m (Response body)
checkRetriesExhausted retryLimit resp
  | getResponseStatus resp == status429 = throwIO
  $ RetriesExhausted { reLimit = retryLimit, reResponse = void resp }
  | otherwise = pure resp

getRetryAfter :: Response body -> Maybe Int
getRetryAfter resp = do
  guard $ getResponseStatus resp == status429
  header <- listToMaybe $ getResponseHeader "Retry-After" resp
  readMaybe $ BS8.unpack header

microseconds :: Int -> Int
microseconds = (* 1000000)
