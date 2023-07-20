-- | Integration of "Freckle.App" tooling with "Yesod"
module Freckle.App.Yesod
  ( respondQueryCanceled
  , respondQueryCanceledHeaders
  ) where

import Freckle.App.Prelude

import Blammo.Logging
import Database.PostgreSQL.Simple (SqlError (..))
import Freckle.App.Stats (HasStatsClient)
import qualified Freckle.App.Stats as Stats
import Network.HTTP.Types (ResponseHeaders, status503)
import qualified Network.Wai as W
import UnliftIO.Exception (displayException, handleJust)
import Yesod.Core.Handler (HandlerFor, sendWaiResponse)

-- | Catch 'SqlError' when queries are canceled due to timeout and respond 503
--
-- Also logs and increments a metric.
respondQueryCanceled
  :: HasStatsClient site => HandlerFor site res -> HandlerFor site res
respondQueryCanceled = respondQueryCanceledHeaders []

-- | 'respondQueryCanceledHeaders' but adding headers to the 503 response
respondQueryCanceledHeaders
  :: HasStatsClient site
  => ResponseHeaders
  -> HandlerFor site res
  -> HandlerFor site res
respondQueryCanceledHeaders headers = handleJust queryCanceled $ \ex -> do
  logError $ "Query canceled" :# ["exception" .= displayException ex]
  Stats.increment "query_canceled"
  sendWaiResponse $ W.responseLBS status503 headers "Query canceled"

queryCanceled :: SqlError -> Maybe SqlError
queryCanceled ex = ex <$ guard (sqlState ex == "57014")
