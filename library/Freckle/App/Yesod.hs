-- | Integration of "Freckle.App" tooling with "Yesod"
module Freckle.App.Yesod
  ( respondQueryCanceled
  , respondQueryCanceledHeaders
  , logExceptionsMiddleware
  ) where

import Freckle.App.Prelude

import Blammo.Logging
import Database.PostgreSQL.Simple (SqlError (..))
import Freckle.App.Exception
  ( AnnotatedException (..)
  , annotatedExceptionMessageFrom
  , fromException
  , withException
  )
import Freckle.App.Stats (HasStatsClient)
import qualified Freckle.App.Stats as Stats
import Network.HTTP.Types (ResponseHeaders, status503)
import qualified Network.Wai as W
import Yesod.Core.Handler (HandlerFor, sendWaiResponse)
import Yesod.Core.Types (HandlerContents)

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
respondQueryCanceledHeaders headers handler =
  catchJust queryCanceled handler $ \ex -> do
    logErrorNS "yesod" $ annotatedExceptionMessageFrom (const "Query canceled") ex
    Stats.increment "query_canceled"
    sendWaiResponse $ W.responseLBS status503 headers "Query canceled"

logExceptionsMiddleware :: (MonadUnliftIO m, MonadLogger m) => m a -> m a
logExceptionsMiddleware f =
  f `withException` \ex ->
    unless (isHandlerContents ex) $
      logErrorNS "yesod" $
        annotatedExceptionMessageFrom (const "Handler exception") ex

isHandlerContents :: AnnotatedException SomeException -> Bool
isHandlerContents = isJust . fromException @HandlerContents . exception

queryCanceled
  :: AnnotatedException SqlError -> Maybe (AnnotatedException SqlError)
queryCanceled ex = ex <$ guard (sqlState (exception ex) == "57014")
