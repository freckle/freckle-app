module Network.Wai.Middleware.Stats
  ( requestStats
  ) where

import Freckle.App.Prelude

import Control.Monad.Reader (runReaderT)
import Freckle.App.Stats (HasStatsClient)
import qualified Freckle.App.Stats as Stats
import Network.HTTP.Types.Status (Status(..))
import Network.Wai (Middleware, Request, requestMethod, responseStatus)

requestStats
  :: HasStatsClient env => env -> (Request -> [(Text, Text)]) -> Middleware
requestStats env getTags app req respond = do
  start <- getCurrentTime
  app req $ \res -> do
    let
      tags =
        getTags req
          <> [ ("method", decodeUtf8 $ requestMethod req)
             , ("status", pack $ show $ statusCode $ responseStatus res)
             ]

    flip runReaderT env $ Stats.tagged tags $ do
      Stats.increment "requests"
      Stats.histogramSinceMs "response_time_ms" start

    respond res
