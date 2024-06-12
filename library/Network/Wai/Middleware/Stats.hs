module Network.Wai.Middleware.Stats
  ( addThreadContextFromStatsTags
  , requestStats
  ) where

import Freckle.App.Prelude

import Blammo.Logging (Pair, withThreadContext)
import Control.Lens ((^.))
import Control.Monad.Reader (runReaderT)
import Data.Aeson ((.=))
import Data.Aeson.Key qualified as Key
import Freckle.App.Stats (HasStatsClient (..), tagsL)
import Freckle.App.Stats qualified as Stats
import Network.HTTP.Types.Status (Status (..))
import Network.Wai (Middleware, Request, requestMethod, responseStatus)

-- | Add any tags in the ambient 'StatsClient' to the logging context
addThreadContextFromStatsTags :: HasStatsClient env => env -> Middleware
addThreadContextFromStatsTags env app req respond = do
  let context = uncurry fromTag <$> env ^. statsClientL . tagsL
  withThreadContext context $ app req respond
 where
  fromTag :: Text -> Text -> Pair
  fromTag k v = Key.fromText k .= v

-- | Emit @requests@ and @response_time_ms@ metrics
requestStats
  :: HasStatsClient env => env -> (Request -> [(Text, Text)]) -> Middleware
requestStats env getTags app req respond = do
  start <- getCurrentTime
  app req $ \res -> do
    let tags =
          getTags req
            <> [ ("method", decodeUtf8 $ requestMethod req)
               , ("status", pack $ show $ statusCode $ responseStatus res)
               ]

    flip runReaderT env $ Stats.tagged tags $ do
      Stats.increment "requests"
      Stats.histogramSinceMs "response_time_ms" start

    respond res
