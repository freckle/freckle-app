-- | Read a Memcached Servers value, to support ENV-based configuration
--
-- Format:
--
-- @
-- memcached://[user[:password]@]host][:port],...
-- @
--
module Freckle.App.Memcached.Servers
  ( MemcachedServers(..)
  , defaultMemcachedServers
  , envParseMemcacheServers
  , toServerSpecs

  -- * Exported for testing
  , readMemcachedServers
  ) where

import Freckle.App.Prelude

import Control.Error.Util (note)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Database.Memcache.Client as Memcache
import qualified Freckle.App.Env as Env
import Network.URI (URI(..), URIAuth(..), parseAbsoluteURI)

newtype MemcachedServers = MemcachedServers
  { unMemcachedServers :: NonEmpty MemcachedServer
  }

defaultMemcachedServers :: MemcachedServers
defaultMemcachedServers = MemcachedServers $ pure defaultMemcachedServer

-- | Parse @MEMCACHED_SERVERS@ for a 'MemcachedServers' value
envParseMemcacheServers :: Env.Parser MemcachedServers
envParseMemcacheServers = Env.var
  (Env.eitherReader readMemcachedServers)
  "MEMCACHED_SERVERS"
  (Env.def defaultMemcachedServers)

readMemcachedServers :: String -> Either String MemcachedServers
readMemcachedServers =
  go . NE.nonEmpty . filter (not . T.null) . map T.strip . T.splitOn "," . pack
 where
  go = \case
    Nothing -> pure defaultMemcachedServers
    Just urls ->
      MemcachedServers <$> traverse (readMemcachedServer . unpack) urls

toServerSpecs :: MemcachedServers -> [Memcache.ServerSpec]
toServerSpecs = map unMemcachedServer . NE.toList . unMemcachedServers

newtype MemcachedServer = MemcachedServer
  { unMemcachedServer :: Memcache.ServerSpec
  }

defaultMemcachedServer :: MemcachedServer
defaultMemcachedServer = MemcachedServer Memcache.def

readMemcachedServer :: String -> Either String MemcachedServer
readMemcachedServer s = do
  uri <- note ("Not a valid URI: " <> s) $ parseAbsoluteURI s
  note "Must begin memcached://" $ guard $ uriScheme uri == "memcached:"

  let mAuth = uriAuthority uri

  pure
    . MemcachedServer
    . maybe id setHost mAuth
    . maybe id setPort mAuth
    . maybe id setAuth (readAuthentication . uriUserInfo =<< mAuth)
    $ Memcache.def

readAuthentication :: String -> Maybe Memcache.Authentication
readAuthentication = go . pack
 where
  go a = do
    (u, p) <- second (T.drop 1) . T.breakOn ":" <$> T.stripSuffix "@" a

    guard $ not $ T.null u
    guard $ not $ T.null p

    pure Memcache.Auth
      { Memcache.username = encodeUtf8 u
      , Memcache.password = encodeUtf8 p
      }

setHost :: URIAuth -> Memcache.ServerSpec -> Memcache.ServerSpec
setHost auth ss = case uriRegName auth of
  "" -> ss
  rn -> ss { Memcache.ssHost = rn }

setPort :: URIAuth -> Memcache.ServerSpec -> Memcache.ServerSpec
setPort auth ss = fromMaybe ss $ do
  p <- case uriPort auth of
    "" -> Nothing
    (':' : p) -> Just p
    p -> Just p
  pure $ ss { Memcache.ssPort = p }

setAuth
  :: Memcache.Authentication -> Memcache.ServerSpec -> Memcache.ServerSpec
setAuth auth ss = ss { Memcache.ssAuth = auth }
