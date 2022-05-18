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
import Network.Socket (HostName, ServiceName)
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

  pure $ case uriAuthority uri of
    Nothing -> defaultMemcachedServer
    Just auth -> MemcachedServer $ Memcache.ServerSpec
      { Memcache.ssHost = fromEmpty defaultHost $ uriRegName auth
      , Memcache.ssPort = fromPort $ fromEmpty defaultPort $ uriPort auth
      , Memcache.ssAuth =
        fromMaybe defaultAuth $ readAuthentication $ uriUserInfo auth
      }

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

defaultHost :: HostName
defaultHost = Memcache.ssHost Memcache.def

defaultPort :: ServiceName
defaultPort = Memcache.ssPort Memcache.def

defaultAuth :: Memcache.Authentication
defaultAuth = Memcache.ssAuth Memcache.def

fromPort :: String -> String
fromPort = \case
  (':' : p) -> p
  p -> p

fromEmpty :: String -> String -> String
fromEmpty d s
  | null s = d
  | otherwise = s
