-- | Read a Memcached Servers value, to support ENV-based configuration
--
-- Format:
--
-- @
-- memcached://[user[:password]@]host][:port],...
-- @
--
-- Usage with "Freckle.App.Env":
--
-- @
-- -- Required
-- Env.var (Env.eitherReader readMemcachedServers <=< Env.nonempty) "MEMCACHED_SERVERS" mempty
--
-- -- Default to localhost:11211
-- Env.var (Env.eitherReader readMemcachedServers) "MEMCACHED_SERVERS" (Env.def defaultMemcachedServers)
--
-- -- Default to disabled
-- Env.var (Env.eitherReader readMemcachedServers) "MEMCACHED_SERVERS" (Env.def emptyMemcachedServers)
-- @
module Freckle.App.Memcached.Servers
  ( MemcachedServers (..)
  , defaultMemcachedServers
  , emptyMemcachedServers
  , readMemcachedServers
  , toServerSpecs
  ) where

import Freckle.App.Prelude

import Control.Error.Util (note)
import qualified Data.Text as T
import qualified Database.Memcache.Client as Memcache
import Network.URI (URI (..), URIAuth (..), parseAbsoluteURI)

newtype MemcachedServers = MemcachedServers
  { unMemcachedServers :: [MemcachedServer]
  }

defaultMemcachedServers :: MemcachedServers
defaultMemcachedServers = MemcachedServers [defaultMemcachedServer]

emptyMemcachedServers :: MemcachedServers
emptyMemcachedServers = MemcachedServers []

readMemcachedServers :: String -> Either String MemcachedServers
readMemcachedServers =
  fmap MemcachedServers
    . traverse (readMemcachedServer . unpack)
    . filter (not . T.null)
    . map T.strip
    . T.splitOn ","
    . pack

toServerSpecs :: MemcachedServers -> [Memcache.ServerSpec]
toServerSpecs = map unMemcachedServer . unMemcachedServers

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

    pure
      Memcache.Auth
        { Memcache.username = encodeUtf8 u
        , Memcache.password = encodeUtf8 p
        }

setHost :: URIAuth -> Memcache.ServerSpec -> Memcache.ServerSpec
setHost auth ss = case uriRegName auth of
  "" -> ss
  rn -> ss {Memcache.ssHost = rn}

setPort :: URIAuth -> Memcache.ServerSpec -> Memcache.ServerSpec
setPort auth ss = fromMaybe ss $ do
  p <- case uriPort auth of
    "" -> Nothing
    (':' : p) -> Just p
    p -> Just p
  pure $ ss {Memcache.ssPort = p}

setAuth
  :: Memcache.Authentication -> Memcache.ServerSpec -> Memcache.ServerSpec
setAuth auth ss = ss {Memcache.ssAuth = auth}
