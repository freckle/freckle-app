module Freckle.App.Memcached.ServersSpec
  ( spec
  ) where

import Freckle.App.Prelude

import Control.Error.Util (hush)
import Data.Either (isLeft, isRight)
import qualified Database.Memcache.Client as Memcache
import Freckle.App.Memcached.Servers
import Freckle.App.Test

spec :: Spec
spec = do
  describe "readMemcachedServers" $ do
    it "requires the correct prefix" $ example $ do
      void (readMemcachedServers "http://") `shouldSatisfy` isLeft
      void (readMemcachedServers "memcached://") `shouldSatisfy` isRight

    it "treats empty cavlues as default" $ example $ do
      readServerSpecs "" `shouldBe` Just [Memcache.def]
      readServerSpecs "memcached://" `shouldBe` Just [Memcache.def]

    it "can set host" $ example $ do
      let mServer = readServerSpec "memcached://my-host"

      fmap Memcache.ssHost mServer `shouldBe` Just "my-host"
      fmap Memcache.ssPort mServer `shouldBe` Just "11211"
      fmap Memcache.ssAuth mServer `shouldBe` Just Memcache.NoAuth

    it "can set port" $ example $ do
      let mServer = readServerSpec "memcached://:11212"

      fmap Memcache.ssHost mServer `shouldBe` Just "127.0.0.1"
      fmap Memcache.ssPort mServer `shouldBe` Just "11212"
      fmap Memcache.ssAuth mServer `shouldBe` Just Memcache.NoAuth

    it "can set auth" $ example $ do
      let mServer = readServerSpec "memcached://user:password@"

      fmap Memcache.ssHost mServer `shouldBe` Just "127.0.0.1"
      fmap Memcache.ssPort mServer `shouldBe` Just "11211"
      fmap Memcache.ssAuth mServer
        `shouldBe` Just (Memcache.Auth "user" "password")

    it "refuses user-less or password-less auth" $ example $ do
      let
        mAuth1 = Memcache.ssAuth <$> readServerSpec "memcached://user:@"
        mAuth2 = Memcache.ssAuth <$> readServerSpec "memcached://:password@"

      mAuth1 `shouldBe` Just Memcache.NoAuth
      mAuth2 `shouldBe` Just Memcache.NoAuth

    it "can set lots at once" $ example $ do
      let mServer = readServerSpec "memcached://user:password@my-host:11212"

      fmap Memcache.ssHost mServer `shouldBe` Just "my-host"
      fmap Memcache.ssPort mServer `shouldBe` Just "11212"
      fmap Memcache.ssAuth mServer
        `shouldBe` Just (Memcache.Auth "user" "password")

    it "can do all of this for a list of servers" $ example $ do
      let
        mServerSpecs = readServerSpecs
          "memcached://a-host,memcached://b-host:11212,memcached://u:p@:11213"

      fmap (map Memcache.ssHost) mServerSpecs
        `shouldBe` Just ["a-host", "b-host", "127.0.0.1"]
      fmap (map Memcache.ssPort) mServerSpecs
        `shouldBe` Just ["11211", "11212", "11213"]
      fmap (map Memcache.ssAuth) mServerSpecs
        `shouldBe` Just
                     [Memcache.NoAuth, Memcache.NoAuth, Memcache.Auth "u" "p"]

readServerSpec :: String -> Maybe Memcache.ServerSpec
readServerSpec = headMay <=< readServerSpecs

readServerSpecs :: String -> Maybe [Memcache.ServerSpec]
readServerSpecs = fmap toServerSpecs . hush . readMemcachedServers
