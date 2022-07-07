{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Freckle.App.MemcachedSpec
  ( spec
  ) where

import Freckle.App.Test

import Blammo.Logging.LogSettings
import Blammo.Logging.Logger
import Control.Lens (lens)
import Data.Aeson (Value(..))
import Data.Aeson.Compat as KeyMap
import qualified Data.List.NonEmpty as NE
import qualified Freckle.App.Env as Env
import Freckle.App.Memcached
import Freckle.App.Memcached.Client (MemcachedClient, newMemcachedClient)
import qualified Freckle.App.Memcached.Client as Memcached
import Freckle.App.Memcached.Servers

data ExampleValue
  = A
  | B
  | C
  deriving stock (Eq, Show)

instance Cachable ExampleValue where
  toCachable = \case
    A -> "A"
    B -> "Broken"
    C -> "C"

  fromCachable = \case
    "A" -> Right A
    "B" -> Right B
    "C" -> Right C
    x -> Left $ "invalid: " <> show x

data App = App
  { appMemcachedClient :: MemcachedClient
  , appLogger :: Logger
  }

instance HasMemcachedClient App where
  memcachedClientL =
    lens appMemcachedClient $ \x y -> x { appMemcachedClient = y }

instance HasLogger App where
  loggerL = lens appLogger $ \x y -> x { appLogger = y }

loadApp :: IO App
loadApp = do
  servers <- Env.parse id $ Env.var
    (Env.eitherReader readMemcachedServers)
    "MEMCACHED_SERVERS"
    (Env.def defaultMemcachedServers)

  App <$> newMemcachedClient servers <*> newTestLogger defaultLogSettings

spec :: Spec
spec = withApp loadApp $ do
  describe "caching" $ do
    it "caches the given action by key using Cachable" $ appExample $ do
      k <- cacheKeyThrow "A"

      val <- caching k (cacheTTL 5) $ pure A
      mbs <- Memcached.get k

      val `shouldBe` A
      mbs `shouldBe` Just "A"

    it "logs, but doesn't fail, on deserialization errors" $ appExample $ do
      k <- cacheKeyThrow "B"

      val0 <- caching k (cacheTTL 5) $ pure B -- set
      val1 <- caching k (cacheTTL 5) $ pure B -- get will fail
      mbs <- Memcached.get k

      val0 `shouldBe` B
      val1 `shouldBe` B
      mbs `shouldBe` Just "Broken"

      msgs <- getLoggedMessagesLenient
      let Just LoggedMessage {..} = NE.last <$> NE.nonEmpty msgs
      loggedMessageText `shouldBe` "Error deserializing"
      loggedMessageMeta `shouldBe` KeyMap.fromList
        [ ("action", String "deserializing")
        , ("message", String "invalid: \"Broken\"")
        ]
