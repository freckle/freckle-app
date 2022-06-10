{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Freckle.App.MemcachedSpec
  ( spec
  ) where

import Freckle.App.Prelude

import Control.Lens ((^?!))
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.List.NonEmpty as NE
import qualified Freckle.App.Env as Env
import Freckle.App.Memcached
import Freckle.App.Memcached.Client (MemcachedClient, newMemcachedClient)
import qualified Freckle.App.Memcached.Client as Memcached
import Freckle.App.Memcached.Servers
import Freckle.App.Test
import Freckle.App.Test.Logging

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

runTestAppT
  :: MonadUnliftIO m => AppExample MemcachedClient a -> m (a, [Maybe Value])
runTestAppT f = liftIO $ do
  mc <- loadClient
  -- NB. we could use `withApp` and not need to call this runner ourselves
  -- within a plain-`IO` example -- except that we want to use
  -- `runCapturedLoggingT` and assert on the logged messages. We should add
  -- Blammo.Logging.Test to support this use-case.
  fmap (second $ map logLineToJSON) $ runCapturedLoggingT $ runReaderT
    (unAppExample f)
    mc

loadClient :: IO MemcachedClient
loadClient = do
  servers <- Env.parse id $ Env.var
    (Env.eitherReader readMemcachedServers)
    "MEMCACHED_SERVERS"
    (Env.def defaultMemcachedServers)
  newMemcachedClient servers

spec :: Spec
spec = do
  describe "caching" $ do
    it "caches the given action by key using Cachable" $ example $ do
      void $ runTestAppT $ do
        k <- cacheKeyThrow "A"

        val <- caching k (cacheTTL 5) $ pure A
        mbs <- Memcached.get k

        liftIO $ val `shouldBe` A
        liftIO $ mbs `shouldBe` Just "A"

    it "logs, but doesn't fail, on deserialization errors" $ example $ do
      (_, msgs) <- runTestAppT $ do
        k <- cacheKeyThrow "B"

        val0 <- caching k (cacheTTL 5) $ pure B -- set
        val1 <- caching k (cacheTTL 5) $ pure B -- get will fail
        mbs <- Memcached.get k

        liftIO $ val0 `shouldBe` B
        liftIO $ val1 `shouldBe` B
        liftIO $ mbs `shouldBe` Just "Broken"

      let Just val = NE.last =<< NE.nonEmpty msgs
      val ^?! key "text" . _String `shouldBe` "Error deserializing"
      val ^?! key "meta" . key "action" . _String `shouldBe` "deserializing"
      val
        ^?! key "meta"
        . key "message"
        . _String
        `shouldBe` "invalid: \"Broken\""
