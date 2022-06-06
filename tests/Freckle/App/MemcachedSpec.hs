module Freckle.App.MemcachedSpec
  ( spec
  ) where

import Freckle.App.Prelude

import Control.Monad.Reader
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

runTestAppT :: MonadUnliftIO m => AppExample MemcachedClient a -> m (a, [Text])
runTestAppT f = liftIO $ do
  mc <- loadClient
  -- NB. we could use `withApp` and not need to call this runner ourselves
  -- within a plain-`IO` example -- except that we want to use
  -- `runCapturedLoggingT` and assert on the logged messages. We should add
  -- Blammo.Logging.Test to support this use-case.
  fmap (second $ map logLineToText) $ runCapturedLoggingT $ runReaderT
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
        key <- cacheKeyThrow "A"

        val <- caching key (cacheTTL 5) $ pure A
        mbs <- Memcached.get key

        liftIO $ val `shouldBe` A
        liftIO $ mbs `shouldBe` Just "A"

    it "logs, but doesn't fail, on deserialization errors" $ example $ do
      (_, msgs) <- runTestAppT $ do
        key <- cacheKeyThrow "B"

        val0 <- caching key (cacheTTL 5) $ pure B -- set
        val1 <- caching key (cacheTTL 5) $ pure B -- get will fail
        mbs <- Memcached.get key

        liftIO $ val0 `shouldBe` B
        liftIO $ val1 `shouldBe` B
        liftIO $ mbs `shouldBe` Just "Broken"

      fmap NE.last (NE.nonEmpty msgs)
        `shouldBe` Just "[Caching] error deserializing: invalid: \"Broken\""
