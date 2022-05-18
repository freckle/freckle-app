module Freckle.App.MemcachedSpec
  ( spec
  ) where

import Freckle.App.Prelude

import Control.Monad.Logger
import Control.Monad.Reader
import qualified Data.List.NonEmpty as NE
import qualified Freckle.App.Env as Env
import Freckle.App.Memcached
import Freckle.App.Memcached.Client (MemcachedClient, newMemcachedClient)
import qualified Freckle.App.Memcached.Client as Memcached
import Freckle.App.Memcached.Servers (envParseMemcacheServers)
import Freckle.App.Test.Logging
import Test.Hspec

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

newtype TestAppT m a = TestAppT
  { unTestAppT :: ReaderT MemcachedClient (LoggingT m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader MemcachedClient
    , MonadIO
    , MonadUnliftIO
    , MonadLogger
    )

runTestAppT :: MonadUnliftIO m => TestAppT m a -> m (a, [Text])
runTestAppT f = do
  servers <- liftIO $ Env.parse envParseMemcacheServers
  mc <- newMemcachedClient servers
  runCapturedLoggingT $ runReaderT (unTestAppT f) mc

spec :: Spec
spec = do
  describe "caching" $ do
    it "caches the given action by key using Cachable" $ example $ do
      void $ runTestAppT $ do
        let Right key = cacheKey "A"

        val <- caching key (cacheTTL 5) $ pure A
        mbs <- Memcached.get =<< cacheKeyThrow "A"

        liftIO $ val `shouldBe` A
        liftIO $ mbs `shouldBe` Just "A"

    it "logs, but doesn't fail, on deserialization errors" $ example $ do
      (_, msgs) <- runTestAppT $ do
        let Right key = cacheKey "B"

        val0 <- caching key (cacheTTL 5) $ pure B -- set
        val1 <- caching key (cacheTTL 5) $ pure B -- get will fail
        mbs <- Memcached.get =<< cacheKeyThrow "B"

        liftIO $ val0 `shouldBe` B
        liftIO $ val1 `shouldBe` B
        liftIO $ mbs `shouldBe` Just "Broken"

      fmap NE.last (NE.nonEmpty msgs)
        `shouldBe` Just "[Caching] error deserializing: invalid: \"Broken\""
