module Freckle.App.MemcachedSpec
  ( spec
  ) where

import Freckle.App.Prelude

import Control.Monad.IO.Unlift (MonadUnliftIO(..))
import Control.Monad.Reader
import qualified Data.List.NonEmpty as NE
import qualified Freckle.App.Env as Env
import Freckle.App.Memcached
import Freckle.App.Memcached.Client (MemcachedClient, newMemcachedClient)
import qualified Freckle.App.Memcached.Client as Memcached
import Freckle.App.Memcached.Servers
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
    , MonadLogger
    )

-- We could derive this in newer versions of unliftio-core, but defining it by
-- hand supports a few resolvers back, without CPP. This is just a copy of the
-- ReaderT instance,
--
-- https://hackage.haskell.org/package/unliftio-core-0.2.0.1/docs/src/Control.Monad.IO.Unlift.html#line-64
--
instance MonadUnliftIO m => MonadUnliftIO (TestAppT m) where
  {-# INLINE withRunInIO #-}
  withRunInIO inner = TestAppT $ withRunInIO $ \run -> inner (run . unTestAppT)

runTestAppT :: MonadUnliftIO m => TestAppT m a -> m (a, [Text])
runTestAppT f = do
  servers <- liftIO $ Env.parse $ Env.var
    (Env.eitherReader readMemcachedServers)
    "MEMCACHED_SERVERS"
    (Env.def defaultMemcachedServers)
  mc <- newMemcachedClient servers
  fmap (second $ map logLineToText) $ runCapturedLoggingT $ runReaderT
    (unTestAppT f)
    mc

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
