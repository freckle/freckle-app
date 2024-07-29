{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Freckle.App.MemcachedSpec
  ( spec
  ) where

import Relude

import AppExample
import Blammo.Logging.LogSettings
import Blammo.Logging.Logger
import Control.Lens (lens, to, (^?))
import Data.Aeson (Value (..))
import Data.Aeson.Lens
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Freckle.App.Env qualified as Env
import Freckle.App.Memcached
import Freckle.App.Memcached.Client
  ( MemcachedClient
  , withMemcachedClient
  )
import Freckle.App.Memcached.Client qualified as Memcached
import Freckle.App.Memcached.Servers
import OpenTelemetry.Trace
  ( HasTracer (..)
  , Tracer
  , TracerProvider
  , initializeGlobalTracerProvider
  , makeTracer
  , shutdownTracerProvider
  , tracerOptions
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldSatisfy)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket)

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
  , appTracer :: Tracer
  }

instance HasMemcachedClient App where
  memcachedClientL =
    lens appMemcachedClient $ \x y -> x {appMemcachedClient = y}

instance HasLogger App where
  loggerL = lens appLogger $ \x y -> x {appLogger = y}

instance HasTracer App where
  tracerL = lens appTracer $ \x y -> x {appTracer = y}

loadApp :: (App -> IO a) -> IO a
loadApp f = do
  servers <-
    Env.parse id
      $ Env.var
        (Env.eitherReader readMemcachedServers)
        "MEMCACHED_SERVERS"
        (Env.def defaultMemcachedServers)
  appLogger <- newTestLogger defaultLogSettings
  withTracerProvider $ \tp -> do
    let appTracer = makeTracer tp "freckle-app" tracerOptions
    withMemcachedClient servers $ \appMemcachedClient -> do
      f App {..}

withTracerProvider :: MonadUnliftIO m => (TracerProvider -> m a) -> m a
withTracerProvider =
  bracket
    (liftIO initializeGlobalTracerProvider)
    (liftIO . shutdownTracerProvider)

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
      Object loggedMessageMeta
        ^? key "error"
          . key "message"
          . _String
        `shouldBe` Just "Unable to deserialize: invalid: \"Broken\""

      -- This assertion is far too brittle, but can be useful to un-comment if
      -- you intend to work on this logic specifically
      -- Object loggedMessageMeta ^? key "error" . key "stack" . _String . to T.lines
      --   `shouldBe` Just
      --     [ "CallStack (from HasCallStack):"
      --     , "  throwM, called at library/Freckle/App/Memcached.hs:121:30 in freckle-app-1.10.8.0-1ebuZKUCQVI9sAWTLATGfO:Freckle.App.Memcached"
      --     , "  cachingAs, called at library/Freckle/App/Memcached.hs:92:11 in freckle-app-1.10.8.0-1ebuZKUCQVI9sAWTLATGfO:Freckle.App.Memcached"
      --     , "  caching, called at tests/Freckle/App/MemcachedSpec.hs:87:15 in main:Freckle.App.MemcachedSpec"
      --     ]
      Object loggedMessageMeta
        ^? key "error"
          . key "stack"
          . _String
          . to T.lines
        `shouldSatisfy` maybe False (not . null)
