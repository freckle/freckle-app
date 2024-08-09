-- | Micro-framework for building a non-web application
--
-- This is a version of the /ReaderT Design Pattern/.
--
-- <https://www.fpcomplete.com/blog/2017/06/readert-design-pattern>
--
-- == Basic Usage
--
-- Start by defining a type to hold your global application state:
--
-- > data App = App
-- >   { appDryRun :: Bool
-- >   , appLogger :: Logger
-- >   }
--
-- This type can be as complex or simple as you want. It might hold a separate
-- @Config@ attribute or may keep everything as one level of properties. It
-- could even hold an @'IORef'@ if you need mutable application state.
--
-- The only requirements are 'HasLogger':
--
-- > instance HasLogger App where
-- >   loggerL = lens appLogger $ \x y -> x { appLogger = y }
--
-- and a bracketed function for building and using a value:
--
-- > loadApp :: (App -> m a) -> m a
-- > loadApp f = do
-- >   app <- -- ...
-- >   f app
--
-- It's likely you'll want to use @"Freckle.App.Env"@ to load your @App@:
--
-- > import qualified Blammo.Logger.LogSettings.Env as LoggerEnv
-- > import qualified Freckle.App.Env as Env
-- >
-- > loadApp f = do
-- >   app <- Env.parse id $ App
-- >     <$> Env.switch "DRY_RUN" mempty
-- >     <*> LoggerEnv.parser
--
-- Now you have application-specific actions that can do IO, log, and access
-- your state:
--
-- > myAppAction :: (MonadIO m, MonadLogger m, MonadReader App env) => m ()
-- > myAppAction = do
-- >   isDryRun <- asks appDryRun
-- >
-- >   if isDryRun
-- >     then logWarn "Skipping due to dry-run"
-- >     else liftIO $ fireTheMissles
--
-- These actions can be (composed of course, or) invoked by a @main@ that
-- handles the reader context and evaluating the logging action.
--
-- > main :: IO ()
-- > main = do
-- >   runApp loadApp $ do
-- >     myAppAction
-- >     myOtherAppAction
--
-- == 'AppT'
--
-- Functions like @myAppAction@ will be run in the concrete stack 'AppT', but
-- you should prefer using using constraints (e.g. @'MonadReader' app@). See its
-- docs for all the constraints it satisfies.
--
-- == Database
--
-- @
-- import "Freckle.App.Database"
-- import "Freckle.App.OpenTelemetry"
-- @
--
-- Adding Database access requires a few more instances on your @App@ type:
--
-- - @'HasSqlPool'@: so we can, you know, talk to a DB
-- - @'HasTracer'@: to satisfy @'MonadTracer'@ so we can trace @'runDB'@
-- - @'HasStatsClient'@: so we can manage connection count metrics
--
-- Most often, this will be easiest if you indeed separate a @Config@ attribute:
--
-- > data Config = Config
-- >   { configDbPoolSize :: Int
-- >   , configLogSettings :: LogSettings
-- >   , configStatsSettings :: StatsSettings
-- >   }
--
-- So you can isolate Env-related concerns
--
-- > loadConfig :: IO Config
-- > loadConfig = Env.parse id $ Config
-- >   <$> Env.var Env.auto "PGPOOLSIZE" (Env.def 1)
-- >   <*> LoggerEnv.parser
-- >   <*> envParseStatsSettings
--
-- from the runtime application state:
--
-- > data App = App
-- >   { appConfig :: Config
-- >   , appLogger :: Logger
-- >   , appSqlPool :: SqlPool
-- >   , appTracer :: Tracer
-- >   , appStatsClient :: StatsClient
-- >   }
-- >
-- > instance HasLogger App where
-- >   loggerL = appLogger $ \x y -> x { appLogger = y }
-- >
-- > instance HasSqlPool App where
-- >   getSqlPool = appSqlPool
-- >
-- > instance HasTracer App where
-- >   tracerL = lens appTracer $ \x y -> x { appTracer = y }
-- >
-- > instance HasStatsClient App where
-- >   statsClientL = lens appStatsClient $ \x y -> x { appStatsClient = y }
--
-- The @"Freckle.App.Database"@ module provides @'makePostgresPool'@ for
-- building a Pool given this (limited) config data:
--
-- > loadApp :: (App -> IO a) -> IO a
-- > loadApp f = do
-- >   appConfig{..} <- loadConfig
-- >   withLogger configLoggerSettings $ \appLogger ->
-- >     appSqlPool <- runWithLogger appLogger $ makePostgresPool configDbPoolSize
-- >     withTracerProvider $ \tracerProvider -> do
-- >       withStatsClient configStatsSettings $ \appStatsClient -> do
-- >         let appTracer = makeTracer tracerProvider "my-app" tracerOptions
-- >         f App{..}
--
-- This unlocks @'runDB'@ for your application:
--
-- > myAppAction
-- >   :: ( MonadUnliftIO m
-- >      , MonadTracer m
-- >      , MonadReader env m
-- >      , HasSqlPool env
-- >      , HasStatsClient env
-- >      )
-- >   => SqlPersistT m [Entity Something]
-- > myAppAction = runDB $ selectList [] []
--
-- == Testing
--
-- @"Freckle.App.Test"@ exposes an @'AppExample'@ type for examples in a
-- @'SpecWith' App@ spec. The can be run by giving your @loadApp@ function to
-- Hspec's @'aroundAll'@.
--
-- Using MTL-style constraints (i.e. 'MonadReader') means you can use your
-- actions directly in expectations, but you may need some type annotations:
--
-- > spec :: Spec
-- > spec = aroundAll loadApp $ do
-- >   describe "myAppAction" $ do
-- >     it "works" $ do
-- >       result <- myAppAction :: AppExample App Text
-- >       result `shouldBe` "as expected"
--
-- If your @App@ type has the required instances, you can use 'runDB' in your
-- specs too:
--
-- > spec :: Spec
-- > spec = aroundAll loadApp $ do
-- >   describe "myQuery" $ do
-- >     it "works" $ do
-- >       result <- runDB myQuery :: AppExample App Text
-- >       result `shouldBe` "as expected"
module Freckle.App
  ( runApp
  , setLineBuffering

    -- * Concrete transformer stack
  , AppT (..)
  , runAppT

    -- * Re-exports
  , module Blammo.Logging
  , module Control.Monad.Reader
  ) where

import Freckle.App.Prelude

import Blammo.Logging (MonadLogger, MonadLoggerIO)
import Blammo.Logging.Setup (WithLogger (..))
import Blammo.Logging.ThreadContext (MonadMask)
import Control.Lens (view)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Primitive (PrimMonad (..))
import Control.Monad.Reader
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Freckle.App.Database
import Freckle.App.Http (MonadHttp (..))
import Freckle.App.OpenTelemetry
import Freckle.App.OpenTelemetry.Context
import Freckle.App.OpenTelemetry.Http
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

runApp
  :: (forall b. (app -> IO b) -> IO b)
  -> AppT app IO a
  -> IO a
runApp loadApp action = do
  setLineBuffering
  loadApp $ runAppT action

-- | Ensure output is streamed if in a Docker container
--
-- 'runApp' calls this for you, but it may be useful if you're running the app
-- some other way.
setLineBuffering :: MonadIO m => m ()
setLineBuffering = liftIO $ do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

newtype AppT app m a = AppT
  { unAppT :: ReaderT app (ResourceT m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadResource
    , MonadReader app
    )
  deriving (MonadLogger, MonadLoggerIO) via WithLogger app (ResourceT m)

instance MonadTrans (AppT app) where
  lift = AppT . lift . lift

instance PrimMonad m => PrimMonad (AppT app m) where
  type PrimState (AppT app m) = PrimState m

  primitive = lift . primitive
  {-# INLINE primitive #-}

instance (MonadUnliftIO m, HasTracer app) => MonadHttp (AppT app m) where
  httpLbs req = inSpan (httpSpanName req) (httpSpanArguments req) $ do
    resp <- liftIO . httpLbs =<< injectContext req
    resp <$ addCurrentSpanAttributes (httpResponseAttributes resp)

instance (Monad m, HasTracer app) => MonadTracer (AppT app m) where
  getTracer = view tracerL

instance
  (MonadUnliftIO m, HasSqlPool app, HasStatsClient app, HasTracer app)
  => MonadSqlTx (ReaderT SqlBackend (AppT app m)) (AppT app m)
  where
  runSqlTx = runDB

runAppT :: MonadUnliftIO m => AppT app m a -> app -> m a
runAppT action app =
  runResourceT $ runReaderT (unAppT action) app
