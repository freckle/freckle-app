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
-- == Database
--
-- Adding Database access requires an instance of @'HasSqlPool'@ on your @App@
-- type. Most often, this will be easiest if you indeed separate a @Config@
-- attribute:
--
-- > data Config = Config
-- >   { configDbPoolSize :: Int
-- >   , configLogSettings :: LogSettings
-- >   }
--
-- So you can isolate Env-related concerns
--
-- > loadConfig :: IO Config
-- > loadConfig = Env.parse id $ Config
-- >   <$> Env.var Env.auto "PGPOOLSIZE" (Env.def 1)
-- >   <*> LoggerEnv.parser
--
-- from the runtime application state:
--
-- > data App = App
-- >   { appConfig :: Config
-- >   , appLogger :: Logger
-- >   , appSqlPool :: SqlPool
-- >   }
-- >
-- > instance HasLogger App where
-- >   loggerL = appLogger $ \x y -> x { appLogger = y }
-- >
-- > instance HasSqlPool App where
-- >   getSqlPool = appSqlPool
--
-- The @"Freckle.App.Database"@ module provides @'makePostgresPool'@ for
-- building a Pool given this (limited) config data:
--
-- > loadApp :: (App -> IO a) -> IO a
-- > loadApp f = do
-- >   appConfig{..} <- loadConfig
-- >   appLogger <- newLogger configLoggerSettings
-- >   appSqlPool <- runLoggerLoggingT appLogger $ makePostgresPool configDbPoolSize
-- >   f App{..}
--
-- This unlocks @'runDB'@ for your application:
--
-- > myAppAction
-- >   :: (MonadIO m, MonadReader env m, HasSqlPool env)
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
-- If your app type 'HasSqlPool', you can use 'runDB' in your specs too:
--
-- > spec :: Spec
-- > spec = aroundAll loadApp $ do
-- >   describe "myQuery" $ do
-- >     it "works" $ do
-- >       result <- runDB myQuery :: AppExample App Text
-- >       result `shouldBe` "as expected"
--
module Freckle.App
  ( runApp
  , module X
  ) where

import Prelude

import Blammo.Logging as X
import Control.Monad.Reader as X
import Freckle.App.Database as X
import System.IO (BufferMode(..), hSetBuffering, stderr, stdout)

runApp
  :: HasLogger app
  => (forall b . (app -> IO b) -> IO b)
  -> ReaderT app (LoggingT IO) a
  -> IO a
runApp loadApp action = do
  -- Ensure output is streamed if in a Docker container
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  loadApp $ \app -> runLoggerLoggingT app $ runReaderT action app
