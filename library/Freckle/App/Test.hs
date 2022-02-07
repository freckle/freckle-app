{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Freckle.App.Test
  ( AppExample
  , withApp
  , withAppSql
  , runAppTest
  , module X
  ) where

import Freckle.App.Prelude

import Control.Monad.Base
import Control.Monad.Catch
import qualified Control.Monad.Fail as Fail
import Control.Monad.Logger
import Control.Monad.Primitive
import Control.Monad.Random (MonadRandom(..))
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Pool as X
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import Freckle.App.Database as X
import LoadEnv
import Test.Hspec as X
  (Spec, beforeAll, beforeWith, context, describe, example, fit, it, xit)
import Test.Hspec.Core.Spec (Arg, Example, SpecWith, evaluateExample)
import Test.Hspec.Expectations.Lifted as X

-- | An Hspec example over some @App@ value
newtype AppExample app a = AppExample (NoLoggingT (ReaderT app IO) a)
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadBase IO
    , MonadBaseControl IO
    , MonadCatch
    , MonadIO
    , MonadReader app
    , MonadThrow
    , MonadUnliftIO
    , MonadLogger
    , Fail.MonadFail
    )

instance MonadRandom (AppExample app) where
  getRandomR = liftIO . getRandomR
  getRandom = liftIO getRandom
  getRandomRs = liftIO . getRandomRs
  getRandoms = liftIO getRandoms

instance PrimMonad (AppExample app) where
  type PrimState (AppExample app) = PrimState IO
  primitive = liftIO . primitive

instance Example (AppExample app a) where
  type Arg (AppExample app a) = app

  evaluateExample (AppExample ex) params action = evaluateExample
    (action $ \app -> void $ runReaderT (runNoLoggingT ex) app)
    params
    ($ ())

-- | Spec before helper
--
-- @
-- spec :: Spec
-- spec = 'withApp' loadApp $ do
-- @
--
-- Reads @.env.test@, then @.env@, then loads the application. Examples within
-- this spec can use 'runAppTest' (and 'runDB', if the app 'HasSqlPool').
--
withApp :: IO app -> SpecWith app -> Spec
withApp load = beforeAll (loadEnvTest *> load)

-- | 'withApp', with custom DB 'Pool' initialization
--
-- Runs the given function on the pool before every spec item. For example, to
-- truncate tables.
--
withAppSql
  :: HasSqlPool app => SqlPersistT IO a -> IO app -> SpecWith app -> Spec
withAppSql f load = beforeAll (loadEnvTest *> load) . beforeWith setup
  where setup app = app <$ runSqlPool f (getSqlPool app)

loadEnvTest :: IO ()
loadEnvTest = loadEnvFrom ".env.test" >> loadEnv

-- | Run an action with the test @App@
--
-- Like @'runApp'@, but without exception handling or logging
--
runAppTest :: ReaderT app (LoggingT IO) a -> AppExample app a
runAppTest action = do
  app <- ask

  liftIO $ runStderrLoggingT $ filterLogger (\_ _ -> False) $ runReaderT
    action
    app
