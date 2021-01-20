{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module FrontRow.App.Test
  ( AppExample
  , withApp
  , runAppTest
  , module X
  )
where

import Prelude

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Primitive
import Control.Monad.Random (MonadRandom(..))
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Pool as X
import Database.Persist.Sql as X (SqlBackend, SqlPersistT)
import FrontRow.App.Database as X
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
    , MonadFail
    , MonadLogger
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
-- spec = withApp loadApp $ do
-- @
--
-- Reads @.env.test@, then @.env@, loads a DB 'Pool' and passes that to the
-- given function to load the rest of the application. Examples within this spec
-- can use 'runAppTest', and 'runDB'.
--
withApp :: (Pool SqlBackend -> IO app) -> SpecWith app -> Spec
withApp load = beforeAll (loadEnvTest *> loadPool) . beforeWith load

loadEnvTest :: IO ()
loadEnvTest = loadEnvFrom ".env.test" >> loadEnv

loadPool :: IO (Pool SqlBackend)
loadPool = makePostgresPool

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
