{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Freckle.App.Test
  ( AppExample (..)
  , appExample
  , withApp
  , withAppSql
  , beforeSql
  , expectationFailure
  , pending
  , pendingWith

    -- * Re-exports
  , module X
  ) where

import Freckle.App.Prelude as X

import Data.Pool as X
import Test.Hspec as X
  ( Expectation
  , Spec
  , beforeAll
  , beforeWith
  , context
  , describe
  , example
  , fit
  , it
  , xit
  )
import Test.Hspec.Expectations.Lifted as X hiding (expectationFailure)

import Blammo.Logging
import Control.Lens (view)
import Control.Monad.Base
import Control.Monad.Catch (ExitCase (..), MonadCatch, MonadThrow, mask)
import qualified Control.Monad.Catch
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Primitive
import Control.Monad.Random (MonadRandom (..))
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import Freckle.App.Database
  ( HasSqlPool (..)
  , HasStatsClient
  , MonadSqlTx (..)
  , runDB
  )
import qualified Freckle.App.Database.XRay as XRay
import qualified Freckle.App.Dotenv as Dotenv
import qualified Freckle.App.Exception.MonadThrow as MonadThrow
import Freckle.App.OpenTelemetry
import qualified Test.Hspec as Hspec hiding (expectationFailure)
import Test.Hspec.Core.Spec (Arg, Example, SpecWith, evaluateExample)
import qualified Test.Hspec.Expectations.Lifted as Hspec (expectationFailure)
import qualified UnliftIO.Exception as UnliftIO

-- | An Hspec example over some @App@ value
--
-- To disable logging in tests, you can either:
--
-- - Export @LOG_LEVEL=error@, if this would be quiet enough, or
-- - Export @LOG_DESTINATION=@/dev/null@ to fully silence
newtype AppExample app a = AppExample
  { unAppExample :: ReaderT app (LoggingT IO) a
  }
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
    , MonadLogger
    , Fail.MonadFail
    )

-- We could derive this in newer versions of unliftio-core, but defining it by
-- hand supports a few resolvers back, without CPP. This is just a copy of the
-- ReaderT instance,
--
-- https://hackage.haskell.org/package/unliftio-core-0.2.0.1/docs/src/Control.Monad.IO.Unlift.html#line-64
--
instance MonadUnliftIO (AppExample app) where
  {-# INLINE withRunInIO #-}
  withRunInIO inner =
    AppExample $ withRunInIO $ \run -> inner (run . unAppExample)

instance MonadMask (AppExample app) where
  mask = UnliftIO.mask
  uninterruptibleMask = UnliftIO.uninterruptibleMask
  generalBracket acquire release use = mask $ \unmasked -> do
    resource <- acquire
    b <-
      unmasked (use resource) `catch` \e -> do
        _ <- release resource (ExitCaseException e)
        MonadThrow.throwM e

    c <- release resource (ExitCaseSuccess b)
    pure (b, c)

instance MonadRandom (AppExample app) where
  getRandomR = liftIO . getRandomR
  getRandom = liftIO getRandom
  getRandomRs = liftIO . getRandomRs
  getRandoms = liftIO getRandoms

instance PrimMonad (AppExample app) where
  type PrimState (AppExample app) = PrimState IO
  primitive = liftIO . primitive

instance HasLogger app => Example (AppExample app a) where
  type Arg (AppExample app a) = app

  evaluateExample (AppExample ex) params action =
    evaluateExample
      (action $ \app -> void $ runLoggerLoggingT app $ runReaderT ex app)
      params
      ($ ())

instance HasTracer app => MonadTracer (AppExample app) where
  getTracer = view tracerL

instance XRay.MonadTracer (AppExample app) where
  getVaultData = pure Nothing

instance
  (HasSqlPool app, HasStatsClient app, HasTracer app)
  => MonadSqlTx (SqlPersistT (AppExample app)) (AppExample app)
  where
  runSqlTx = runDB

-- | A type restricted version of id
--
-- Like 'example', which forces the expectation to 'IO', this can be used to
-- force the expectation to 'AppExample'.
--
-- This can be used to avoid ambiguity errors when your expectation uses only
-- polymorphic functions like 'runDB' or lifted 'shouldBe' et-al.
appExample :: AppExample app a -> AppExample app a
appExample = id

-- | Spec before helper
--
-- @
-- spec :: Spec
-- spec = 'withApp' loadApp $ do
-- @
--
-- Reads @.env.test@, then loads the application. Examples within this spec can
-- use any @'MonadReader' app@ (including 'runDB', if the app 'HasSqlPool').
withApp :: ((app -> IO ()) -> IO ()) -> SpecWith app -> Spec
withApp run = beforeAll Dotenv.loadTest . Hspec.aroundAll run

-- | 'withApp', with custom DB 'Pool' initialization
--
-- Runs the given function on the pool before every spec item. For example, to
-- truncate tables.
withAppSql
  :: HasSqlPool app
  => SqlPersistT IO a
  -> ((app -> IO ()) -> IO ())
  -> SpecWith app
  -> Spec
withAppSql f run = withApp run . beforeSql f
{-# DEPRECATED withAppSql "Replace `withAppSql f g` with `withApp g . beforeSql f`" #-}

-- | Run the given SQL action before every spec item
beforeSql :: HasSqlPool app => SqlPersistT IO a -> SpecWith app -> SpecWith app
beforeSql f = beforeWith $ \app -> app <$ runSqlPool f (getSqlPool app)

expectationFailure :: (MonadIO m, HasCallStack) => String -> m a
expectationFailure msg = Hspec.expectationFailure msg >> error "unreachable"

pending :: MonadIO m => m ()
pending = liftIO Hspec.pending

pendingWith :: MonadIO m => String -> m ()
pendingWith msg = liftIO $ Hspec.pendingWith msg
