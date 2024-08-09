module AppExample
  ( AppExample (..)
  , appExample
  , withApp
  ) where

import Prelude

import Control.Lens (view)
import Control.Monad.Catch
import Control.Monad.Reader (MonadReader, ReaderT (..))
import Data.Functor (void)
import Freckle.App.Dotenv qualified as Dotenv
import OpenTelemetry.Trace (HasTracer (..))
import OpenTelemetry.Trace.Monad (MonadTracer (..))
import Test.Hspec (Spec, SpecWith, aroundAll, beforeAll)
import Test.Hspec.Core.Spec (Example (..))
import UnliftIO

withApp :: ((app -> IO ()) -> IO ()) -> SpecWith app -> Spec
withApp run = beforeAll Dotenv.loadTest . aroundAll run

-- | An Hspec example over some @app@ value
newtype AppExample app a = AppExample
  { unAppExample :: ReaderT app IO a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadCatch
    , MonadIO
    , MonadUnliftIO
    , MonadReader app
    , MonadThrow
    , MonadFail
    )

instance MonadMask (AppExample app) where
  mask = UnliftIO.mask
  uninterruptibleMask = UnliftIO.uninterruptibleMask
  generalBracket acquire release use = UnliftIO.mask $ \unmasked -> do
    resource <- acquire
    b <-
      unmasked (use resource) `UnliftIO.catch` \e -> do
        _ <- release resource (ExitCaseException e)
        throwM e

    c <- release resource (ExitCaseSuccess b)
    pure (b, c)

instance HasTracer app => MonadTracer (AppExample app) where
  getTracer = view tracerL

instance Example (AppExample app a) where
  type Arg (AppExample app a) = app

  evaluateExample (AppExample ex) params action =
    evaluateExample
      (action $ \app -> void $ runReaderT ex app)
      params
      ($ ())

appExample :: AppExample app a -> AppExample app a
appExample = id
