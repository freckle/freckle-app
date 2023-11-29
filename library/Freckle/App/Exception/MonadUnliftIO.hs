module Freckle.App.Exception.MonadUnliftIO
  ( throw
  , catch
  , try
  , checkpointCallStack

    -- * Miscellany
  , IO
  , MonadIO
  , MonadUnliftIO
  , module Freckle.App.Exception.Types
  ) where

import Freckle.App.Exception.Types

import Data.Either (Either (..))
import Data.Functor (fmap)
import System.IO (IO)
import UnliftIO (MonadIO, MonadUnliftIO)

import qualified Control.Exception.Annotated.UnliftIO as Annotated

-- Throws an exception, wrapped in 'AnnotatedException' which includes a call stack
throw
  :: forall e m a
   . HasCallStack
  => MonadIO m
  => Exception e
  => e
  -- ^ Exception to throw; see 'StringException' or 'Impossible' if you need an idea
  -> m a
throw = Annotated.throw

catch
  :: forall m a
   . MonadUnliftIO m
  => HasCallStack
  => [ExceptionHandler m a]
  -- ^ Recovery actions to run if the first action throws an exception
  --   with a type of either @e@ or @'AnnotatedException' e@
  -> m a
  -- ^ Action to run
  -> m a
catch handlers action =
  Annotated.catches
    action
    (fmap (\case (ExceptionHandler f) -> Annotated.Handler f) handlers)

try
  :: forall e m a
   . Exception e
  => MonadUnliftIO m
  => m a
  -- ^ Action to run
  -> m (Either e a)
  -- ^ Returns 'Left' if the action throws an exception with a type
  --   of either @e@ or @'AnnotatedException' e@
try = Annotated.try

-- | When dealing with a library that does not use 'AnnotatedException',
--   apply this function to augment its exceptions with call stacks.
checkpointCallStack
  :: forall m a
   . MonadUnliftIO m
  => HasCallStack
  => m a
  -- ^ Action that might throw whatever types of exceptions
  -> m a
  -- ^ Action that only throws 'AnnotatedException',
  --   where the annotations include a call stack
checkpointCallStack =
  Annotated.checkpointCallStack
