module Freckle.App.Exception.MonadThrow
  ( throw
  , throwString
  , catches
  , try
  , checkpointCallStack

    -- * Miscellany
  , MonadThrow
  , MonadCatch
  , MonadMask
  , module Freckle.App.Exception.Types
  ) where

import Freckle.App.Exception.Types

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Data.Either (Either (..))
import Data.Function ((.))
import Data.Functor (fmap)
import Data.String (String)

import qualified Control.Exception.Annotated as Annotated

-- Throws an exception, wrapped in 'AnnotatedException' which includes a call stack
throw
  :: forall e m a
   . HasCallStack
  => MonadThrow m
  => Exception e
  => e
  -- ^ Exception to throw; see 'StringException' or 'Impossible' if you need an idea
  -> m a
throw = Annotated.throw

throwString
  :: forall m a
   . HasCallStack
  => MonadThrow m
  => String
  -> m a
throwString = throw . StringException

catches
  :: (HasCallStack, MonadCatch m)
  => m a
  -- ^ Action to run
  -> [ExceptionHandler m a]
  -- ^ Recovery actions to run if the first action throws an exception
  --   with a type of either @e@ or @'AnnotatedException' e@
  -> m a
catches action handlers =
  Annotated.catches
    action
    (fmap (\case (ExceptionHandler f) -> Annotated.Handler f) handlers)

try
  :: Exception e
  => MonadCatch m
  => m a
  -- ^ Action to run
  -> m (Either e a)
  -- ^ Returns 'Left' if the action throws an exception with a type
  --   of either @e@ or @'AnnotatedException' e@
try = Annotated.try

-- | When dealing with a library that does not use 'AnnotatedException',
--   apply this function to augment its exceptions with call stacks.
checkpointCallStack
  :: MonadCatch m
  => HasCallStack
  => m a
  -- ^ Action that might throw whatever types of exceptions
  -> m a
  -- ^ Action that only throws 'AnnotatedException',
  --   where the annotations include a call stack
checkpointCallStack =
  Annotated.checkpointCallStack
