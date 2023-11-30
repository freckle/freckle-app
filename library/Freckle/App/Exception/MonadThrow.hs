module Freckle.App.Exception.MonadThrow
  ( throwM
  , throwString
  , fromJustNoteM
  , impossible
  , catch
  , catchJust
  , catches
  , try
  , tryJust
  , checkpointCallStack

    -- * Miscellany
  , MonadThrow
  , MonadCatch
  , MonadMask
  , module Freckle.App.Exception.Types
  ) where

import Freckle.App.Exception.Types

import Control.Applicative (pure)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Data.Either (Either (..))
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Maybe (Maybe, maybe)
import Data.String (String)
import GHC.IO.Exception (userError)
import GHC.Stack (withFrozenCallStack)

import qualified Control.Exception.Annotated as Annotated
import qualified Control.Monad.Catch

-- Throws an exception, wrapped in 'AnnotatedException' which includes a call stack
throwM
  :: forall e m a. HasCallStack => MonadThrow m => Exception e => e -> m a
throwM = Annotated.throw

throwString :: forall m a. HasCallStack => MonadThrow m => String -> m a
throwString = throwM . userError

fromJustNoteM :: (HasCallStack, MonadThrow m) => String -> Maybe a -> m a
fromJustNoteM err = maybe (throwString err) pure

impossible :: (HasCallStack, MonadThrow m) => m a
impossible = throwString "Impossible"

catch :: (HasCallStack, Exception e, MonadCatch m) => m a -> (e -> m a) -> m a
catch = Annotated.catch

catchJust
  :: forall e b m a
   . (HasCallStack, Exception e, MonadCatch m)
  => (e -> Maybe b)
  -> m a
  -> (b -> m a)
  -> m a
catchJust test action handler =
  withFrozenCallStack Annotated.catch action $ \e ->
    maybe (Control.Monad.Catch.throwM e) handler (test e)

catches
  :: forall m a
   . (HasCallStack, MonadCatch m)
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
  :: forall e m a
   . Exception e
  => MonadCatch m
  => m a
  -- ^ Action to run
  -> m (Either e a)
  -- ^ Returns 'Left' if the action throws an exception with a type
  --   of either @e@ or @'AnnotatedException' e@
try = Annotated.try

tryJust
  :: forall e b m a
   . Exception e
  => MonadCatch m
  => (e -> Maybe b)
  -> m a
  -- ^ Action to run
  -> m (Either b a)
tryJust test action =
  withFrozenCallStack Annotated.catch (Right <$> action) $ \e ->
    maybe (Control.Monad.Catch.throwM e) (pure . Left) (test e)

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
