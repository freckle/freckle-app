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
  , withException
  , checkpoint
  , checkpointMany
  , checkpointCallStack

    -- * Miscellany
  , MonadThrow
  , MonadCatch
  , MonadMask
  , module Freckle.App.Exception.Types
  ) where

import Freckle.App.Exception.Types

import Control.Applicative (pure)
import Control.Exception.Annotated (checkpoint, checkpointMany)
import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Data.Either (Either (..))
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Maybe (Maybe, maybe)
import Data.String (String)
import GHC.IO.Exception (userError)
import GHC.Stack (withFrozenCallStack)

import Control.Exception.Annotated qualified as Annotated
import Control.Monad.Catch qualified

-- Throws an exception, wrapped in 'AnnotatedException' which includes a call stack
throwM :: forall e m a. (Exception e, MonadThrow m, HasCallStack) => e -> m a
throwM e = withFrozenCallStack $ Annotated.throw e

throwString :: forall m a. (MonadThrow m, HasCallStack) => String -> m a
throwString s = withFrozenCallStack $ throwM $ userError s

fromJustNoteM
  :: forall m a. (MonadThrow m, HasCallStack) => String -> Maybe a -> m a
fromJustNoteM err = withFrozenCallStack $ maybe (throwString err) pure

impossible :: forall m a. (MonadThrow m, HasCallStack) => m a
impossible = withFrozenCallStack $ throwString "Impossible"

catch
  :: forall e m a
   . (Exception e, MonadCatch m, HasCallStack)
  => m a
  -> (e -> m a)
  -> m a
catch action handler = withFrozenCallStack $ Annotated.catch action handler

catchJust
  :: forall e b m a
   . (Exception e, MonadCatch m)
  => (e -> Maybe b)
  -> m a
  -> (b -> m a)
  -> m a
catchJust test action handler =
  Control.Monad.Catch.catches
    action
    [ Control.Monad.Catch.Handler $ \caught ->
        maybe (Control.Monad.Catch.throwM caught) handler (test caught)
    , Control.Monad.Catch.Handler $ \caught@(AnnotatedException _ unwrapped) ->
        maybe (Control.Monad.Catch.throwM caught) handler (test unwrapped)
    ]

catches
  :: forall m a
   . (MonadCatch m, HasCallStack)
  => m a
  -- ^ Action to run
  -> [ExceptionHandler m a]
  -- ^ Recovery actions to run if the first action throws an exception
  --   with a type of either @e@ or @'AnnotatedException' e@
  -> m a
catches action handlers =
  withFrozenCallStack $
    Annotated.catches
      action
      (fmap (\case (ExceptionHandler f) -> Annotated.Handler f) handlers)

try
  :: forall e m a
   . (Exception e, MonadCatch m, HasCallStack)
  => m a
  -- ^ Action to run
  -> m (Either e a)
  -- ^ Returns 'Left' if the action throws an exception with a type
  --   of either @e@ or @'AnnotatedException' e@
try action = withFrozenCallStack $ Annotated.try action

tryJust
  :: forall e b m a
   . (Exception e, MonadCatch m)
  => (e -> Maybe b)
  -> m a
  -- ^ Action to run
  -> m (Either b a)
tryJust test action =
  withFrozenCallStack $
    Control.Monad.Catch.catches
      (Right <$> action)
      [ Control.Monad.Catch.Handler $ \caught ->
          maybe (Control.Monad.Catch.throwM caught) (pure . Left) (test caught)
      , Control.Monad.Catch.Handler $ \caught@(AnnotatedException _ unwrapped) ->
          maybe (Control.Monad.Catch.throwM caught) (pure . Left) (test unwrapped)
      ]

withException
  :: forall e a m b
   . (Exception e, MonadCatch m)
  => m a
  -> (e -> m b)
  -> m a
withException action onException =
  withFrozenCallStack $
    Control.Monad.Catch.catches
      action
      [ Control.Monad.Catch.Handler $ \caught -> do
          void $ onException caught
          Control.Monad.Catch.throwM caught
      , Control.Monad.Catch.Handler $ \caught@(AnnotatedException _ unwrapped) -> do
          void $ onException unwrapped
          Control.Monad.Catch.throwM caught
      ]

-- | When dealing with a library that does not use 'AnnotatedException',
--   apply this function to augment its exceptions with call stacks.
checkpointCallStack
  :: forall m a
   . (MonadCatch m, HasCallStack)
  => m a
  -- ^ Action that might throw whatever types of exceptions
  -> m a
  -- ^ Action that only throws 'AnnotatedException',
  --   where the annotations include a call stack
checkpointCallStack action =
  withFrozenCallStack $ Annotated.checkpointCallStack action
