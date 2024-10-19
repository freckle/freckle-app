module Freckle.App.Exception.MonadUnliftIO
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
  , IO
  , MonadIO
  , MonadUnliftIO
  , module Freckle.App.Exception.Types
  ) where

import Freckle.App.Exception.Types

import Control.Applicative (pure)
import Control.Exception.Annotated.UnliftIO (checkpoint, checkpointMany)
import Control.Monad (void)
import Data.Either (Either (..))
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Maybe (Maybe, maybe)
import Data.String (String)
import GHC.IO.Exception (userError)
import GHC.Stack (withFrozenCallStack)
import System.IO (IO)
import UnliftIO (MonadIO, MonadUnliftIO)

import Control.Exception.Annotated.UnliftIO qualified as Annotated
import UnliftIO.Exception qualified

-- Throws an exception, wrapped in 'AnnotatedException' which includes a call stack
throwM :: forall e m a. (Exception e, MonadIO m, HasCallStack) => e -> m a
throwM e = withFrozenCallStack $ Annotated.throw e

throwString :: forall m a. (MonadIO m, HasCallStack) => String -> m a
throwString s = withFrozenCallStack $ throwM $ userError s

fromJustNoteM
  :: forall m a. (MonadIO m, HasCallStack) => String -> Maybe a -> m a
fromJustNoteM err = withFrozenCallStack $ maybe (throwString err) pure

impossible :: forall m a. (MonadIO m, HasCallStack) => m a
impossible = withFrozenCallStack $ throwString "Impossible"

catch
  :: forall e m a
   . (Exception e, MonadUnliftIO m, HasCallStack)
  => m a
  -> (e -> m a)
  -> m a
catch action handler = withFrozenCallStack $ Annotated.catch action handler

catchJust
  :: forall e b m a
   . (Exception e, MonadUnliftIO m)
  => (e -> Maybe b)
  -> m a
  -> (b -> m a)
  -> m a
catchJust test action handler =
  withFrozenCallStack $
    UnliftIO.Exception.catches
      action
      [ UnliftIO.Exception.Handler $ \caught ->
          maybe (UnliftIO.Exception.throwIO caught) handler (test caught)
      , UnliftIO.Exception.Handler $ \caught@(AnnotatedException _ unwrapped) ->
          maybe (UnliftIO.Exception.throwIO caught) handler (test unwrapped)
      ]

-- action $ \e ->
--   maybe (UnliftIO.Exception.throwIO e) handler (test e)

catches
  :: forall m a
   . (MonadUnliftIO m, HasCallStack)
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
   . (Exception e, MonadUnliftIO m, HasCallStack)
  => m a
  -- ^ Action to run
  -> m (Either e a)
  -- ^ Returns 'Left' if the action throws an exception with a type
  --   of either @e@ or @'AnnotatedException' e@
try = withFrozenCallStack Annotated.try

tryJust
  :: forall e b m a
   . (Exception e, MonadUnliftIO m)
  => (e -> Maybe b)
  -> m a
  -- ^ Action to run
  -> m (Either b a)
tryJust test action =
  withFrozenCallStack $
    UnliftIO.Exception.catches
      (Right <$> action)
      [ UnliftIO.Exception.Handler $ \caught ->
          maybe (UnliftIO.Exception.throwIO caught) (pure . Left) (test caught)
      , UnliftIO.Exception.Handler $ \caught@(AnnotatedException _ unwrapped) ->
          maybe (UnliftIO.Exception.throwIO caught) (pure . Left) (test unwrapped)
      ]

withException
  :: forall e a m b
   . (Exception e, MonadUnliftIO m)
  => m a
  -> (e -> m b)
  -> m a
withException action onException =
  withFrozenCallStack $
    UnliftIO.Exception.catches
      action
      [ UnliftIO.Exception.Handler $ \caught -> do
          void $ onException caught
          UnliftIO.Exception.throwIO caught
      , UnliftIO.Exception.Handler $ \caught@(AnnotatedException _ unwrapped) -> do
          void $ onException unwrapped
          UnliftIO.Exception.throwIO caught
      ]

-- | When dealing with a library that does not use 'AnnotatedException',
--   apply this function to augment its exceptions with call stacks.
checkpointCallStack
  :: forall m a
   . (MonadUnliftIO m, HasCallStack)
  => m a
  -- ^ Action that might throw whatever types of exceptions
  -> m a
  -- ^ Action that only throws 'AnnotatedException',
  --   where the annotations include a call stack
checkpointCallStack action =
  withFrozenCallStack $
    Annotated.checkpointCallStack action
