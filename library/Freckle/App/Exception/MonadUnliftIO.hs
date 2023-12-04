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
import Data.Either (Either (..))
import Data.Function (($), (.))
import Data.Functor (fmap, (<$>))
import Data.Maybe (Maybe, maybe)
import Data.String (String)
import GHC.IO.Exception (userError)
import GHC.Stack (withFrozenCallStack)
import System.IO (IO)
import UnliftIO (MonadIO, MonadUnliftIO)

import qualified Control.Exception.Annotated.UnliftIO as Annotated
import qualified UnliftIO.Exception

-- Throws an exception, wrapped in 'AnnotatedException' which includes a call stack
throwM :: forall e m a. (Exception e, MonadIO m, HasCallStack) => e -> m a
throwM = Annotated.throw

throwString :: forall m a. (MonadIO m, HasCallStack) => String -> m a
throwString = throwM . userError

fromJustNoteM
  :: forall m a. (MonadIO m, HasCallStack) => String -> Maybe a -> m a
fromJustNoteM err = maybe (throwString err) pure

impossible :: forall m a. (MonadIO m, HasCallStack) => m a
impossible = throwString "Impossible"

catch
  :: forall e m a
   . (Exception e, MonadUnliftIO m, HasCallStack)
  => m a
  -> (e -> m a)
  -> m a
catch = withFrozenCallStack Annotated.catch

catchJust
  :: forall e b m a
   . (Exception e, MonadUnliftIO m, HasCallStack)
  => (e -> Maybe b)
  -> m a
  -> (b -> m a)
  -> m a
catchJust test action handler =
  withFrozenCallStack $ Annotated.catch action $ \e ->
    maybe (UnliftIO.Exception.throwIO e) handler (test e)

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
   . (Exception e, MonadUnliftIO m, HasCallStack)
  => (e -> Maybe b)
  -> m a
  -- ^ Action to run
  -> m (Either b a)
tryJust test action =
  withFrozenCallStack $ Annotated.catch (Right <$> action) $ \e ->
    maybe (UnliftIO.Exception.throwIO e) (pure . Left) (test e)

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
checkpointCallStack =
  withFrozenCallStack Annotated.checkpointCallStack
