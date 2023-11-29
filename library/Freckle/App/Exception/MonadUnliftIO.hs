module Freckle.App.Exception.MonadUnliftIO
  ( throw
  , throwString
  , fromJustNoteM
  , catch
  , catches
  , try
  , checkpointCallStack

    -- * Miscellany
  , IO
  , MonadIO
  , MonadUnliftIO
  , module Freckle.App.Exception.Types
  ) where

import Freckle.App.Exception.Types

import Control.Applicative (pure)
import Data.Either (Either (..))
import Data.Function ((.))
import Data.Functor (fmap)
import Data.String (String)
import System.IO (IO)
import Data.Maybe (Maybe, maybe)
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

throwString
  :: forall m a
   . HasCallStack
  => MonadIO m
  => String
  -> m a
throwString = throw . StringException

fromJustNoteM :: (HasCallStack, MonadIO m) => String -> Maybe a -> m a
fromJustNoteM err = maybe (throwString err) pure

catch :: forall e m a. (MonadUnliftIO m, Exception e, HasCallStack) => m a -> (e -> m a) -> m a
catch = Annotated.catch

catches
  :: forall m a
   . MonadUnliftIO m
  => HasCallStack
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
