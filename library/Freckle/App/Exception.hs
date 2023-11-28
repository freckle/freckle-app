-- |
--
-- == Which monadic constraints are you using?
--
-- In a 'MonadThrow'/'MonadCatch' setting, use 'throw', 'catch', and 'try'. These
-- can be used in situations that don't necessarily involve 'IO'.
--
-- In a 'MonadIO'/'MonadUnliftIO' setting where 'IO' is definitely part of your
-- monad stack, use the "...IO" family of functions: 'throwIO', 'catchIO', and 'tryIO'.
--
-- == Throwing
--
-- To throw an exception, use either 'throwIO' (in a 'MonadIO'/'MonadUnliftIO' setting)
-- or 'throw' (in a 'MonadThrow' setting). This throws the exception wrapped in
-- 'AnnotatedException', which includes a call stack.
--
-- If you're throwing an exception that is never intended to be caught (such as a "this
-- should never happen" situation), you can use 'stringException' to conveniently construct
-- the exception.
--
-- == Augmenting non-annotated exceptions
--
-- When dealing with a library that does not use 'AnnotatedException', wrap its actions
-- in 'checkpointCallStack'/'checkpointCallStackIO' to augment its exceptions with call
-- stacks.
module Freckle.App.Exception
  ( -- * Throw
    throw
  , throwIO
  , StringException (..)
  , Impossible (..)

    -- * Catch
  , catch
  , catchIO
  , ExceptionHandler (..)

    -- * Try
  , try
  , tryIO

    -- * Annotation
  , checkpointCallStack
  , checkpointCallStackIO

    -- * Miscellany
  , Exception (..)
  , SomeException (..)
  , AnnotatedException (..)
  , HasCallStack
  , MonadThrow
  , MonadCatch
  , MonadMask
  , IO
  , MonadIO
  , MonadUnliftIO
  ) where

import Control.Exception.Annotated (AnnotatedException, Handler (..))
import Control.Monad.Catch
  ( Exception
  , MonadCatch
  , MonadMask
  , MonadThrow
  , SomeException (..)
  )
import Data.Either (Either (..))
import Data.Function ((.))
import Data.Functor (fmap)
import Data.String (String)
import GHC.Stack (HasCallStack)
import System.IO (IO)
import Text.Show (Show (showsPrec), showString, shows)
import UnliftIO (MonadIO, MonadUnliftIO)

import qualified Control.Exception.Annotated
import qualified Control.Exception.Annotated.UnliftIO

throw
  :: forall e m a
   . HasCallStack
  => MonadThrow m
  => Exception e
  => e
  -- ^ Exception to throw; see 'StringException' if you need an idea
  -> m a
throw = Control.Exception.Annotated.throw

throwIO
  :: forall e m a
   . HasCallStack
  => MonadIO m
  => Exception e
  => e
  -- ^ Exception to throw; see 'StringException' if you need an idea
  -> m a
throwIO = Control.Exception.Annotated.UnliftIO.throw

catch
  :: (HasCallStack, MonadCatch m)
  => [ExceptionHandler m a]
  -- ^ Recovery actions to run if the first action throws an exception
  --   with a type of either @e@ or @'AnnotatedException' e@
  -> m a
  -- ^ Action to run
  -> m a
catch handlers action =
  Control.Exception.Annotated.catches
    action
    (fmap (\case (ExceptionHandler f) -> Handler f) handlers)

catchIO
  :: forall m a
   . MonadUnliftIO m
  => HasCallStack
  => [ExceptionHandler m a]
  -- ^ Recovery actions to run if the first action throws an exception
  --   with a type of either @e@ or @'AnnotatedException' e@
  -> m a
  -- ^ Action to run
  -> m a
catchIO handlers action =
  Control.Exception.Annotated.UnliftIO.catches
    action
    (fmap (\case (ExceptionHandler f) -> Handler f) handlers)

try
  :: Exception e
  => MonadCatch m
  => m a
  -- ^ Action to run
  -> m (Either e a)
  -- ^ Returns 'Left' if the action throws an exception with a type
  --   of either @e@ or @'AnnotatedException' e@
try = Control.Exception.Annotated.try

tryIO
  :: forall e m a
   . Exception e
  => MonadUnliftIO m
  => m a
  -- ^ Action to run
  -> m (Either e a)
  -- ^ Returns 'Left' if the action throws an exception with a type
  --   of either @e@ or @'AnnotatedException' e@
tryIO = Control.Exception.Annotated.UnliftIO.try

checkpointCallStack
  :: MonadCatch m
  => HasCallStack
  => m a
  -- ^ Action that might throw whatever types of exceptions
  -> m a
  -- ^ Action that only throws 'AnnotatedException',
  --   where the annotations include a call stack
checkpointCallStack =
  Control.Exception.Annotated.checkpointCallStack

checkpointCallStackIO
  :: forall m a
   . MonadUnliftIO m
  => HasCallStack
  => m a
  -- ^ Action that might throw whatever types of exceptions
  -> m a
  -- ^ Action that only throws 'AnnotatedException',
  --   where the annotations include a call stack
checkpointCallStackIO =
  Control.Exception.Annotated.UnliftIO.checkpointCallStack

-- | A convenient exception type with no particular meaning
newtype StringException = StringException String
  deriving anyclass (Exception)

instance Show StringException where
  showsPrec _ (StringException s) =
    shows @String "Exception:\n\n" . showString s

-- Renamed just so that it can go into Freckle.App.Prelude and have a less generic name than 'Handler'
data ExceptionHandler m a
  = forall e. Exception e => ExceptionHandler (e -> m a)

data Impossible = Impossible
  deriving stock (Show)
  deriving anyclass (Exception)
