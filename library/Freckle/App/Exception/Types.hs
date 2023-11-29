module Freckle.App.Exception.Types
  ( StringException (..)
  , Impossible (..)
  , ExceptionHandler (..)
  , AnnotatedException (..)
  , Exception (..)
  , SomeException (..)
  , HasCallStack
  ) where

import Control.Exception (Exception (..), SomeException (..))
import Control.Exception.Annotated (AnnotatedException (..))
import Data.Function ((.))
import Data.String (String)
import GHC.Stack (HasCallStack)
import Text.Show (Show (showsPrec), showString, shows)

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
