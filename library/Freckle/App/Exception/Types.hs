module Freckle.App.Exception.Types
  ( ExceptionHandler (..)
  , AnnotatedException (..)
  , Exception (..)
  , SomeException (..)
  , HasCallStack
  ) where

import Control.Exception (Exception (..), SomeException (..))
import Control.Exception.Annotated (AnnotatedException (..))
import GHC.Stack (HasCallStack)

-- Renamed just so that it can go into Freckle.App.Prelude and have a less generic name than 'Handler'
data ExceptionHandler m a
  = forall e. Exception e => ExceptionHandler (e -> m a)
