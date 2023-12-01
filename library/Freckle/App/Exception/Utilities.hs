module Freckle.App.Exception.Utilities
  ( fromExceptionAnnotated
  ) where

import Control.Applicative ((<|>))
import Control.Exception.Annotated
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe (..))

-- | Like 'fromException', but matches both @e@ and @'AnnotatedException' e@
--
-- If the exception type is @e@, the value returned is an 'AnnotatedException'
-- with no annotations.
fromExceptionAnnotated
  :: forall e. Exception e => SomeException -> Maybe (AnnotatedException e)
fromExceptionAnnotated e = annotated <|> notAnnotated
 where
  annotated = fromException @(AnnotatedException e) $ toException e
  notAnnotated =
    (\e' -> AnnotatedException {exception = e', annotations = []})
      <$> fromException @e (toException e)
