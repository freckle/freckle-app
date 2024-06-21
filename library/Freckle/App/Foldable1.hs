module Freckle.App.Foldable1 (foldMap1') where

import Freckle.App.Prelude

import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup.Foldable (Foldable1)
import Data.Semigroup.Foldable qualified as Foldable1

-- | A strict left fold into some @'Semigroup'@
foldMap1' :: (Foldable1 t, Semigroup m) => (a -> m) -> t a -> m
foldMap1' f = go . Foldable1.toNonEmpty
 where
  go (a :| as) = foldl' (\acc x -> acc <> f x) (f a) as
