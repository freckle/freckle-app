{-# LANGUAGE NamedFieldPuns #-}

module Freckle.App.Random
  ( smallRandomSubsetOfLargeIntegerRange
  , Range (..)
  , NonEmptyRange (..)
  , inclusiveRange
  ) where

import Freckle.App.Prelude

import Control.Monad.Random (MonadRandom (..), Random)
import Control.Monad.ST (runST)
import Control.Monad.State.Strict (execStateT, get, put)
import Data.Functor ((<&>))
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Numeric.Natural (Natural)

import qualified Data.Set as Set

-- | A possibly-empty contiguous range of integers
data Range i
  = RangeEmpty
  | RangeNonEmpty (NonEmptyRange i)

-- | A nonempty contiguous range of integers
data NonEmptyRange i = NonEmptyRange
  { inclusiveMinBound :: i
  , offset :: Natural
  -- ^ The size of the range minus one
  }

inclusiveRange
  :: Integral i
  => i
  -- ^ Lower bound, inclusive
  -> i
  -- ^ Upper bound, inclusive
  -> Range i
inclusiveRange a b =
  if a <= b
    then RangeNonEmpty (NonEmptyRange a (fromIntegral (b - a)))
    else RangeEmpty

-- | Select a fixed number of items uniformly at random
--   from a contiguous range of integers
--
-- This process accommodates selecting from a large range, but only has
-- reasonable performance when the number of items being selected is small
-- (it is quadratic in the number of items).
--
-- If the requested size is greater than or equal to the range, the entire
-- range is returned.
--
-- e.g. @smallRandomSubsetOfLargeIntegerRange 10 (inclusiveRange 30 70)@
-- may produce something like @fromList [32,34,45,54,56,58,62,63,64,65]@.
smallRandomSubsetOfLargeIntegerRange
  :: (MonadRandom m, Random i, Integral i)
  => Natural
  -- ^ How many items are wanted
  -> Range i
  -> m (Set i)
smallRandomSubsetOfLargeIntegerRange n = \case
  RangeEmpty -> pure Set.empty
  RangeNonEmpty r ->
    fmap gaps $
      flip execStateT (RangeWithGaps r Set.empty) $
        for_ [1 .. n] $ \_ -> do
          get >>= (lift . randomlyRemove) >>= put

data RangeWithGaps i = RangeWithGaps
  { rangeWithoutGaps :: NonEmptyRange i
  , gaps :: Set i
  -- ^ The set of items that has been removed from the larger range
  }

-- | Randomly remove an item from a 'RangeWithGaps'.
--
-- This selects uniformly at random an item from a 'RangeWithGaps' and
-- removes it (adds it to the 'gaps' set).
--
-- If every item in the range has already been removed, this does nothing.
randomlyRemove
  :: (MonadRandom m, Random i, Integral i) => RangeWithGaps i -> m (RangeWithGaps i)
randomlyRemove rg =
  randomFromRangeWithGaps rg <&> \case
    Nothing -> rg
    Just i -> rg {gaps = Set.insert i (gaps rg)}

-- | Randomly select an item from a 'RangeWithGaps'
--
-- This selects uniformly at random an item from the range that is not
-- present in the 'gaps' set.
randomFromRangeWithGaps
  :: (MonadRandom m, Random i, Integral i)
  => RangeWithGaps i
  -> m (Maybe i)
randomFromRangeWithGaps rg =
  let
    RangeWithGaps {rangeWithoutGaps, gaps} = rg
    NonEmptyRange {inclusiveMinBound, offset} = rangeWithoutGaps
  in
    if fromIntegral (Set.size gaps) == offset + 1
      then pure Nothing
      else
        Just
          <$> do
            r <-
              (inclusiveMinBound +)
                <$> getRandomR (0, fromIntegral offset - fromIntegral (Set.size gaps))
            pure $ runST $ do
              xRef <- newSTRef r
              gapQueue <- newSTRef $ Set.toAscList gaps
              let go = do
                    x <- readSTRef xRef
                    readSTRef gapQueue >>= \case
                      g : gs | g <= x -> do
                        writeSTRef xRef (x + 1)
                        writeSTRef gapQueue gs
                        go
                      _ -> pure x
              go
