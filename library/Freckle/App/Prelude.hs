-- | Those functions and types we can't do without!
module Freckle.App.Prelude
  (
  -- * 'Prelude' as the starting point, without common partial functions
    module Prelude

  -- * Commonly imported types
  , Alternative
  , Exception
  , Generic
  , HashMap
  , HashSet
  , Hashable
  , Int64
  , Map
  , MonadIO
  , MonadReader
  , MonadUnliftIO
  , NominalDiffTime
  , NonEmpty
  , PrimMonad
  , ReaderT
  , Set
  , Text
  , UTCTime
  , Vector

  -- * Commonly used functions

  -- ** Lifts
  , lift
  , liftIO

  -- ** 'Text'
  , tshow

  -- ** 'Maybe'
  , catMaybes
  , fromMaybe
  , isJust
  , isNothing
  , listToMaybe
  , mapMaybe
  , maybeToList

  -- ** Safe alternatives to partial prelude functions (e.g. 'headMay')
  , module SafeAlternatives

  -- ** 'Either'
  , partitionEithers

  -- ** 'Foldable'
  , module Foldable

  -- ** 'Traversable'
  , module Traversable

  -- ** 'Functor'
  , (<$$>)

  -- ** 'Bifunctor'
  , bimap
  , first
  , second

  -- ** 'Applicative'
  , (<|>)
  , liftA2
  , optional

  -- ** 'Monad'
  , (<=<)
  , (>=>)
  , guard
  , join
  , unless
  , void
  , when

  -- ** 'Arrow'
  , (&&&)
  , (***)

  -- ** 'UTCTime'
  , getCurrentTime
  ) where

-- Use 'Prelude' as the starting point, removing common partial functions

import Prelude hiding
  (cycle, foldl1, foldr1, head, init, last, maximum, minimum, read, tail, (!!))

-- Commonly used types (and their commonly used functions)

import Control.Applicative (Alternative, liftA2, optional, (<|>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime)
import Data.Vector (Vector)
import GHC.Generics (Generic)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (Exception)

-- Safe alternatives to prelude functions

import Data.Semigroup.Foldable as SafeAlternatives (fold1, foldMap1)
import Safe as SafeAlternatives
  ( atMay
  , cycleMay
  , headMay
  , initMay
  , lastMay
  , maximumMay
  , minimumMay
  , readMay
  , tailMay
  )

-- Commonly used functions

import Control.Arrow ((&&&), (***))
import Control.Monad (guard, join, unless, void, when, (<=<), (>=>))
import Control.Monad.Trans (lift)
import Data.Bifunctor (bimap, first, second)
import Data.Either (partitionEithers)
import Data.Foldable as Foldable hiding (foldl1, foldr1)
import Data.Maybe
  (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import qualified Data.Text as T
import Data.Traversable as Traversable

infixl 4 <$$>
(<$$>) :: (Functor c, Functor d) => (a -> b) -> c (d a) -> c (d b)
f <$$> as = (f <$>) <$> as

-- | 'Show' as 'Text'
tshow :: Show a => a -> Text
tshow = T.pack . show
