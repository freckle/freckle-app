{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Freckle.App.Test
  ( AppExample (..)
  , appExample
  , withApp
  , beforeSql
  , expectationFailure
  , pending
  , pendingWith
  , expectJust
  , expectRight
  , expect
  , withFailureDetail

    -- * Types and classes
  , Alternative ((<|>))
  , Applicative ((*>), (<*), (<*>), liftA2, pure)
  , Bool (False, True)
  , Bounded (maxBound, minBound)
  , Char
  , Constraint
  , Double
  , Either (Left, Right)
  , Enum
    ( enumFrom
    , enumFromThen
    , enumFromThenTo
    , enumFromTo
    , fromEnum
    , pred
    , succ
    , toEnum
    )
  , Eq ((/=), (==))
  , Exception (displayException)
  , ExceptionHandler (ExceptionHandler)
  , Expectation
  , FilePath
  , Float
  , Floating
    ( (**)
    , acos
    , acosh
    , asin
    , asinh
    , atan
    , atanh
    , cos
    , cosh
    , exp
    , log
    , logBase
    , pi
    , sin
    , sinh
    , sqrt
    , tan
    , tanh
    )
  , Foldable
    ( elem
    , fold
    , foldMap
    , foldMap'
    , foldl
    , foldl'
    , foldr
    , foldr'
    , length
    , maximum
    , minimum
    , null
    , product
    , sum
    , toList
    )
  , Fractional ((/), fromRational, recip)
  , Functor ((<$), fmap)
  , Generic
  , HasCallStack
  , Hashable
  , HashMap
  , HashSet
  , Int
  , Int64
  , Integer
  , Integral (div, divMod, mod, quot, quotRem, rem, toInteger)
  , IO
  , IOError
  , LocalPool
  , Map
  , Maybe (Just, Nothing)
  , Monad ((>>), (>>=), return)
  , MonadFail (fail)
  , MonadIO (liftIO)
  , MonadReader
  , MonadUnliftIO
  , Monoid (mappend, mconcat, mempty)
  , Natural
  , NominalDiffTime
  , NonEmpty
  , Num ((*), (+), (-), abs, fromInteger, negate, signum)
  , Ord ((<), (<=), (>), (>=), compare, max, min)
  , Ordering (EQ, GT, LT)
  , Pool
  , PoolConfig
  , PrimMonad
  , Rational
  , Read (readList, readsPrec)
  , ReaderT
  , ReadS
  , Real (toRational)
  , RealFloat
    ( atan2
    , decodeFloat
    , encodeFloat
    , exponent
    , floatDigits
    , floatRadix
    , floatRange
    , isDenormalized
    , isIEEE
    , isInfinite
    , isNaN
    , isNegativeZero
    , scaleFloat
    , significand
    )
  , RealFrac (ceiling, floor, properFraction, round, truncate)
  , Semigroup ((<>))
  , Set
  , Show (show, showList, showsPrec)
  , ShowS
  , SomeException (SomeException)
  , Spec
  , String
  , Text
  , Traversable (mapM, sequence, sequenceA, traverse)
  , Type
  , type (~)
  , UTCTime
  , Vector
  , Void
  , Word

    -- * Functions and operators
  , ($!)
  , ($)
  , (&&&)
  , (&&)
  , (***)
  , (++)
  , (.)
  , (<$$>)
  , (<$>)
  , (<=<)
  , (=<<)
  , (>=>)
  , (^)
  , (^^)
  , all
  , and
  , any
  , appendFile
  , asTypeOf
  , asum
  , atMay
  , beforeAll
  , beforeWith
  , bimap
  , break
  , catch
  , catches
  , catchJust
  , catMaybes
  , concat
  , concatMap
  , const
  , context
  , createPool
  , curry
  , cycleMay
  , decodeUtf8
  , defaultPoolConfig
  , describe
  , destroyAllResources
  , destroyResource
  , drop
  , dropWhile
  , either
  , encodeUtf8
  , error
  , errorWithoutStackTrace
  , even
  , example
  , filter
  , find
  , first
  , fit
  , flip
  , fmapDefault
  , fold1
  , foldlM
  , foldMap1
  , foldMapDefault
  , foldrM
  , for
  , for_
  , forAccumM
  , forM
  , forM_
  , fromIntegral
  , fromJustNoteM
  , fromMaybe
  , fst
  , gcd
  , getChar
  , getContents
  , getCurrentTime
  , getLine
  , guard
  , headMay
  , id
  , impossible
  , initMay
  , interact
  , ioError
  , isJust
  , isNothing
  , it
  , iterate
  , join
  , lastMay
  , lcm
  , lex
  , lift
  , lines
  , listToMaybe
  , lookup
  , map
  , mapAccumL
  , mapAccumM
  , mapAccumR
  , mapM_
  , mapMaybe
  , maximumBy
  , maximumMay
  , maybe
  , maybeToList
  , minimumBy
  , minimumMay
  , msum
  , newPool
  , not
  , notElem
  , odd
  , optional
  , or
  , otherwise
  , pack
  , partitionEithers
  , print
  , putChar
  , putResource
  , putStr
  , putStrLn
  , readFile
  , readIO
  , readLn
  , readMay
  , readParen
  , reads
  , realToFrac
  , repeat
  , replicate
  , reverse
  , scanl
  , scanl1
  , scanr
  , scanr1
  , second
  , seq
  , sequence_
  , sequenceA_
  , setNumStripes
  , shouldBe
  , shouldContain
  , shouldEndWith
  , shouldMatchList
  , shouldNotBe
  , shouldNotContain
  , shouldNotReturn
  , shouldNotSatisfy
  , shouldReturn
  , shouldSatisfy
  , shouldStartWith
  , showChar
  , showParen
  , shows
  , showString
  , snd
  , span
  , splitAt
  , subtract
  , tailMay
  , take
  , takeResource
  , takeWhile
  , throwM
  , throwString
  , traverse_
  , try
  , tryJust
  , tryTakeResource
  , tryWithResource
  , tshow
  , uncurry
  , undefined
  , unless
  , unlines
  , unpack
  , until
  , unwords
  , unzip
  , unzip3
  , userError
  , void
  , when
  , withResource
  , words
  , writeFile
  , xit
  , zip
  , zip3
  , zipWith
  , zipWith3
  , (||)
  ) where

-- freckle-prelude-0.1.0.0 no longer exports some names this module has always
-- exported, and exports @throw@, which it has not. Import the former from where
-- they now live, and hide the latter, so the export list below is the same
-- whichever version is in use.
#if MIN_VERSION_freckle_prelude(0,1,0)
import Control.Monad.Fail (fail)
import Freckle.App.Exception (fromJustNoteM, throwString)
import Freckle.App.Prelude hiding (throw)
import Prelude (error, errorWithoutStackTrace)
#else
import Freckle.App.Prelude
#endif

import Data.Pool
import Test.Hspec
  ( Expectation
  , Spec
  , beforeAll
  , beforeWith
  , context
  , describe
  , example
  , fit
  , it
  , xit
  )
import Test.Hspec.Expectations.Lifted hiding (expectationFailure)

import Blammo.Logging (MonadLogger, MonadLoggerIO)
import Blammo.Logging.Setup (WithLogger (..))
import Blammo.Logging.ThreadContext (MonadMask)
import Control.Lens (view)
import Control.Monad.Base
import Control.Monad.Catch (ExitCase (..), MonadCatch, MonadThrow, mask)
import Control.Monad.Catch qualified
import Control.Monad.Fail qualified as Fail
import Control.Monad.Primitive
import Control.Monad.Random (MonadRandom (..))
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.List (intercalate)
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import Freckle.App.Database
  ( HasSqlPool (..)
  , HasStatsClient
  , MonadSqlTx (..)
  , runDB
  )
import Freckle.App.Dotenv qualified as Dotenv
import Freckle.App.Exception.MonadThrow qualified as MonadThrow
import Freckle.App.OpenTelemetry
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec qualified as Hspec hiding (expectationFailure)
import Test.Hspec.Core.Spec (Arg, Example, SpecWith, evaluateExample)
import Test.Hspec.Expectations.Lifted qualified as Hspec (expectationFailure)
import UnliftIO.Exception qualified as UnliftIO

-- | An Hspec example over some @App@ value
--
-- To disable logging in tests, you can either:
--
-- - Export @LOG_LEVEL=error@, if this would be quiet enough, or
-- - Export @LOG_DESTINATION=@/dev/null@ to fully silence
newtype AppExample app a = AppExample
  { unAppExample :: ReaderT app IO a
  }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadBase IO
    , MonadBaseControl IO
    , MonadCatch
    , MonadIO
    , MonadUnliftIO
    , MonadReader app
    , MonadThrow
    , Fail.MonadFail
    )
  deriving
    (MonadLogger, MonadLoggerIO)
    via WithLogger app IO

instance MonadMask (AppExample app) where
  mask = UnliftIO.mask
  uninterruptibleMask = UnliftIO.uninterruptibleMask
  generalBracket acquire release use = mask $ \unmasked -> do
    resource <- acquire
    b <-
      unmasked (use resource) `catch` \e -> do
        _ <- release resource (ExitCaseException e)
        MonadThrow.throwM e

    c <- release resource (ExitCaseSuccess b)
    pure (b, c)

instance MonadRandom (AppExample app) where
  getRandomR = liftIO . getRandomR
  getRandom = liftIO getRandom
  getRandomRs = liftIO . getRandomRs
  getRandoms = liftIO getRandoms

instance PrimMonad (AppExample app) where
  type PrimState (AppExample app) = PrimState IO
  primitive = liftIO . primitive

instance Example (AppExample app a) where
  type Arg (AppExample app a) = app

  evaluateExample (AppExample ex) params action =
    evaluateExample
      (action $ \app -> void $ runReaderT ex app)
      params
      ($ ())

instance HasTracer app => MonadTracer (AppExample app) where
  getTracer = view tracerL

instance
  (HasSqlPool app, HasStatsClient app, HasTracer app)
  => MonadSqlTx (SqlPersistT (AppExample app)) (AppExample app)
  where
  runSqlTx = runDB

-- | A type restricted version of id
--
-- Like 'example', which forces the expectation to 'IO', this can be used to
-- force the expectation to 'AppExample'.
--
-- This can be used to avoid ambiguity errors when your expectation uses only
-- polymorphic functions like 'runDB' or lifted 'shouldBe' et-al.
appExample :: AppExample app a -> AppExample app a
appExample = id

-- | Spec before helper
--
-- @
-- spec :: Spec
-- spec = 'withApp' loadApp $ do
-- @
--
-- Reads @.env.test@, then loads the application. Examples within this spec can
-- use any @'MonadReader' app@ (including 'runDB', if the app 'HasSqlPool').
withApp :: ((app -> IO ()) -> IO ()) -> SpecWith app -> Spec
withApp run = beforeAll Dotenv.loadTest . Hspec.aroundAll run

-- | Run the given SQL action before every spec item
beforeSql :: HasSqlPool app => SqlPersistT IO a -> SpecWith app -> SpecWith app
beforeSql f = beforeWith $ \app -> app <$ runSqlPool f (getSqlPool app)

expectationFailure :: (MonadIO m, HasCallStack) => String -> m a
expectationFailure msg = Hspec.expectationFailure msg >> error "unreachable"

pending :: MonadIO m => m ()
pending = liftIO Hspec.pending

pendingWith :: MonadIO m => String -> m ()
pendingWith msg = liftIO $ Hspec.pendingWith msg

-- | Unwraps 'Just', or throws an assertion failure if the 'Maybe' is 'Nothing'
--
-- Like @(\``shouldSatisfy`\` 'isJust')@, but returns the 'Just' value.
expectJust :: (HasCallStack, MonadIO m) => Maybe a -> m a
expectJust = \case
  Nothing -> expectationFailure "expected Just, but got Nothing"
  Just a -> pure a

-- | Unwraps 'Right', or throws an assertion failure if the 'Either' is 'Left'
--
-- Like @(\``shouldSatisfy`\` 'isRight')@, but returns the 'Right' value.
expectRight :: (HasCallStack, MonadIO m, Show a) => Either a b -> m b
expectRight = \case
  Left a -> expectationFailure $ "expected Right, but got " <> show a
  Right b -> pure b

-- | Applies an @a -> Maybe b@ function, then either unwraps a 'Just' result
--   or throws an assertion failure if the result is 'Nothing'
--
-- @'expect' f@ is like @(\``shouldSatisfy`\` f)@, but where @f@ is returns 'Maybe'
-- rather than 'Bool', and a successful result is returned for use in subsequent
-- assertions.
--
-- This can be useful with @lens@, for example: @b <- expect (preview _Left) a@
expect :: (HasCallStack, MonadIO m, Show a) => (a -> Maybe b) -> a -> m b
expect f a = case f a of
  Nothing -> expectationFailure $ "predicate failed on: " <> show a
  Just b -> pure b

-- | Prepend a message to any 'HUnitFailure' thrown by an action
--
-- This can be useful when you have additional information that was generated during
-- the execution of a test that would be helpful in understanding why the test failed.
withFailureDetail
  :: (HasCallStack, MonadUnliftIO m) => (HasCallStack => m a) -> String -> m a
withFailureDetail action message =
  catch action $ \(HUnitFailure l r) ->
    throwM $ HUnitFailure l $ case r of
      Reason reason -> Reason $ intercalate "\n\n" [message, reason]
      ExpectedButGot maybeReason expected got ->
        ExpectedButGot
          (Just $ intercalate "\n\n" $ message : maybeToList maybeReason)
          expected
          got

infix 0 `withFailureDetail`
