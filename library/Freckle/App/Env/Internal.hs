{-# LANGUAGE TupleSections #-}

-- | Internal Env machinery exposed for testing
module Freckle.App.Env.Internal
  ( Error(..)
  , Parser(..)
  , bindParser
  , Reader(..)
  , Mod(..)
  , Var(..)
  , varParser
  ) where

import Freckle.App.Prelude

import Control.Applicative

-- | Environment parsing errors
data Error
  = UnsetError
  -- ^ A variable was not found, and no default was specified
  | InvalidError String
  -- ^ A variable was found, but it failed to parse
  deriving stock (Eq, Show)

isUnsetError :: Error -> Bool
isUnsetError UnsetError = True
isUnsetError (InvalidError _) = False

-- | Parse an Environment
--
-- Errors are accumulated into tuples mapping name to error.
--
newtype Parser a = Parser
  { unParser :: [(String, String)] -> Either [(String, Error)] a
  }
  deriving stock Functor

instance Applicative Parser where
  pure a = Parser . const $ Right a
  Parser f <*> Parser a = Parser $ \env -> case (f env, a env) of
    (Right f', Right a') -> Right $ f' a'

    -- Accumulate errors
    (Left e1, Left e2) -> Left $ e1 ++ e2
    (Left e, _) -> Left e
    (_, Left e) -> Left e

instance Alternative Parser where
  empty = Parser $ const $ Left []
  Parser f <|> Parser g = Parser $ \env -> case f env of
    Left ferrs | all (isUnsetError . snd) ferrs -> case g env of
      Left gerrs -> Left (ferrs ++ gerrs)
      y -> y
    x -> x

-- | Monadic bind for @'Parser'@
--
-- This short-circuits all parsing and is not ideal for an applicative style
-- parser, which ideally reports all errors instead of short-circuiting. As such
-- a `Monad` instance is not exposed for @'Parser'@.
--
bindParser :: Parser a -> (a -> Parser b) -> Parser b
bindParser (Parser f) g = Parser $ \envs -> do
  x <- f envs
  let h = unParser $ g x
  h envs

-- | Read a single environment variable's value
--
-- This will only ever fail with @'InvalidError'@, since @'UnsetError'@ is
-- handled before invoking any @'Reader'@.
--
newtype Reader a = Reader
  { unReader :: String -> Either Error a
  }
  deriving stock (Functor)

newtype Mod a = Mod (Var a -> Var a)

instance Semigroup (Mod a) where
  Mod f <> Mod g = Mod $ f . g

instance Monoid (Mod a) where
  mempty = Mod id

data Var a = Var
  { varName :: String
  , varReader :: Reader a
  , varDefault :: Maybe a
  }

varParser :: Var a -> Parser a
varParser Var {..} = Parser $ \env -> case (lookup varName env, varDefault) of
  (Nothing, Just d) -> Right d
  (Nothing, _) -> Left [(varName, UnsetError)]
  (Just v, _) -> first (pure . (varName, )) $ unReader varReader v
