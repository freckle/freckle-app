{-# LANGUAGE CPP #-}

-- | Parse the shell environment for configuration
--
-- A minor extension of [envparse](https://hackage.haskell.org/package/envparse).
--
-- Usage:
--
-- > import Freckle.App.Env
-- >
-- > data Config = Config -- Example
-- >   { cBatchSize :: Natural
-- >   , cDryRun :: Bool
-- >   , cLogLevel :: LogLevel
-- >   }
-- >
-- > loadConfig :: IO Config
-- > loadConfig = parse id $ Config
-- >   <$> var auto "BATCH_SIZE" (def 1)
-- >   <*> switch "DRY_RUN" mempty
-- >   <*> flag (Off LevelInfo) (On LevelDebug) "DEBUG" mempty
module Freckle.App.Env
  ( module Env

    -- * Replacements
  , Off (..)
  , On (..)
  , flag

    -- * Extensions
  , Timeout (..)
  , eitherReader
  , time
  , keyValues
  , keyValue
  , splitOnParse
  , timeout
  ) where

import Prelude

import Control.Error.Util (note)
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import Env qualified
import Prelude qualified as Unsafe (read)

#if MIN_VERSION_envparse(0,5,2)
import Data.Bifunctor (second)
import Env hiding (eitherReader, flag)
#else
import Data.Bifunctor (first, second)
import Env hiding (flag)
#endif

-- | Designates the value of a parameter when a flag is not provided.
newtype Off a = Off a

-- | Designates the value of a parameter when a flag is provided.
newtype On a = On a

-- | Parse a simple flag
--
-- If the variable is present and non-empty in the environment, the active value
-- is returned, otherwise the default is used.
--
-- >>> data LogLevel = LevelDebug | LevelInfo | LevelWarn | LevelError | LevelOther Text deriving (Eq, Prelude.Show, Prelude.Read, Ord)
--
-- >>> flag (Off LevelInfo) (On LevelDebug) "DEBUG" mempty `parsePure` [("DEBUG", "1")]
-- Right LevelDebug
--
-- >>> flag (Off LevelInfo) (On LevelDebug) "DEBUG" mempty `parsePure` [("DEBUG", "")]
-- Right LevelInfo
--
-- >>> flag (Off LevelInfo) (On LevelDebug) "DEBUG" mempty `parsePure` []
-- Right LevelInfo
--
-- N.B. only the empty string is falsey:
--
-- >>> flag (Off LevelInfo) (On LevelDebug) "DEBUG" mempty `parsePure` [("DEBUG", "false")]
-- Right LevelDebug
--
-- >>> flag (Off LevelInfo) (On LevelDebug) "DEBUG" mempty `parsePure` [("DEBUG", "no")]
-- Right LevelDebug
flag :: Off a -> On a -> String -> Mod Flag a -> Parser Error a
flag (Off f) (On t) n m = Env.flag f t n m

-- | Create a 'Reader' from a simple parser function
--
-- This is a building-block for other 'Reader's
eitherReader :: (String -> Either String a) -> Env.Reader Error a
#if MIN_VERSION_envparse(0,5,2)
eitherReader = Env.eitherReader
#else
eitherReader f s = first (unread . suffix) $ f s
 where
  suffix x = x <> ": " <> show s
#endif

-- | Read a time value using the given format
--
-- >>> var (time "%Y-%m-%d") "TIME" mempty `parsePure` [("TIME", "1985-02-12")]
-- Right 1985-02-12 00:00:00 UTC
--
-- >>> var (time "%Y-%m-%d") "TIME" mempty `parsePure` [("TIME", "10:00PM")]
-- Left [("TIME",UnreadError "unable to parse time as %Y-%m-%d: \"10:00PM\"")]
time :: String -> Env.Reader Error UTCTime
time fmt =
  eitherReader $
    note ("unable to parse time as " <> fmt)
      . parseTimeM True defaultTimeLocale fmt

-- | Read key-value pairs
--
-- >>> var keyValues "TAGS" mempty `parsePure` [("TAGS", "foo:bar,baz:bat")]
-- Right [("foo","bar"),("baz","bat")]
--
-- Value-less keys are not supported:
--
-- >>> var keyValues "TAGS" mempty `parsePure` [("TAGS", "foo,baz:bat")]
-- Left [("TAGS",UnreadError "Key foo has no value: \"foo\"")]
--
-- Nor are key-less values:
--
-- >>> var keyValues "TAGS" mempty `parsePure` [("TAGS", "foo:bar,:bat")]
-- Left [("TAGS",UnreadError "Value bat has no key: \":bat\"")]
keyValues :: Env.Reader Error [(Text, Text)]
keyValues = splitOnParse ',' $ keyValue ':'

keyValue :: Char -> Env.Reader Error (Text, Text)
keyValue c =
  eitherReader $ go . second (T.drop 1) . T.breakOn (T.singleton c) . T.pack
 where
  go = \case
    (k, v) | T.null v -> Left $ "Key " <> T.unpack k <> " has no value"
    (k, v) | T.null k -> Left $ "Value " <> T.unpack v <> " has no key"
    (k, v) -> Right (k, v)

-- | Use 'splitOn' then call the given 'Reader' on each element
--
-- @
-- 'splitOnParse' c pure == 'splitOn' c
-- @
--
-- >>> var (splitOnParse @Error ',' nonempty) "X" mempty `parsePure` [("X", "a,b")] :: Either [(String, Error)] [Text]
-- Right ["a","b"]
--
-- >>> var (splitOnParse @Error ',' nonempty) "X" mempty `parsePure` [("X", ",,")] :: Either [(String, Error)] [Text]
-- Left [("X",EmptyError)]
splitOnParse :: Char -> Env.Reader e a -> Env.Reader e [a]
splitOnParse c p = traverse p <=< splitOn c

-- | Represents a timeout in seconds or milliseconds
data Timeout
  = TimeoutSeconds Int
  | TimeoutMilliseconds Int
  deriving stock (Show, Eq)

-- | Read a timeout value as seconds or milliseconds
--
-- >>> var timeout "TIMEOUT" mempty `parsePure` [("TIMEOUT", "10")]
-- Right (TimeoutSeconds 10)
--
-- >>> var timeout "TIMEOUT" mempty `parsePure` [("TIMEOUT", "10s")]
-- Right (TimeoutSeconds 10)
--
-- >>> var timeout "TIMEOUT" mempty `parsePure` [("TIMEOUT", "10ms")]
-- Right (TimeoutMilliseconds 10)
--
-- >>> var timeout "TIMEOUT" mempty `parsePure` [("TIMEOUT", "20m")]
-- Left [("TIMEOUT",UnreadError "must be {digits}(s|ms): \"20m\"")]
--
-- >>> var timeout "TIMEOUT" mempty `parsePure` [("TIMEOUT", "2m0")]
-- Left [("TIMEOUT",UnreadError "must be {digits}(s|ms): \"2m0\"")]
timeout :: Env.Reader Error Timeout
timeout = eitherReader $ parseTimeout . span isDigit
 where
  parseTimeout = \case
    ("", _) -> Left "must be {digits}(s|ms)"
    (digits, "") -> Right $ TimeoutSeconds $ Unsafe.read digits
    (digits, "s") -> Right $ TimeoutSeconds $ Unsafe.read digits
    (digits, "ms") ->
      Right $ TimeoutMilliseconds $ Unsafe.read digits
    _ -> Left "must be {digits}(s|ms)"
