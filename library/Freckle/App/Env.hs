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
-- > loadConfig = parse $ Config
-- >   <$> var auto "BATCH_SIZE" (def 1)
-- >   <*> switch "DRY_RUN" mempty
-- >   <*> flag (Off LevelInfo) (On LevelDebug) "DEBUG" mempty
--
module Freckle.App.Env
  ( module Env

  -- * Replacements
  , Off(..)
  , On(..)
  , flag

  -- * Extensions
  , kept
  , eitherReader
  , time
  , keyValues
  ) where

import Freckle.App.Prelude

import Control.Error.Util (note)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, parseTimeM)
import Env hiding (flag)
import qualified Env
import Env.Internal.Free (hoistAlt)
import Env.Internal.Parser (Parser(..), VarF(..))

-- | Designates the value of a parameter when a flag is not provided.
newtype Off a = Off a

-- | Designates the value of a parameter when a flag is provided.
newtype On a = On a

-- | Parse a simple flag
--
-- If the variable is present and non-empty in the environment, the active value
-- is returned, otherwise the default is used.
--
-- >>> import Blammo.Logging (LogLevel(..))
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
--
flag :: Off a -> On a -> String -> Mod Flag a -> Parser Error a
flag (Off f) (On t) n m = Env.flag f t n m

-- | Modify a 'Parser' so all variables are as if they used 'keep'
--
-- By default, read variables are removed from the environment. This is often
-- problematic (e.g. in tests that repeatedly load an app and re-read the
-- environment), and the security benefits are minor.
--
kept :: Parser e a -> Parser e a
kept = Parser . hoistAlt (\v -> v { varfKeep = True }) . unParser

-- | Create a 'Reader' from a simple parser function
--
-- This is a building-block for other 'Reader's
--
eitherReader :: (String -> Either String a) -> Reader Error a
eitherReader f s = first (unread . suffix) $ f s
  where suffix x = x <> ": " <> show s

-- | Read a time value using the given format
--
-- >>> var (time "%Y-%m-%d") "TIME" mempty `parsePure` [("TIME", "1985-02-12")]
-- Right 1985-02-12 00:00:00 UTC
--
-- >>> var (time "%Y-%m-%d") "TIME" mempty `parsePure` [("TIME", "10:00PM")]
-- Left [("TIME",UnreadError "unable to parse time as %Y-%m-%d: \"10:00PM\"")]
--
time :: String -> Reader Error UTCTime
time fmt =
  eitherReader
    $ note ("unable to parse time as " <> fmt)
    . parseTimeM True defaultTimeLocale fmt

-- | Read key-value pairs
--
-- >>> var keyValues "TAGS" mempty `parsePure` [("TAGS", "foo:bar,baz:bat")]
-- Right [("foo","bar"),("baz","bat")]
--
-- Value-less keys are not supported:
--
-- >>> var keyValues "TAGS" mempty `parsePure` [("TAGS", "foo,baz:bat")]
-- Left [("TAGS",UnreadError "Key foo has no value: \"foo,baz:bat\"")]
--
-- Nor are key-less values:
--
-- >>> var keyValues "TAGS" mempty `parsePure` [("TAGS", "foo:bar,:bat")]
-- Left [("TAGS",UnreadError "Value bat has no key: \"foo:bar,:bat\"")]
--
keyValues :: Reader Error [(Text, Text)]
keyValues = eitherReader $ traverse keyValue . T.splitOn "," . pack
 where
  keyValue :: Text -> Either String (Text, Text)
  keyValue t = case second (T.drop 1) $ T.breakOn ":" t of
    (k, v) | T.null v -> Left $ "Key " <> unpack k <> " has no value"
    (k, v) | T.null k -> Left $ "Value " <> unpack v <> " has no key"
    (k, v) -> Right (k, v)
