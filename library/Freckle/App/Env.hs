-- | Parse the shell environment for configuration
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
-- N.B. Usage is meant to mimic envparse, but the implementation is greatly
-- simplified (at loss of some features) and some bugs have been fixed.
--
-- <http://hackage.haskell.org/package/envparse>
--
module Freckle.App.Env
  (
  -- * Parsing
    Parser
  , Off(..)
  , On(..)
  , parse
  , var
  , flag
  , switch
  , handleEither

  -- * Readers
  , str
  , auto
  , time
  , keyValues
  , eitherReader

  -- * Modifiers
  , def
  , nonEmpty
  ) where

import Freckle.App.Prelude

import Control.Error.Util (note)
import Data.String
import Data.Text (pack, unpack)
import qualified Data.Text as T
import Data.Time
import Freckle.App.Env.Internal
import System.Environment (getEnvironment)
import System.Exit (die)
import Text.Read (readEither)

-- | Designates the value of a parameter when a flag is not provided.
newtype Off a = Off a

-- | Designates the value of a parameter when a flag is provided.
newtype On a = On a

-- $setup
-- >>> :{
--  let
--    exampleParse :: [(String, String)] -> Parser a -> Either [(String, Error)] a
--    exampleParse env = ($ env) . unParser
-- :}

-- | Parse the current environment in @'IO'@
--
-- The process will exit non-zero after printing any errors.
--
parse :: Parser a -> IO a
parse p = do
  env <- getEnvironment
  either (die . prettyErrors) pure $ unParser p env
 where
  prettyErrors = unlines . map (uncurry prettyError)
  prettyError name UnsetError = name <> " must be set"
  prettyError name (InvalidError msg) = name <> " is invalid:\n  " <> msg

-- | Parse a variable by name, using the given Reader and options
--
-- >>> exampleParse @String [("EDITOR", "vim")] $ var str "EDITOR" (def "vi")
-- Right "vim"
--
-- >>> exampleParse @String [] $ var str "EDITOR" (def "vi")
-- Right "vi"
--
-- Parsers are instances of @'Alternative'@, which means you can use combinators
-- like @'optional'@ or @'<|>'@.
--
-- >>> import Control.Applicative
--
-- >>> exampleParse @(Maybe String) [] $ optional $ var str "EDITOR" nonEmpty
-- Right Nothing
--
-- The above will no longer fail if the environment variable is missing, but it
-- will still validate it if it is present:
--
-- >>> exampleParse @(Maybe String) [("EDITOR", "")] $ optional $ var str "EDITOR" nonEmpty
-- Left [("EDITOR",InvalidError "value cannot be empty")]
--
-- >>> exampleParse @(Maybe String) [("EDITOR", "vim")] $ optional $ var str "EDITOR" nonEmpty
-- Right (Just "vim")
--
-- >>> let p = var str "VISUAL" nonEmpty <|> var str "EDITOR" nonEmpty <|> pure "vi"
-- >>> exampleParse @String [("VISUAL", "vim"), ("EDITOR", "ed")] p
-- Right "vim"
--
-- >>> exampleParse @String [("EDITOR", "ed")] p
-- Right "ed"
--
-- >>> exampleParse @String [] p
-- Right "vi"
--
-- Again, values that /are/ present are still validated:
--
-- >>> exampleParse @String [("VISUAL", ""), ("EDITOR", "ed")] p
-- Left [("VISUAL",InvalidError "value cannot be empty")]
--
var :: Reader a -> String -> Mod a -> Parser a
var r n (Mod m) =
  varParser $ m Var { varName = n, varReader = r, varDefault = Nothing }

-- | Parse a simple flag
--
-- If the variable is present and non-empty in the environment, the active value
-- is returned, otherwise the default is used.
--
-- >>> import Control.Monad.Logger
--
-- >>> exampleParse [("DEBUG", "1")] $ flag (Off LevelInfo) (On LevelDebug) "DEBUG" mempty
-- Right LevelDebug
--
-- >>> exampleParse [("DEBUG", "")] $ flag (Off LevelInfo) (On LevelDebug) "DEBUG" mempty
-- Right LevelInfo
--
-- >>> exampleParse [] $ flag (Off LevelInfo) (On LevelDebug) "DEBUG" mempty
-- Right LevelInfo
--
-- N.B. only the empty string is falsey:
--
-- >>> exampleParse [("DEBUG", "false")] $ flag (Off LevelInfo) (On LevelDebug) "DEBUG" mempty
-- Right LevelDebug
--
-- >>> exampleParse [("DEBUG", "no")] $ flag (Off LevelInfo) (On LevelDebug) "DEBUG" mempty
-- Right LevelDebug
--
flag :: Off a -> On a -> String -> Mod a -> Parser a
flag (Off f) (On t) n (Mod m) = varParser $ m Var
  { varName = n
  , varReader = Reader $ \case
    "" -> Right f
    _ -> Right t
  , varDefault = Just f
  }

-- | A simplified version of @'flag'@ for @'Bool'@ values
--
-- >>> exampleParse [("VERBOSE", "1")] $ switch "VERBOSE" mempty
-- Right True
--
-- >>> exampleParse [] $ switch "VERBOSE" mempty
-- Right False
--
switch :: String -> Mod Bool -> Parser Bool
switch = flag (Off False) (On True)

-- | Create a @'Reader'@ from a simple parser function
--
-- This is a building-block for other @'Reader'@s
--
eitherReader :: (String -> Either String a) -> Reader a
eitherReader f =
  Reader $ \s -> first (InvalidError . (<> (": \"" <> s <> "\""))) $ f s

-- | Use a value's @'Read'@ instance
--
-- >>> import Numeric.Natural
--
-- >>> exampleParse @Natural [("SIZE", "1")] $ var auto "SIZE" mempty
-- Right 1
--
-- >>> exampleParse @Natural [("SIZE", "-1")] $ var auto "SIZE" mempty
-- Left [("SIZE",InvalidError "Prelude.read: no parse: \"-1\"")]
--
auto :: Read a => Reader a
auto = eitherReader readEither

-- | Read a time value using the given format
--
-- >>> exampleParse [("TIME", "1985-02-12")] $ var (time "%Y-%m-%d") "TIME" mempty
-- Right 1985-02-12 00:00:00 UTC
--
-- >>> exampleParse [("TIME", "10:00PM")] $ var (time "%Y-%m-%d") "TIME" mempty
-- Left [("TIME",InvalidError "unable to parse time as %Y-%m-%d: \"10:00PM\"")]
--
time :: String -> Reader UTCTime
time fmt =
  eitherReader
    $ note ("unable to parse time as " <> fmt)
    . parseTimeM True defaultTimeLocale fmt

-- | Read key-value pairs
--
-- >>> exampleParse [("TAGS", "foo:bar,baz:bat")] $ var keyValues "TAGS" mempty
-- Right [("foo","bar"),("baz","bat")]
--
-- Value-less keys are not supported:
--
-- >>> exampleParse [("TAGS", "foo,baz:bat")] $ var keyValues "TAGS" mempty
-- Left [("TAGS",InvalidError "Key foo has no value: \"foo,baz:bat\"")]
--
-- Nor are key-less values:
--
-- >>> exampleParse [("TAGS", "foo:bar,:bat")] $ var keyValues "TAGS" mempty
-- Left [("TAGS",InvalidError "Value bat has no key: \"foo:bar,:bat\"")]
--
keyValues :: Reader [(Text, Text)]
keyValues = eitherReader $ traverse keyValue . T.splitOn "," . pack
 where
  keyValue :: Text -> Either String (Text, Text)
  keyValue t = case second (T.drop 1) $ T.breakOn ":" t of
    (k, v) | T.null v -> Left $ "Key " <> unpack k <> " has no value"
    (k, v) | T.null k -> Left $ "Value " <> unpack v <> " has no key"
    (k, v) -> Right (k, v)

-- | Use a value's @'IsString'@ instance
--
-- >>> import Data.Text (Text)
--
-- >>> exampleParse @Text [("FOO", "foo")] $ var str "FOO" mempty
-- Right "foo"
--
-- Take note: if this fails, it's basically @'error'@.
--
str :: IsString a => Reader a
str = Reader $ pure . fromString

-- | Modify parsing to fail on empty strings
--
-- >>> exampleParse @String [("FOO", "")] $ var str "FOO" nonEmpty
-- Left [("FOO",InvalidError "value cannot be empty")]
--
nonEmpty :: Mod a
nonEmpty = Mod $ \v -> v
  { varReader = Reader $ \case
    [] -> Left $ InvalidError "value cannot be empty"
    xs -> unReader (varReader v) xs
  }

-- | Declare a default value for the parser
def :: a -> Mod a
def d = Mod $ \v -> v { varDefault = Just d }

-- | Handle parsers that may fail
--
-- Handling @'Either'@ parser results causes short circuiting in the parser
-- results.
--
-- >>> exampleParse @String [("FOO", "")] $ handleEither "CONTEXT" $ pure $ Left "failed"
-- Left [("CONTEXT",InvalidError "failed")]
--
-- >>> exampleParse @String [("FOO", "")] $ handleEither "CONTEXT" $ pure $ Right "stuff"
-- Right "stuff"
--
handleEither
  :: String -- ^ Parser context reported on error
  -> Parser (Either String a)
  -> Parser a
handleEither context p = bindParser p $ \case
  Left err -> Parser $ \_ -> Left [(context, InvalidError err)]
  Right x -> pure x
