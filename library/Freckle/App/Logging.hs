{-# LANGUAGE TupleSections #-}

module Freckle.App.Logging
  (
  -- * Logging settings
    HasLogging(..)
  , getLogDefaultANSI
  , getLogBehaviors
  , LogLevel
  , LogFormat(..)
  , LogLocation(..)

  -- ** Loading
  , parseEnvLogFormat
  , parseEnvLogLevel
  , parseEnvLogLocation

  -- * 'MonadLogger'-style running
  , runAppLoggerT

  -- * Formats, for use from other Logging libraries
  , formatJsonLogStr
  , formatJsonNoLoc
  , formatJson
  , formatTerminal
  )
where

import Prelude

import Control.Monad.Logger
import Data.Aeson (ToJSON, encode, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Freckle.App.Env as Env
import System.Console.ANSI
  ( Color(Blue, Magenta, Red, Yellow)
  , ColorIntensity(Dull)
  , ConsoleLayer(Foreground)
  , SGR(Reset, SetColor)
  , hSupportsANSI
  , setSGRCode
  )
import System.IO (stderr, stdout)

data LogFormat
  = FormatJSON
  -- ^ Emit @{"level": "{level}", "message": "{message}"}@
  | FormatTerminal
  -- ^ Emit @[{level}] @{message}@, possibly colorized

data LogLocation
  = LogStdout
  | LogStderr
  | LogFile FilePath

-- | Class for getting Logging settings from your @app@ type
class HasLogging a where
    getLogLevel :: a -> LogLevel

    getLogFormat :: a -> LogFormat

    getLogLocation :: a -> LogLocation

-- | Provide a pure decision for colorizing output
--
-- This is useful in a context where actively checking for ANSI terminal support
-- is either not possible or too expensive. Given that we support 'LogFile', and
-- so are unlikely to be redirecting terminal output to a file, it is relatively
-- safe to use this determination.
--
getLogDefaultANSI :: HasLogging a => a -> Bool
getLogDefaultANSI app = case (getLogLocation app, getLogFormat app) of
  (LogStdout, FormatTerminal) -> True
  (LogStdout, FormatJSON) -> False
  (LogStderr, FormatTerminal) -> True
  (LogStderr, FormatJSON) -> False
  (LogFile _, FormatTerminal) -> False
  (LogFile _, FormatJSON) -> False

getLogBehaviors :: HasLogging a => a -> IO (ByteString -> IO (), Bool)
getLogBehaviors app = case getLogLocation app of
  LogStdout -> (BS8.hPutStr stdout, ) <$> hSupportsANSI stdout
  LogStderr -> (BS8.hPutStr stderr, ) <$> hSupportsANSI stderr
  LogFile path -> pure (BS8.appendFile path, False)

parseEnvLogLevel :: Env.Parser LogLevel
parseEnvLogLevel = Env.var parse "LOG_LEVEL" $ Env.def LevelWarn
 where
  parse = Env.eitherReader $ \case
    "warn" -> Right LevelWarn
    "error" -> Right LevelError
    "debug" -> Right LevelDebug
    "info" -> Right LevelInfo
    level -> Left $ "unexpected log level: " <> level

parseEnvLogFormat :: Env.Parser LogFormat
parseEnvLogFormat = Env.var parse "LOG_FORMAT" $ Env.def FormatTerminal
 where
  parse = Env.eitherReader $ \case
    "json" -> Right FormatJSON
    "terminal" -> Right FormatTerminal
    format -> Left $ "unexpected format: " <> format

parseEnvLogLocation :: Env.Parser LogLocation
parseEnvLogLocation = Env.var parse "LOG_LOCATION" $ Env.def LogStdout
 where
  parse = Env.eitherReader $ \case
    "stdout" -> Right LogStdout
    "stderr" -> Right LogStderr
    "file" -> Right $ LogFile "fancy.log"
    file -> Right $ LogFile file

runAppLoggerT :: HasLogging a => a -> LoggingT IO b -> IO b
runAppLoggerT app f = do
  (putLogLine, isANSI) <- getLogBehaviors app

  let
    logger = case getLogFormat app of
      FormatJSON -> jsonLogger putLogLine
      FormatTerminal -> ansiLogger putLogLine isANSI

  flip runLoggingT logger
    $ filterLogger (\_ level -> level >= getLogLevel app) f
 where
  jsonLogger putLogLine loc src level str =
    putLogLine $ formatJsonLogStr loc src level str

  ansiLogger putLogLine isANSI loc src level str =
    putLogLine $ formatTerminal isANSI loc src level str

formatJsonLogStr :: Loc -> LogSource -> LogLevel -> LogStr -> ByteString
formatJsonLogStr loc src level =
  formatJson (Just loc) (Just src) level . decodeUtf8 . fromLogStr

formatJsonNoLoc :: ToJSON a => LogLevel -> a -> ByteString
formatJsonNoLoc = formatJson Nothing Nothing

formatJson
  :: ToJSON a => Maybe Loc -> Maybe LogSource -> LogLevel -> a -> ByteString
formatJson loc src level msg = (<> "\n") $ BSL.toStrict $ encode $ object
  [ "loc" .= (locJson <$> loc)
  , "src" .= src
  , "level" .= levelText level
  , "message" .= msg
  ]
 where
  locJson Loc {..} = object
    [ "filename" .= loc_filename
    , "package" .= loc_package
    , "module" .= loc_module
    , "start" .= loc_start
    , "end" .= loc_end
    ]

formatTerminal
  :: ToLogStr a
  => Bool -- ^ Supports escapes?
  -> Loc
  -> LogSource
  -> LogLevel
  -> a
  -> ByteString
formatTerminal isANSI loc src level str = mconcat
  [ esc $ style level
  , BS.snoc levelStr labelEnd
  , esc Reset
  , BS.intercalate (BS.singleton labelEnd) logStr
  , esc Reset
  ]
 where
  labelEnd = fromIntegral $ fromEnum ']'

  (levelStr : logStr) =
    BS.split labelEnd . fromLogStr $ defaultLogStr loc src level $ toLogStr str

  esc x = if isANSI then BS8.pack $ setSGRCode [x] else ""

style :: LogLevel -> SGR
style = \case
  LevelDebug -> SetColor Foreground Dull Magenta
  LevelInfo -> SetColor Foreground Dull Blue
  LevelWarn -> SetColor Foreground Dull Yellow
  LevelError -> SetColor Foreground Dull Red
  LevelOther _ -> Reset

levelText :: LogLevel -> Text
levelText = \case
  LevelDebug -> "Debug"
  LevelInfo -> "Info"
  LevelWarn -> "Warn"
  LevelError -> "Error"
  LevelOther x -> x
