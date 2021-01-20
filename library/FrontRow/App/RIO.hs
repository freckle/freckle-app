-- | Compatibility between "FrontRow.App" and "RIO"
--
-- "FrontRow.App" was created before "RIO" existed. We need to decide if we're
-- going to move to using "RIO" without "FrontRow.App" (and port the things
-- we've added to be "RIO"-based), or not.
--
-- As part of that decisions, some Apps are using "RIO". These should still be
-- able to make use of "FrontRow.App", by using this module.
--
module FrontRow.App.RIO
  ( toAppLogLevel
  , fromAppLogLevel
  , makeLogFunc
  )
where

import Prelude

import Control.Monad (when)
import Control.Monad.Logger (Loc(..), LogLevel(..))
import Data.Maybe (fromMaybe, listToMaybe)
import FrontRow.App.Logging
import GHC.Exception (CallStack, SrcLoc(..), getCallStack)
import qualified RIO

toAppLogLevel :: RIO.LogLevel -> LogLevel
toAppLogLevel = \case
  RIO.LevelDebug -> LevelDebug
  RIO.LevelInfo -> LevelInfo
  RIO.LevelWarn -> LevelWarn
  RIO.LevelError -> LevelError
  RIO.LevelOther x -> LevelOther x

fromAppLogLevel :: LogLevel -> RIO.LogLevel
fromAppLogLevel = \case
  LevelDebug -> RIO.LevelDebug
  LevelInfo -> RIO.LevelInfo
  LevelWarn -> RIO.LevelWarn
  LevelError -> RIO.LevelError
  LevelOther x -> RIO.LevelOther x

makeLogFunc :: HasLogging a => a -> IO RIO.LogFunc
makeLogFunc app = do
  (putLogLine, isANSI) <- getLogBehaviors app

  pure $ RIO.mkLogFunc $ \cs src rioLevel builder -> do
    let
      level = toAppLogLevel rioLevel
      msg = RIO.utf8BuilderToText builder

    when (level >= getLogLevel app) $ putLogLine $ case getLogFormat app of
      FormatJSON -> formatJson (Just $ callStackToLoc cs) (Just src) level msg
      FormatTerminal -> formatTerminal isANSI (callStackToLoc cs) src level msg

callStackToLoc :: CallStack -> Loc
callStackToLoc cs = fromMaybe unknownLoc $ do
  (_, SrcLoc {..}) <- listToMaybe $ getCallStack cs

  pure $ Loc
    { loc_filename = srcLocFile
    , loc_package = srcLocPackage
    , loc_module = srcLocModule
    , loc_start = (srcLocStartLine, srcLocStartCol)
    , loc_end = (srcLocEndLine, srcLocEndCol)
    }

unknownLoc :: Loc
unknownLoc = Loc
  { loc_filename = "<unknown>"
  , loc_package = "<unknown>"
  , loc_module = "unknown"
  , loc_start = (0, 0)
  , loc_end = (0, 0)
  }
