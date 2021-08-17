-- | Facilities for inferring an application version
--
-- Various inputs are checked: files written during a docker build, git
-- information, or falling back to an unknown version. This is useful for
-- Bugsnag reports, client age comparison, etc.
--
module Freckle.App.Version
  ( AppVersion(..)
  , getAppVersion
  , tryGetAppVersion
  )
where

import Prelude

import Control.Applicative ((<|>))
import Control.Error.Util (hoistEither, note)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Trans.Except
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import UnliftIO.Exception (tryIO)

data AppVersion = AppVersion
  { avName :: Text
  , avCreatedAt :: UTCTime
  }
  deriving stock (Eq, Show)

-- | Attempt to infer an @'AppVersion'@
--
-- - If files exist under @\/app-version@ they ar read, otherwise
-- - If we're in a Git repository commit information is used, otherwise
-- - An /Unknown/ version as of the current time is returned
--
getAppVersion :: MonadUnliftIO m => m AppVersion
getAppVersion =
  either (const getAppVersionUnknown) pure =<< tryGetAppVersion "/app-version"

-- | A more testable version of @'getAppVersion'@
--
-- - Reports what didn't work in @'Left'@
-- - Accepts a parent path, for file-system version information
--
tryGetAppVersion
  :: MonadUnliftIO m => FilePath -> m (Either [String] AppVersion)
tryGetAppVersion parent =
  runExceptT
    $ withExceptT pure (getAppVersionFiles parent)
    <|> withExceptT pure getAppVersionGit

getAppVersionFiles :: MonadIO m => FilePath -> ExceptT String m AppVersion
getAppVersionFiles parent = do
  name <- readFileExceptT $ parent </> "name"
  seconds <- readFileExceptT $ parent </> "created-at"
  hoistEither $ toAppVersion name seconds

getAppVersionGit :: MonadIO m => ExceptT String m AppVersion
getAppVersionGit = do
  name <- git ["rev-parse", "HEAD"]
  seconds <- git ["show", "--no-patch", "--no-notes", "--pretty=%at"]
  hoistEither $ toAppVersion name seconds

toAppVersion :: String -> String -> Either String AppVersion
toAppVersion name seconds = do
  createdAt <- parseUnixSeconds $ strip seconds
  pure AppVersion { avName = T.strip $ pack name, avCreatedAt = createdAt }

parseUnixSeconds :: String -> Either String UTCTime
parseUnixSeconds x = note err $ parseTimeM True defaultTimeLocale "%s" x
  where err = x <> " does not parse as UTCTime with format %s"

getAppVersionUnknown :: MonadIO m => m AppVersion
getAppVersionUnknown = AppVersion "Unknown" <$> liftIO getCurrentTime

readFileExceptT :: MonadIO m => FilePath -> ExceptT String m String
readFileExceptT path = ExceptT $ liftIO $ first err <$> tryIO (readFile path)
  where err ex = "readFile: " <> show ex

git :: MonadIO m => [String] -> ExceptT String m String
git args = do
  (ec, stdout, stderr) <- exceptIO $ readProcessWithExitCode "git" args []

  case ec of
    ExitSuccess -> pure stdout
    ExitFailure n ->
      throwE $ "[" <> show n <> "] git " <> unwords args <> ": " <> stderr

exceptIO :: MonadIO m => IO a -> ExceptT String m a
exceptIO = withExceptT show . ExceptT . liftIO . tryIO

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace
