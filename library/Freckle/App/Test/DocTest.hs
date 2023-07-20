{-# LANGUAGE PackageImports #-}

module Freckle.App.Test.DocTest
  ( doctest
  , doctestWith

    -- * Lower-level, for site-specific use
  , findPackageFlags
  , findDocTestedFiles
  ) where

import Freckle.App.Prelude

import Control.Monad (filterM)
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Yaml (decodeFileThrow)
import qualified Test.DocTest as DocTest
import "Glob" System.FilePath.Glob (globDir1)

-- | Run doctest on files in the given directory
doctest :: FilePath -> IO ()
doctest = doctestWith []

-- | Run doctest on files in the given directory and with additional flags
doctestWith :: [String] -> FilePath -> IO ()
doctestWith flags dir = do
  packageFlags <- findPackageFlags
  sourceFiles <- findDocTestedFiles dir
  DocTest.doctest $ packageFlags <> flags <> sourceFiles

-- | Representation of only the information we need in a @package.yaml@
data PackageYaml = PackageYaml
  { defaultExtensions :: [String]
  , name :: String
  }

instance FromJSON PackageYaml where
  parseJSON = withObject "PackageYaml" $
    \o -> PackageYaml <$> o .: "default-extensions" <*> o .: "name"

-- Parse @default-extensions@ and @name& out of @package.yaml@
--
-- NB. This won't find target-specific extensions. If your package does this
-- (consider not, then) add them via the direct argument to @'doctestWith'@.
--
findPackageFlags :: IO [String]
findPackageFlags = do
  PackageYaml {..} <- decodeFileThrow "package.yaml"
  pure $ ("-package " <> name) : map ("-X" <>) defaultExtensions

-- | Find any source files with doctest comments
--
-- Doctest with a lot of files is really slow. Like /really/ slow:
--
-- <https://github.com/sol/doctest/issues/141>
--
-- Also, some suites won't actually work on a lot of our files because of some
-- instance-import-related debt that we don't have the time to clean up at this
-- time:
--
-- <https://freckleinc.slack.com/archives/C459XJBGR/p1519220418000125>
--
-- So we want to only run doctest for files that need it. This function finds
-- such files by /naively/ looking for the @^-- >>>@ pattern.
findDocTestedFiles :: FilePath -> IO [FilePath]
findDocTestedFiles dir = do
  paths <- globDir1 "**/*.hs" dir
  filterM (fmap hasDocTests . T.readFile) paths

hasDocTests :: Text -> Bool
hasDocTests = any ("-- >>>" `T.isInfixOf`) . T.lines
