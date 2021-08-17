module Freckle.App.VersionSpec
  ( spec
  )
where

import Prelude

import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Freckle.App.Version
import System.Directory (withCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess, readProcess)
import Test.Hspec

spec :: Spec
spec = do
  describe "getAppVersion" $ do
    -- NB. This is a single test case because the temporary directory and git
    -- stuff steps all over itself when run in parallel.
    --
    -- This could be a bug in withSystemTempDirectory or withCurrentDirectory?
    --
    it "tries files, then git, and records failed attempts" $ do
      inTemporaryDirectory $ \tmp -> do
        eVersion1 <- tryGetAppVersion tmp
        first (map $ unwords . take 6 . words) eVersion1 `shouldBe` Left
          [ "readFile: " <> tmp <> "/name: openFile: does not exist"
          , "[128] git rev-parse HEAD: fatal: not"
          ]

        callProcess "git" ["init", "--quiet"]
        callProcess "git" ["commit", "--quiet", "--allow-empty", "-m", "x"]
        sha <- readProcess "git" ["rev-parse", "HEAD"] ""

        eVersion2 <- tryGetAppVersion tmp
        fmap (unpack . (<> "\n") . avName) eVersion2 `shouldBe` Right sha

        let seconds = "1582301740"
        writeFile "name" "a version"
        writeFile "created-at" seconds

        eVersion3 <- tryGetAppVersion tmp
        eVersion3 `shouldBe` Right AppVersion
          { avName = "a version"
          , avCreatedAt = parseTimeUnsafe "%s" seconds
          }

inTemporaryDirectory :: (FilePath -> IO a) -> IO a
inTemporaryDirectory f = withSystemTempDirectory "freckle-app-tests"
  $ \tmp -> withCurrentDirectory tmp $ f tmp

parseTimeUnsafe :: String -> String -> UTCTime
parseTimeUnsafe fmt str = fromMaybe err
  $ parseTimeM True defaultTimeLocale fmt str
  where err = error $ str <> " did not parse as UTCTime format " <> fmt
