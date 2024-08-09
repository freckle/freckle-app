module Freckle.App.Test.Hspec.Runner
  ( run
  , runParConfig
  , runWith
  , makeParallelConfig
  ) where

import Freckle.App.Prelude

import Control.Concurrent (getNumCapabilities, setNumCapabilities)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.List (isInfixOf)
import System.Environment (getArgs, lookupEnv)
import Test.Hspec (Spec, runIO)
import Test.Hspec.JUnit.Config
  ( setJUnitConfigOutputFile
  , setJUnitConfigSourcePathPrefix
  )
import Test.Hspec.JUnit.Config.Env (envJUnitConfig)
import Test.Hspec.JUnit.Formatter qualified as JUnit (add)
import Test.Hspec.JUnit.Formatter.Env qualified as JUnit (whenEnabled)
import Test.Hspec.Runner
  ( Config
  , Path
  , configConcurrentJobs
  , configSkipPredicate
  , defaultConfig
  , evaluateSummary
  , hspecWithResult
  , readConfig
  )
import Prelude qualified as Unsafe (read)

run :: String -> Spec -> IO ()
run = runWith defaultConfig

runParConfig :: String -> Spec -> IO ()
runParConfig name spec = do
  config <- makeParallelConfig defaultConfig
  runWith config name spec

runWith :: Config -> String -> Spec -> IO ()
runWith config name spec = do
  args <- getArgs

  -- Run unreliable tests first, so local dev errors are reported for reliable
  -- specs at the end
  putStrLn "Running UNRELIABLE tests; failures here should not fail the build"
  void $
    runner ("unreliable-" <> name) id
      =<< load
        args
        (skip reliableTests config)

  putStrLn "Running RELIABLE"
  reliableSummary <-
    runner name id
      =<< load args (skip (anys [unreliableTests, isolatedTests]) config)

  putStrLn "Running ISOLATED"
  isolatedSummary <-
    runner ("isolated-" <> name) noConcurrency
      =<< load args (skip (not . isolatedTests) config)

  evaluateSummary $ reliableSummary <> isolatedSummary
 where
  load = flip readConfig
  runner filename changeConfig =
    runSpec (addJUnit "/tmp/junit" filename spec) . changeConfig
  noConcurrency x = x {configConcurrentJobs = Just 1}
  runSpec = flip hspecWithResult

addJUnit :: FilePath -> String -> Spec -> Spec
addJUnit path name spec = do
  junitConfig <- runIO $ withOverride <$> envJUnitConfig
  JUnit.whenEnabled (JUnit.add junitConfig) spec
 where
  filePath = path <> "/" <> name <> "/test_results.xml"
  withOverride =
    setJUnitConfigSourcePathPrefix name . setJUnitConfigOutputFile filePath

makeParallelConfig :: Config -> IO Config
makeParallelConfig config = do
  jobCores <-
    fromMaybe 1
      <$> runMaybeT
        (MaybeT lookupTestCapabilities <|> MaybeT lookupHostCapabilities)
  putStrLn $ "Running spec with " <> show jobCores <> " cores"
  setNumCapabilities jobCores
  -- Api specs are IO bound, having more jobs than cores allows for more
  -- cooperative IO from green thread interleaving.
  pure config {configConcurrentJobs = Just $ jobCores * 4}

lookupTestCapabilities :: IO (Maybe Int)
lookupTestCapabilities = fmap Unsafe.read <$> lookupEnv "TEST_CAPABILITIES"

lookupHostCapabilities :: IO (Maybe Int)
lookupHostCapabilities = Just . reduceCapabilities <$> getNumCapabilities

-- Reduce capabilities to avoid contention with postgres
reduceCapabilities :: Int -> Int
reduceCapabilities = max 1 . (`div` 2)

skip :: (Path -> Bool) -> Config -> Config
skip predicate config = config {configSkipPredicate = Just predicate}

unreliableTests :: Path -> Bool
unreliableTests = ("UNRELIABLE" `isInfixOf`) . snd

reliableTests :: Path -> Bool
reliableTests = not . unreliableTests

isolatedTests :: Path -> Bool
isolatedTests = ("ISOLATED" `isInfixOf`) . snd

anys :: [a -> Bool] -> a -> Bool
anys xs a = any ($ a) xs
