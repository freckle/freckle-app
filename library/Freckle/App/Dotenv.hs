-- | Conventional @.env@ handling
module Freckle.App.Dotenv
  ( load
  , loadTest
  , loadFile
  ) where

import Freckle.App.Prelude

import qualified Configuration.Dotenv as Dotenv
import System.FilePath (takeDirectory, (</>))
import UnliftIO.Directory (doesFileExist, getCurrentDirectory)

-- | Call 'loadFile' with @.env@
load :: IO ()
load = loadFile ".env"

-- | Call 'loadFile' with @.env.test@
loadTest :: IO ()
loadTest = loadFile ".env.test"

-- | An opinionated 'Configuration.Dotenv.loadFile'
--
-- Additional behaviors:
--
-- 1. Attempt to locate the file in parent directories too
--
--    We, sadly, have a monorepository. So we need to locate a @.env@ file in
--    parent directories when running tests in sub-directories.
--
-- 2. Silently ignore no file found
--
--    Since this is used by 'Freckle.App.Test.withApp', which we aim to use in
--    every non-trivial project, we can't fail in projects that don't need or
--    have a @.env(.test)@ file (such as this one!).
--
-- 3. Use the @.env.example@ feature, but only if one exists alongside
loadFile :: FilePath -> IO ()
loadFile = traverse_ go <=< locateInParents
 where
  go path = do
    let examplePath = takeDirectory path </> ".env.example"
    exampleExists <- doesFileExist examplePath

    void $
      Dotenv.loadFile $
        Dotenv.defaultConfig
          { Dotenv.configPath = [path]
          , Dotenv.configExamplePath = [examplePath | exampleExists]
          }

locateInParents :: FilePath -> IO (Maybe FilePath)
locateInParents path = go =<< getCurrentDirectory
 where
  go cwd = do
    let absPath = cwd </> path

    exists <- doesFileExist absPath

    if exists
      then pure $ Just absPath
      else if cwd == "/" then pure Nothing else go $ takeDirectory cwd
