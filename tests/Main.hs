module Main
  ( main
  )
where

import Prelude

import Freckle.App.Test.Hspec.Runner (runParConfig)
import qualified Spec
import Test.Hspec

main :: IO ()
main = "freckle-app" `runParConfig` parallel Spec.spec
