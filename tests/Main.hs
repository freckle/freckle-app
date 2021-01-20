module Main
  ( main
  )
where

import Prelude

import FrontRow.App.Test.Hspec.Runner (runParConfig)
import qualified Spec
import Test.Hspec

main :: IO ()
main = "frontrow-app" `runParConfig` parallel Spec.spec
