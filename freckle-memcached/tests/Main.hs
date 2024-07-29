module Main
  ( main
  ) where

import Prelude

import Spec qualified
import Test.Hspec

main :: IO ()
main = hspec Spec.spec
