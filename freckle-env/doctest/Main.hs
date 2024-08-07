module Main (main) where

import Prelude

import Test.DocTest

main :: IO ()
main =
  doctest
    [ "library/"
    , "-XGHC2021"
    , "-XDerivingStrategies"
    , "-XLambdaCase"
    , "-XNoImplicitPrelude"
    , "-XNoMonomorphismRestriction"
    ]
