module Main
  ( main
  )
where

import Prelude

import Freckle.App.Test.DocTest

main :: IO ()
main = doctest "library/"
