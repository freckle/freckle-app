module Main
  ( main
  )
where

import Prelude

import FrontRow.App.Test.DocTest

main :: IO ()
main = doctest "library/"
