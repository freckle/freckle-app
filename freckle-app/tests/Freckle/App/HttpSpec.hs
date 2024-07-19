module Freckle.App.HttpSpec
  ( spec
  ) where

import Freckle.App.Prelude

import Control.Lens (to, (^?), _Left, _Right)
import Data.Aeson
import Data.Aeson.Lens
import Data.List.NonEmpty qualified as NE
import Freckle.App.Http
import Freckle.App.Test.Http
import Network.HTTP.Types.Status (status200)
import Test.Hspec

spec :: Spec
spec = do
  describe "httpJson" $ do
    stubs <- runIO $ loadHttpStubsDirectory "tests/files"

    it "fetches JSON via HTTP" $ do
      resp <-
        flip runHttpStubsT stubs
          . httpJson @_ @Value
          $ parseRequest_ "https://www.stackage.org/lts-17.10"

      getResponseStatus resp `shouldBe` status200
      getResponseBody resp
        ^? _Right
          . key "snapshot"
          . key "ghc"
          . _String
        `shouldBe` Just "8.10.4"

    it "places JSON parse errors in a Left body" $ do
      resp <-
        flip runHttpStubsT stubs
          . httpJson @_ @[()]
          $ parseRequest_ "https://www.stackage.org/lts-17.10"

      let expectedErrorMessages =
            [ "Error in $: expected [a], encountered Object"
            , "Error in $: parsing [] failed, expected Array, but encountered Object"
            ]

      getResponseStatus resp `shouldBe` status200
      getResponseBody resp
        ^? _Left
          . to hdeErrors
          . to NE.head
        `shouldSatisfy` maybe False (`elem` expectedErrorMessages)
