module FrontRow.App.HttpSpec
  ( spec
  ) where

import Prelude

import Control.Lens (_Left, _Right, to, (^?))
import Data.Aeson
import Data.Aeson.Lens
import Data.Either (isLeft)
import qualified Data.List.NonEmpty as NE
import FrontRow.App.Http
import Network.HTTP.Types.Status (status200, status404)
import Test.Hspec

spec :: Spec
spec = do
  describe "httpJson" $ do
    it "fetches JSON via HTTP" $ do
      resp <- httpJson @_ @Value
        $ parseRequest_ "https://www.stackage.org/lts-17.10"

      getResponseStatus resp `shouldBe` status200
      getResponseBody resp
        ^? _Right
        . key "snapshot"
        . key "ghc"
        . _String
        `shouldBe` Just "8.10.4"

    it "places JSON parse errors in a Left body" $ do
      resp <- httpJson @_ @[()]
        $ parseRequest_ "https://www.stackage.org/lts-17.10"

      let
        expectedErrorMessage
          = "Error in $: parsing [] failed, expected Array, but encountered Object"

      getResponseStatus resp `shouldBe` status200
      getResponseBody resp
        ^? _Left
        . to hdeErrors
        . to NE.toList
        `shouldBe` Just [expectedErrorMessage]

  describe "getResponseBodyUnsafe" $ do
    it "throws HttpStatusError for non-2XX" $ do
      resp <- httpJson @_ @Value
        $ parseRequest_ "https://example.com/i/dont/exist"

      let
        expectedStatusError :: Selector (HttpStatusError HttpDecodeError Value)
        expectedStatusError HttpStatusError {..} =
          hseStatus == status404 && isLeft (getResponseBody hseResponse)

      getResponseStatus resp `shouldBe` status404
      getResponseBodyUnsafe resp `shouldThrow` expectedStatusError
