module FrontRow.App.HttpSpec
  ( spec
  ) where

import Prelude

import Control.Lens ((^?))
import Data.Aeson
import Data.Aeson.Lens
import FrontRow.App.Http
import Network.HTTP.Types.Status (status200)
import Test.Hspec

spec :: Spec
spec = do
  describe "httpJson" $ do
    it "fetches JSON via HTTP" $ do
      resp <- httpJson @_ @Value
        $ parseRequest_ "https://www.stackage.org/lts-17.10"

      getResponseStatus resp `shouldBe` status200
      body <- getResponseBodyUnsafe resp
      body ^? key "snapshot" . key "ghc" . _String `shouldBe` Just "8.10.4"
