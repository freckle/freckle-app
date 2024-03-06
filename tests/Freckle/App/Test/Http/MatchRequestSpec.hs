module Freckle.App.Test.Http.MatchRequestSpec
  ( spec
  ) where

import Freckle.App.Prelude

import Freckle.App.Http
  ( Request
  , addAcceptHeader
  , addRequestHeader
  , parseRequest_
  )
import Freckle.App.Test.Http.MatchRequest
import Network.HTTP.Types.Header (hAccept)
import Test.Hspec

spec :: Spec
spec = do
  describe "matchRequestFromUrl" $ do
    context "matching complete requests" $ do
      let
        url = "https://localhost:3000/hello?world"
        mr = matchRequestFromUrl url

      it "matches the same request" $ do
        mr `shouldMatchRequest` parseRequest_ url

      it "matches on method" $ do
        mr `shouldNotMatchRequest` parseRequest_ ("POST " <> url)

      it "matches on scheme" $ do
        mr `shouldNotMatchRequest` parseRequest_ "http://localhost:3000/hello?world"

      it "matches on host" $ do
        mr `shouldNotMatchRequest` parseRequest_ "https://localhost2:3000/hello?world"

      it "matches on port" $ do
        mr `shouldNotMatchRequest` parseRequest_ "https://localhost:3001/hello?world"

      it "matches on path" $ do
        mr `shouldNotMatchRequest` parseRequest_ "https://localhost:3000/world?world"

      it "matches on query" $ do
        mr `shouldNotMatchRequest` parseRequest_ "https://localhost:3000/hello?wut"

    it "can match on everything unspecified" $ do
      let mr = matchRequestFromUrl "https://"

      mr `shouldMatchRequest` parseRequest_ "https://example.com"
      mr `shouldMatchRequest` parseRequest_ "https://example.com/"
      mr `shouldMatchRequest` parseRequest_ "https://example.com/foo"
      mr `shouldMatchRequest` parseRequest_ "https://example.com/bar"

    it "can match any path" $ do
      let mr = matchRequestFromUrl "https://example.com"

      mr `shouldMatchRequest` parseRequest_ "https://example.com"
      mr `shouldMatchRequest` parseRequest_ "https://example.com/"
      mr `shouldMatchRequest` parseRequest_ "https://example.com/foo"
      mr `shouldMatchRequest` parseRequest_ "https://example.com/bar"

    it "can match the root path explicitly" $ do
      let mr = matchRequestFromUrl "https://example.com/"

      mr `shouldMatchRequest` parseRequest_ "https://example.com/"
      mr `shouldMatchRequest` parseRequest_ "https://example.com"
      mr `shouldNotMatchRequest` parseRequest_ "https://example.com/foo"
      mr `shouldNotMatchRequest` parseRequest_ "https://example.com/bar"

    context "matching headers" $ do
      let
        url = "https://example.com/"
        accept = "text/plain"
        hasNoHeaders = parseRequest_ url
        hasOnlyTheHeader = addAcceptHeader accept hasNoHeaders
        hasExtraHeaders = addRequestHeader "User-Agent" "me" hasOnlyTheHeader

      it "can match headers exactly" $ do
        let mr = matchRequestFromUrl url <> MatchHeaders [(hAccept, accept)]

        mr `shouldMatchRequest` hasOnlyTheHeader
        mr `shouldNotMatchRequest` hasExtraHeaders
        mr `shouldNotMatchRequest` hasNoHeaders

      it "can match headers-include" $ do
        let mr = matchRequestFromUrl url <> MatchHeader (hAccept, accept)

        mr `shouldMatchRequest` hasOnlyTheHeader
        mr `shouldMatchRequest` hasExtraHeaders
        mr `shouldNotMatchRequest` hasNoHeaders

shouldMatchRequest :: HasCallStack => MatchRequest -> Request -> IO ()
mr `shouldMatchRequest` req = do
  case matchRequest req mr of
    Left err -> do
      let preamble = unlines ["Expected to match request, but did not", "", show req]
      expectationFailure $ preamble <> err
    Right () -> pure ()

infix 1 `shouldMatchRequest`

shouldNotMatchRequest :: HasCallStack => MatchRequest -> Request -> IO ()
mr `shouldNotMatchRequest` req = do
  case matchRequest req mr of
    Left {} -> pure ()
    Right () -> do
      let preamble = unlines ["Expected to NOT match request, but did", "", show req]
      expectationFailure $ preamble <> showMatchRequest mr

infix 1 `shouldNotMatchRequest`
