module Freckle.App.WaiSpec
  ( spec
  )
where

import Prelude

import Data.ByteString (ByteString)
import Data.Function (on)
import Data.List (deleteBy)
import Freckle.App.Wai (corsMiddleware, noCacheMiddleware)
import Network.HTTP.Types.Method (Method)
import Network.HTTP.Types.Status (status200)
import Network.Wai
import Network.Wai.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "noCacheMiddleware" $ do
    let
      runTestSession :: Session a -> IO a
      runTestSession f = runSession f $ noCacheMiddleware app

    it "adds an appropriate Cache-Control header" $ runTestSession $ do
      response <- request defaultRequest

      assertHeader
        "Cache-Control"
        "no-cache, no-store, max-age=0, private"
        response
      assertBody "Test" response

  describe "corsMiddleware" $ do
    let
      runTestSession :: Session a -> IO a
      runTestSession = runTestSessionWith (const False) []

      runTestSessionWith
        :: (ByteString -> Bool) -> [ByteString] -> Session a -> IO a
      runTestSessionWith validateOrigin extraExposedHeaders f =
        runSession f $ corsMiddleware validateOrigin extraExposedHeaders app

    it "adds CORS headers to responses for non-OPTIONS" $ runTestSession $ do
      response <- request $ setMethod "GET" $ setOriginHeader
        "unimportant"
        defaultRequest

      assertAccessControlHeaders "BADORIGIN" response
      assertBody "Test" response

    it "responds itself, with CORS headers, for OPTIONS" $ runTestSession $ do
      response <- request $ setMethod "OPTIONS" $ setOriginHeader
        "unimportant"
        defaultRequest

      assertAccessControlHeaders "BADORIGIN" response
      assertBody mempty response

    it "doesn't operate on requests without Origin" $ runTestSession $ do
      response1 <- request defaultRequest
      response2 <- request $ setMethod "OPTIONS" defaultRequest

      assertBody "Test" response1
      assertNoAccessControlHeaders response1
      assertBody "Test" response2
      assertNoAccessControlHeaders response2

    it "accepts only valid Origins" $ runTestSessionWith (== "A") [] $ do
      responseA <- request $ setOriginHeader "A" defaultRequest
      responseB <- request $ setOriginHeader "B" defaultRequest

      assertAccessControlHeaders "A" responseA
      assertAccessControlHeaders "BADORIGIN" responseB
      assertBody "Test" responseA
      assertBody "Test" responseB

    it "adds extra Exposed-Headers"
      $ runTestSessionWith (const False) ["X-Foo"]
      $ do
          response <- request $ setOriginHeader "unimportant" defaultRequest
          assertHeader
            "Access-Control-Expose-Headers"
            "Set-Cookie, Content-Disposition, Link, X-Foo"
            response

app :: Application
app _req respond = respond $ responseLBS status200 [] "Test"

setMethod :: Method -> Request -> Request
setMethod method req = req { requestMethod = method }

setOriginHeader :: ByteString -> Request -> Request
setOriginHeader origin req =
  let
    header = ("Origin", origin)
    others = deleteBy ((==) `on` fst) header $ requestHeaders req
  in req { requestHeaders = others <> [header] }

assertAccessControlHeaders :: ByteString -> SResponse -> Session ()
assertAccessControlHeaders origin response = do
  assertHeader "Access-Control-Allow-Origin" origin response
  assertHeader
    "Access-Control-Allow-Methods"
    "POST, GET, OPTIONS, PUT, DELETE, PATCH"
    response
  assertHeader "Access-Control-Allow-Credentials" "true" response
  assertHeader "Access-Control-Allow-Headers" "Content-Type, *" response
  assertHeader
    "Access-Control-Expose-Headers"
    "Set-Cookie, Content-Disposition, Link"
    response

assertNoAccessControlHeaders :: SResponse -> Session ()
assertNoAccessControlHeaders response = do
  assertNoHeader "Access-Control-Allow-Origin" response
  assertNoHeader "Access-Control-Allow-Methods" response
  assertNoHeader "Access-Control-Allow-Credentials" response
  assertNoHeader "Access-Control-Allow-Headers" response
  assertNoHeader "Access-Control-Expose-Headers" response
