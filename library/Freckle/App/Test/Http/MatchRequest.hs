-- | 'Request' predicates for matching 'HttpStub's
--
-- == Usage
--
-- @
-- stubs :: ['HttpStub']
-- stubs =
--   [ \"https://example.com\"
--       & 'matchL' <>~ 'MatchMethod' \"POST\"
--       & 'matchL' <>~ 'MatchHeaders' [(hAccept, \"text/plain+csv\")]
--       & 'matchL' <>~ 'MatchBody' \"id,name\n42,Pat\n\"
--       & 'statusL' .~ 'status201'
--       & 'bodyL' .~ \"OK\n\"
--   ]
-- @
module Freckle.App.Test.Http.MatchRequest
  ( MatchRequest (..)
  , matchRequestFromUrl
  , matchRequest
  , showMatchRequest
  , showMatchRequestWithMismatches
  ) where

import Freckle.App.Prelude

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Client (Request, RequestBody (..), parseRequest_)
import qualified Network.HTTP.Client.Internal as HTTP
import Network.HTTP.Types.Header (Header, RequestHeaders)
import Network.HTTP.Types.Method (Method)

data MatchRequest
  = MatchAnything
  | MatchAnd MatchRequest MatchRequest
  | MatchMethod Method
  | MatchSecure Bool
  | MatchHost ByteString
  | MatchPort Int
  | MatchPath ByteString
  | MatchQuery ByteString
  | MatchHeaders RequestHeaders
  | MatchHeader Header
  | MatchBody ByteString
  deriving stock (Show)

instance Semigroup MatchRequest where
  a <> b = MatchAnd a b

matchRequestFromUrl :: String -> MatchRequest
matchRequestFromUrl url =
  fold1 $ maybe id (<>) optionalMatches requiredMatches
 where
  req = parseRequest_ url

  method = HTTP.method req
  secure = HTTP.secure req
  host = HTTP.host req
  port = HTTP.port req
  path = HTTP.path req
  query = HTTP.queryString req
  headers = HTTP.requestHeaders req
  body = simplifyRequestBody req

  requiredMatches = MatchMethod method :| [MatchSecure secure, MatchPort port]

  optionalMatches =
    NE.nonEmpty $
      catMaybes
        [ MatchHost host <$ guard (host /= "")
        , MatchPath path <$ guard (hasExplicitPath secure host port url)
        , MatchQuery query <$ guard (query /= "")
        , MatchHeaders headers <$ guard (not $ null headers)
        , MatchBody body <$ guard (body /= "")
        ]

hasExplicitPath :: Bool -> ByteString -> Int -> String -> Bool
hasExplicitPath secure host port url =
  any ((`isPrefixOf` url) . toUrlPrefix) $
    concat
      [ [Just port]
      , Nothing <$ guard (secure && port == 443)
      , Nothing <$ guard (not secure && port == 80)
      ]
 where
  toUrlPrefix mport =
    mconcat
      [ "http"
      , if secure then "s" else ""
      , "://"
      , BS8.unpack host
      , maybe "" ((":" <>) . show) mport
      , "/"
      ]

-- | Match a 'Request'
--
-- Success is @'Right' ()@, failure is a message in 'Left'.
matchRequest :: Request -> MatchRequest -> Either String ()
matchRequest req mr =
  maybe (Right ()) (Left . showMatchRequestWithMismatches mr) $
    buildMismatch req mr

showMatchRequest :: MatchRequest -> String
showMatchRequest mr =
  "MatchRequest {"
    <> concatMap (("\n  " <>) . show) (flattenMatchRequest mr)
    <> "\n}"
    <> "\n"

showMatchRequestWithMismatches :: MatchRequest -> NonEmpty String -> String
showMatchRequestWithMismatches mr mismatches =
  showMatchRequest mr
    <> "\nMismatches {"
    <> concatMap ("\n  " <>) (toList mismatches)
    <> "\n}"
    <> "\n"

flattenMatchRequest :: MatchRequest -> [MatchRequest]
flattenMatchRequest = \case
  MatchAnd a b -> flattenMatchRequest a <> flattenMatchRequest b
  x -> [x]

buildMismatch :: Request -> MatchRequest -> Maybe (NonEmpty String)
buildMismatch req = \case
  MatchAnything -> Nothing
  MatchAnd a b -> buildMismatch req a <|> buildMismatch req b
  MatchMethod m -> propMismatch "!=" (==) "method" m HTTP.method req
  MatchSecure s -> propMismatch "!=" (==) "secure" s HTTP.secure req
  MatchHost h -> propMismatch "!=" (==) "host" h HTTP.host req
  MatchPort p -> propMismatch "!=" (==) "port" p HTTP.port req
  MatchPath p -> propMismatch "!=" (==) "path" p HTTP.path req
  MatchQuery q -> propMismatch "!=" (==) "query" q HTTP.queryString req
  MatchHeaders hs -> propMismatch "!=" (==) "headers" hs HTTP.requestHeaders req
  MatchHeader h -> propMismatch "not in" elem "header" h HTTP.requestHeaders req
  MatchBody bs -> propMismatch "!=" (==) "body" bs simplifyRequestBody req

propMismatch
  :: (Show a, Show b)
  => String
  -- ^ Label to show infix when comparison fails, e.g. "!="
  -> (a -> b -> Bool)
  -- ^ How to compare values
  -> String
  -- ^ Label for the property itself
  -> a
  -- ^ Value to compare to property
  -> (Request -> b)
  -- ^ Function to get property from 'Request'
  -> Request
  -> Maybe (NonEmpty String)
propMismatch opLabel op propLabel a f req
  | a `op` b = Nothing
  | otherwise = Just $ pure msg
 where
  b = f req
  msg =
    "✗ "
      <> propLabel
      <> ": "
      <> show a
      <> " "
      <> opLabel
      <> " "
      <> show b

simplifyRequestBody :: Request -> ByteString
simplifyRequestBody = go . HTTP.requestBody
 where
  go = \case
    RequestBodyLBS lbs -> BSL.toStrict lbs
    RequestBodyBS bs -> bs
    _ -> ""
