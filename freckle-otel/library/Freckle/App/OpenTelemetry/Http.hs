module Freckle.App.OpenTelemetry.Http
  ( httpSpanName
  , httpSpanArguments
  , httpAttributes
  , httpResponseAttributes
  ) where

import Prelude

import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error qualified as T
import Freckle.App.OpenTelemetry
  ( SpanArguments (..)
  , byteStringToAttribute
  , clientSpanArguments
  )
import Network.HTTP.Client (Request, Response)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types.Status (statusCode)
import OpenTelemetry.Attributes (Attribute, ToAttribute (..))

httpSpanName :: Request -> Text
httpSpanName req =
  T.decodeUtf8With T.lenientDecode $ HTTP.method req <> " " <> HTTP.path req

httpSpanArguments :: Request -> SpanArguments
httpSpanArguments req = clientSpanArguments {attributes = httpAttributes req}

httpAttributes :: Request -> HashMap Text Attribute
httpAttributes req =
  HashMap.fromList
    [ ("service.name", byteStringToAttribute $ HTTP.host req)
    , ("resource.name", toAttribute $ httpSpanName req)
    , ("http.host", byteStringToAttribute $ HTTP.host req)
    , ("http.method", byteStringToAttribute $ HTTP.method req)
    , ("http.path", byteStringToAttribute $ HTTP.path req)
    , ("http.query", byteStringToAttribute $ HTTP.queryString req)
    ]

httpResponseAttributes :: Response body -> HashMap Text Attribute
httpResponseAttributes resp = statusAttr <> foldMap (uncurry headerAttr) (HTTP.responseHeaders resp)
 where
  statusAttr =
    HashMap.singleton "http.status_code"
      . toAttribute
      . statusCode
      $ HTTP.responseStatus resp

  headerAttr k = HashMap.singleton (headerAttrKey k) . byteStringToAttribute

  headerAttrKey =
    ("http.response.headers." <>)
      . T.toLower
      . T.decodeUtf8With T.lenientDecode
      . CI.original
