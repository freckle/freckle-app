module Freckle.App.OpenTelemetry.Http
  ( httpSpanName
  , httpSpanArguments
  , httpAttributes
  , httpResponseAttributes
  ) where

import Freckle.App.Prelude

import qualified Data.CaseInsensitive as CI
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Freckle.App.OpenTelemetry
  ( SpanArguments (..)
  , byteStringToAttribute
  , clientSpanArguments
  )
import Network.HTTP.Client (Request, Response)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status (statusCode)
import OpenTelemetry.Attributes (Attribute, ToAttribute (..))

httpSpanName :: Request -> Text
httpSpanName req =
  decodeUtf8With lenientDecode $ HTTP.method req <> " " <> HTTP.path req

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
      . decodeUtf8With lenientDecode
      . CI.original
