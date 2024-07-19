module Freckle.App.Http.Header
  ( HasHeaders (..)
  , getHeaderCsv
  , lookupHeader

    -- * Utilities
  , splitHeader
  ) where

import Freckle.App.Prelude

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Char (isSpace)
import Network.HTTP.Client (Request, Response, requestHeaders, responseHeaders)
import Network.HTTP.Simple (getRequestHeader, getResponseHeader)
import Network.HTTP.Types.Header (Header, HeaderName)

class HasHeaders a where
  getHeaders :: a -> [Header]

  getHeader :: HeaderName -> a -> [ByteString]
  getHeader h = map snd . filter ((== h) . fst) . getHeaders

instance HasHeaders [Header] where
  getHeaders = id

instance HasHeaders Request where
  getHeaders = requestHeaders
  getHeader = getRequestHeader

instance HasHeaders (Response body) where
  getHeaders = responseHeaders
  getHeader = getResponseHeader

getHeaderCsv :: HasHeaders a => HeaderName -> a -> [ByteString]
getHeaderCsv hn = concatMap splitHeader . getHeader hn

splitHeader :: ByteString -> [ByteString]
splitHeader = map trimSpace . BS8.split ','

trimSpace :: ByteString -> ByteString
trimSpace = BS8.dropWhile isSpace . BS8.dropWhileEnd isSpace

lookupHeader :: HasHeaders a => HeaderName -> a -> Maybe ByteString
lookupHeader h = listToMaybe . getHeader h
