module Freckle.App.Memcached.MD5
  ( md5CacheKey
  , md5Key
  , md5Text
  ) where

import Freckle.App.Prelude

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Digest.Pure.MD5 as Digest
import Freckle.App.Memcached.CacheKey

md5CacheKey :: Show a => a -> CacheKey
md5CacheKey = either (error "md5 is always cacheable") id . cacheKey . md5Key

-- | Pack any showable into an md5 encoded text
md5Key :: Show a => a -> Text
md5Key = md5Text . pack . show

md5Text :: Text -> Text
md5Text = pack . show . Digest.md5 . BSL.fromStrict . encodeUtf8
