module Freckle.App.Memcached.MD5
  ( md5CacheKey
  , md5Key
  , md5Text
  ) where

import Prelude

import Data.ByteString.Lazy qualified as BSL
import Data.Digest.Pure.MD5 qualified as Digest
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Freckle.App.Memcached.CacheKey

md5CacheKey :: Show a => a -> CacheKey
md5CacheKey = either (error "md5 is always cacheable") id . cacheKey . md5Key

-- | Pack any showable into an md5 encoded text
md5Key :: Show a => a -> Text
md5Key = md5Text . T.pack . show

md5Text :: Text -> Text
md5Text = T.pack . show . Digest.md5 . BSL.fromStrict . T.encodeUtf8
