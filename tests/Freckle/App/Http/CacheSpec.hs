{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Freckle.App.Http.CacheSpec
  ( spec
  ) where

import Freckle.App.Prelude

import qualified Codec.Compression.GZip as GZip
import Control.Lens ((&), (.~), (<>~))
import Control.Monad.State (StateT, execStateT)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import Data.Time (addUTCTime)
import Freckle.App.Http
import Freckle.App.Http.Cache
import Freckle.App.Http.Cache.KeyExtension
import Freckle.App.Http.Cache.State
import Freckle.App.Test.Http
import Network.HTTP.Types.Header
  ( hAcceptEncoding
  , hAcceptLanguage
  , hAge
  , hCacheControl
  , hContentEncoding
  , hETag
  , hExpires
  , hIfNoneMatch
  , hVary
  )
import Network.HTTP.Types.Status
  ( status100
  , status304
  , status307
  , status400
  , status503
  )
import Test.Hspec (Spec, context, describe, it)
import Test.Hspec.Expectations.Json.Lifted (shouldMatchJson)
import Test.Hspec.Expectations.Lifted

type CacheSettings = HttpCacheSettings (StateT Cache IO) CachedResponse

spec :: Spec
spec = do
  describe "httpCached" $ do
    it "caches successful GET requests" $ do
      let
        stubs =
          [ "https://example.com/1" & bodyL .~ "Hello\n"
          , "https://example.com/2" & bodyL .~ "World\n"
          ]

        req1 = parseRequest_ "https://example.com/1"
        req2 = parseRequest_ "https://example.com/2"

      cache <- execCached $ do
        requestBodyCached settings stubs req1 `shouldReturn` "Hello\n"
        requestBodyCached settings stubs req2 `shouldReturn` "World\n"

        -- No stubs, so these would fail if not cached
        requestBodyCached settings [] req1 `shouldReturn` "Hello\n"
        requestBodyCached settings [] req2 `shouldReturn` "World\n"

      cache.map `shouldSatisfy` ((== 2) . HashMap.size)

    it "evicts stale caches" $ do
      let
        stubs =
          [ "https://example.com/1"
              & headersL <>~ [(hCacheControl, "max-age=2")]
              & bodyL .~ "Hi\n"
          ]

        -- On the request that we expect to evict, we'll use this so that we
        -- don't store a cache from that and we can observe the eviction.
        stubsNoStore =
          [ "https://example.com/1"
              & headersL <>~ [(hCacheControl, "no-store")]
              & bodyL .~ "Hi\n"
          ]

        req = parseRequest_ "https://example.com/1"

      cache <- execCached $ do
        requestBodyCached settings stubs req `shouldReturn` "Hi\n"

        -- Cached, no requests made
        requestBodyCached settings [] req `shouldReturn` "Hi\n"

        -- Expired, trigger eviction
        requestBodyCached settingsFuture stubsNoStore req `shouldReturn` "Hi\n"

      cache.map `shouldSatisfy` ((== 0) . HashMap.size)

    it "incorporates cacheKeyHeaders into the cache key" $ do
      let
        stubs =
          [ "https://example.com/1"
              & matchL <>~ MatchHeader (hAcceptLanguage, "en")
              & headersL <>~ [(hVary, "accept-language")]
              & bodyL .~ "Hello\n"
          , "https://example.com/1"
              & matchL <>~ MatchHeader (hAcceptLanguage, "es")
              & headersL <>~ [(hVary, "accept-language")]
              & bodyL .~ "Hola\n"
          , "https://example.com/2"
              & matchL <>~ MatchHeader (hAcceptLanguage, "en")
              & headersL <>~ [(hVary, "accept-language")]
              & bodyL .~ "World\n"
          , "https://example.com/2"
              & matchL <>~ MatchHeader (hAcceptLanguage, "es")
              & headersL <>~ [(hVary, "accept-language")]
              & bodyL .~ "Mundo\n"
          ]

        reqEn1 =
          parseRequest_ "https://example.com/1"
            & addRequestHeader hAcceptLanguage "en"
        reqEn2 =
          parseRequest_ "https://example.com/2"
            & addRequestHeader hAcceptLanguage "en"
        reqEs1 =
          parseRequest_ "https://example.com/1"
            & addRequestHeader hAcceptLanguage "es"
        reqEs2 =
          parseRequest_ "https://example.com/2"
            & addRequestHeader hAcceptLanguage "es"

        settings' =
          settings
            { cacheKeyExtension = includeHeader id hAcceptLanguage
            }

      cache <- execCached $ do
        requestBodyCached settings' stubs reqEn1 `shouldReturn` "Hello\n"
        requestBodyCached settings' stubs reqEn2 `shouldReturn` "World\n"
        requestBodyCached settings' stubs reqEs1 `shouldReturn` "Hola\n"
        requestBodyCached settings' stubs reqEs2 `shouldReturn` "Mundo\n"

        -- No stubs, so these would fail if not cached
        requestBodyCached settings' [] reqEn1 `shouldReturn` "Hello\n"
        requestBodyCached settings' [] reqEn2 `shouldReturn` "World\n"
        requestBodyCached settings' [] reqEs1 `shouldReturn` "Hola\n"
        requestBodyCached settings' [] reqEs2 `shouldReturn` "Mundo\n"

      cache.map `shouldSatisfy` ((== 4) . HashMap.size)

    context "compression" $ do
      it "caches gzipped responses as gzipped" $ do
        let
          gzipped = GZip.compress "Hi (zipped)\n"

          stubs =
            [ "https://example.com/1"
                & matchL <>~ MatchHeader (hAcceptEncoding, "gzip")
                & headersL <>~ [(hContentEncoding, "gzip")]
                & headersL <>~ [(hVary, "accept-encoding")]
                & bodyL .~ gzipped
            , "https://example.com/1"
                & bodyL .~ "Hi (not zipped)\n"
                & headersL <>~ [(hContentEncoding, "text/plain")]
                & headersL <>~ [(hVary, "accept-encoding")]
            ]

          req =
            parseRequest_ "https://example.com/1"
              & addRequestHeader hAcceptEncoding "text-plain"
          reqGzipped =
            parseRequest_ "https://example.com/1"
              & addRequestHeader hAcceptEncoding "gzip"
          reqGzippedAsIs =
            parseRequest_ "https://example.com/1"
              & addRequestHeader hAcceptEncoding "gzip"
              & disableRequestDecompress

        cache <- execCached $ do
          requestBodyCached settings stubs req `shouldReturn` "Hi (not zipped)\n"
          requestBodyCached settings stubs reqGzipped `shouldReturn` "Hi (zipped)\n"
          requestBodyCached settings stubs reqGzippedAsIs `shouldReturn` gzipped

        cache.map `shouldSatisfy` ((== 2) . HashMap.size)

        -- We don't want to expose the constructor, but we do want to verify the
        -- cache contains the gzipped form.
        map (show . getResponseBody . (.response) . snd) (HashMap.toList cache.map)
          `shouldMatchList` [ "PotentiallyGzipped {unwrap = \"Hi (not zipped)\\n\"}"
                            , "PotentiallyGzipped {unwrap = " <> show gzipped <> "}"
                            ]

      it "handles large gzip responses correctly" $ do
        bs <- BSL.readFile "tests/files/constructed-responses.gzip"
        val <- expectDecode $ GZip.decompress bs

        let
          stubs =
            [ "https://example.com/1"
                & matchL <>~ MatchHeader (hAcceptEncoding, "gzip")
                & headersL <>~ [(hContentEncoding, "gzip")]
                & bodyL .~ bs
            ]

          req =
            parseRequest_ "https://example.com/1"
              & addRequestHeader hVary "accept-encoding"
              & addRequestHeader hAcceptEncoding "gzip"

        void $ execCached $ do
          actual <- expectDecode =<< requestBodyCached settings stubs req
          actual `shouldMatchJson` val

    context "Handling ETag" $ do
      let etag = "W/\"99\""

      it "uses cached response and doesn't evict on 304 from If-None-Match" $ do
        let stubs =
              [ "https://example.com/1"
                  & matchL <>~ MatchHeader (hIfNoneMatch, etag)
                  & statusL .~ status304
                  & bodyL .~ "<ignore me>\n"
              , "https://example.com/1"
                  & headersL <>~ [(hCacheControl, "max-age=-1")]
                  & headersL <>~ [(hETag, etag)]
                  & bodyL .~ "Original body\n"
              ]

        cache <- execCached $ do
          let req = parseRequest_ "https://example.com/1"
          requestBodyCached settings stubs req `shouldReturn` "Original body\n"
          requestBodyCached settings stubs req `shouldReturn` "Original body\n"

        cache.map `shouldSatisfy` ((== 1) . HashMap.size)

      it "updates cached response on 304 from If-None-Match" $ do
        let stubs =
              [ "https://example.com/1"
                  & matchL <>~ MatchHeader (hIfNoneMatch, etag)
                  & statusL .~ status304
                  & headersL <>~ [(hCacheControl, "max-age=120")]
                  & bodyL .~ "<ignore me>\n"
              , "https://example.com/1"
                  & headersL <>~ [(hCacheControl, "max-age=-1")]
                  & headersL <>~ [(hETag, etag)]
                  & bodyL .~ "Original body\n"
              ]

        cache <- execCached $ do
          let req = parseRequest_ "https://example.com/1"
          requestBodyCached settings stubs req `shouldReturn` "Original body\n"
          requestBodyCached settings stubs req `shouldReturn` "Original body\n"

        cache.map `shouldSatisfy` ((== 1) . HashMap.size)
        map (.ttl) (HashMap.elems cache.map) `shouldBe` [120]

      it "evicts a stale response after trying If-None-Match" $ do
        let stubs =
              [ "https://example.com/1"
                  & matchL <>~ MatchHeader (hIfNoneMatch, etag)
                  & headersL <>~ [(hCacheControl, "no-store")]
                  & bodyL .~ "Newer body\n"
              , "https://example.com/1"
                  & headersL <>~ [(hCacheControl, "max-age=-1")]
                  & headersL <>~ [(hETag, etag)]
                  & bodyL .~ "Original body\n"
              ]

        cache <- execCached $ do
          let req = parseRequest_ "https://example.com/1"
          requestBodyCached settings stubs req `shouldReturn` "Original body\n"
          requestBodyCached settings stubs req `shouldReturn` "Newer body\n"

        cache.map `shouldSatisfy` ((== 0) . HashMap.size)

    context "setting TTL" $ do
      let req = parseRequest_ "https://example.com"

      it "sets TTL based on max-age" $ do
        let stubs =
              [ "https://example.com"
                  & headersL <>~ [(hCacheControl, "max-age=42")]
              ]

        cache <- execCached $ requestBodyCached settings stubs req
        map (.ttl) (HashMap.elems cache.map) `shouldBe` [42]

      it "sets TTL based on max-age + Age" $ do
        let stubs =
              [ "https://example.com"
                  & headersL <>~ [(hAge, "78000"), (hCacheControl, "max-age=78250")]
              ]

        cache <- execCached $ requestBodyCached settings stubs req
        map (.ttl) (HashMap.elems cache.map) `shouldBe` [250]

      it "sets TTL based on Expires" $ do
        let
          expDate = "Wed, 21 Oct 2015 07:28:00 GMT"
          expSeconds = 1445412480 -- `date --date '{eDate}' +%s`
          stubs = ["https://example.com" & headersL <>~ [(hExpires, expDate)]]

        cache <- execCached $ requestBodyCached settings stubs req
        map (.ttl) (HashMap.elems cache.map) `shouldBe` [expSeconds]

    context "un-cacheable requests" $ do
      it "does not cache if told not to" $ do
        let req = parseRequest_ "https://example.com"
        cache <- execCached $ requestBodyCached settingsDisabled stubAnything req
        cache.map `shouldSatisfy` ((== 0) . HashMap.size)

      it "does not cache non-GET methods" $ do
        let req = parseRequest_ "POST https://example.com"
        cache <- execCached $ requestBodyCached settings stubAnything req
        cache.map `shouldSatisfy` ((== 0) . HashMap.size)

      it "does not cache no-store" $ do
        let req =
              parseRequest_ "https://example.com"
                & addRequestHeader hCacheControl "no-store"
        cache <- execCached $ requestBodyCached settings stubAnything req
        cache.map `shouldSatisfy` ((== 0) . HashMap.size)

      it "does not cache private in a shared cache" $ do
        let req =
              parseRequest_ "https://example.com"
                & addRequestHeader hCacheControl "private"
        cache <- execCached $ requestBodyCached settingsShared stubAnything req
        cache.map `shouldSatisfy` ((== 0) . HashMap.size)

    context "un-cacheable responses" $ do
      let req = parseRequest_ "https://example.com"

      it "does not cache no-store" $ do
        let stubs =
              [ "https://example.com"
                  & headersL <>~ [(hCacheControl, "no-store, max-age=0, public")]
              ]

        cache <- execCached $ requestBodyCached settings stubs req
        cache.map `shouldSatisfy` ((== 0) . HashMap.size)

      it "does not cache private in a shared cache" $ do
        let stubs =
              [ "https://example.com"
                  & headersL <>~ [(hCacheControl, "max-age=0, private")]
              ]

        cache <- execCached $ requestBodyCached settingsShared stubs req
        cache.map `shouldSatisfy` ((== 0) . HashMap.size)

      for_ [status100, status307, status400, status503] $ \s -> do
        it ("does not cache un-cacheable status " <> show (statusCode s)) $ do
          let stubs = ["https://example.com" & statusL .~ s]

          cache <- execCached $ requestBodyCached settingsShared stubs req
          cache.map `shouldSatisfy` ((== 0) . HashMap.size)

execCached :: StateT Cache IO a -> IO Cache
execCached = flip execStateT mempty

requestBodyCached
  :: CacheSettings
  -> [HttpStub]
  -> Request
  -> StateT Cache IO ByteString
requestBodyCached ss stubs req =
  getResponseBody <$> httpCached ss (pure . httpStubbed stubs) req

settings :: CacheSettings
settings = stateHttpCacheSettings

settingsDisabled :: CacheSettings
settingsDisabled =
  settings
    { cacheable = const False
    }

settingsShared :: CacheSettings
settingsShared =
  settings
    { shared = True
    }

settingsFuture :: CacheSettings
settingsFuture =
  settings
    { getCurrentTime = liftIO $ addUTCTime 5 <$> getCurrentTime
    }

stubAnything :: [HttpStub]
stubAnything = [httpStub "Anything" MatchAnything]

expectDecode :: (HasCallStack, MonadIO m, FromJSON a) => ByteString -> m a
expectDecode bs = case eitherDecode bs of
  Left err -> do
    expectationFailure $
      mconcat
        [ "Expected input to decode as JSON"
        , "\nInput:  " <> show bs
        , "\nErrors: " <> err
        ]
    error "<unreachable>"
  Right a -> pure a
