{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Implements stubbing of an HTTP request function
module Freckle.App.Test.Http
  ( -- $docs
    httpStubbed

    -- * Defining stubs
  , HttpStub (..)
  , httpStub
  , httpStubUrl

    -- * Stub modifiers
  , labelL
  , MatchRequest (..)
  , matchL

    -- * Response modifiers
  , statusL
  , headersL
  , bodyL

    -- * Response helpers
  , json

    -- * FileSystem stubs
  , loadHttpStubsDirectory

    -- * 'MonadHttp' instances

    -- ** For use with @DerivingVia@
  , HasHttpStubs (..)
  , ReaderHttpStubs (..)

    -- ** Concrete transformer
  , HttpStubsT
  , runHttpStubsT
  ) where

import Relude

import Control.Lens (Lens', lens, view,  (.~), (<>~))
import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.List (stripPrefix)
import Freckle.App.Http (MonadHttp (..))
import Freckle.App.Test.Http.MatchRequest
import Network.HTTP.Client (Request, Response)
import Network.HTTP.Client.Internal qualified as HTTP
import Network.HTTP.Types.Header (ResponseHeaders, hAccept, hContentType)
import Network.HTTP.Types.Status (Status, status200)
import System.Directory (doesFileExist)
import System.FilePath (addTrailingPathSeparator)
import System.FilePath.Glob (globDir1)
import Safe(headMay)
import Data.Traversable   (for)

-- | Respond to a 'Request' with the first 'HttpStub' to match
--
-- If no stubs match, 'error' is used. If you'd rather experience a 404, add
-- a final stub for any request that does that:
--
-- @
-- stubs :: ['HttpStub']
-- stubs =
--   [ -- ...
--   , -- ...
--   , 'httpStub' \"Anything\" 'MatchAnything'
--       & 'statusL' .~ 'status404'
--       & 'bodyL' .~ \"Not found\"
--   ]
-- @
httpStubbed
  :: HasCallStack
  => [HttpStub]
  -> Request
  -> Response BSL.ByteString
httpStubbed stubs req =
  maybe (error errorMessage) (toResponse req) $ headMay matched
 where
  (unmatched, matched) =
    partitionEithers
      $ map
        ( \stub ->
            bimap (stub,) (const stub.response)
              $ matchRequest req stub.match
        )
        stubs

  errorMessage =
    toText $ "No stubs were found that matched:\n"
      <> show req
      <> "\n"
      <> concatMap (uncurry unmatchedMessage) unmatched

  unmatchedMessage stub err = "\n== " <> stub.label <> " ==\n" <> err

-- | Fields that can be defined for a response
data HttpStubResponse = HttpStubResponse
  { status :: Status
  , headers :: ResponseHeaders
  , body :: BSL.ByteString
  }

toResponse :: Request -> HttpStubResponse -> Response BSL.ByteString
toResponse req stub =
  HTTP.Response
    { HTTP.responseStatus = stub.status
    , HTTP.responseVersion = HTTP.requestVersion req
    , HTTP.responseHeaders = stub.headers
    , HTTP.responseBody = stub.body
    , HTTP.responseCookieJar = mempty
    , HTTP.responseClose' = HTTP.ResponseClose $ pure ()
    , HTTP.responseOriginalRequest = req
#if MIN_VERSION_http_client(0,7,16)
    , HTTP.responseEarlyHints = []
#endif
    }

rstatusL :: Lens' HttpStubResponse Status
rstatusL = lens (.status) $ \x y -> x {status = y}

rheadersL :: Lens' HttpStubResponse ResponseHeaders
rheadersL = lens (.headers) $ \x y -> x {headers = y}

rbodyL :: Lens' HttpStubResponse BSL.ByteString
rbodyL = lens (.body) $ \x y -> x {body = y}

data HttpStub = HttpStub
  { label :: String
  , match :: MatchRequest
  , response :: HttpStubResponse
  }

instance IsString HttpStub where
  fromString = httpStubUrl

labelL :: Lens' HttpStub String
labelL = lens (.label) $ \x y -> x {label = y}

matchL :: Lens' HttpStub MatchRequest
matchL = lens (.match) $ \x y -> x {match = y}

responseL :: Lens' HttpStub HttpStubResponse
responseL = lens (.response) $ \x y -> x {response = y}

-- | Respond 200 with empty body for matching requests
httpStub :: String -> MatchRequest -> HttpStub
httpStub label match = HttpStub {label, match, response}
 where
  response =
    HttpStubResponse
      { status = status200
      , headers = []
      , body = ""
      }

-- | Respond 200 with empty body for requests parsed from the given URL
httpStubUrl :: String -> HttpStub
httpStubUrl url = httpStub url $ matchRequestFromUrl url

statusL :: Lens' HttpStub Status
statusL = responseL . rstatusL

headersL :: Lens' HttpStub ResponseHeaders
headersL = responseL . rheadersL

bodyL :: Lens' HttpStub BSL.ByteString
bodyL = responseL . rbodyL

-- | Modify the stub to match JSON requests and respond with the given value
json :: ToJSON a => a -> HttpStub -> HttpStub
json a stub =
  stub
    & matchL <>~ MatchHeader (hAccept, "application/json")
    & headersL <>~ [(hContentType, "application/json")]
    & bodyL .~ encode a

-- | Load stubs from the filesystem
--
-- Within the given directory, files are expected to be named for scheme, then
-- host, then path/port/query.
--
-- Given,
--
-- @
-- files/
--   https/
--     www.example.com/
--       hello                  => "Hello"
--       world                  => "World"
--   http/
--     localhost:3000/
--       hello?world=1          => "Hello 2"
-- @
--
-- Then @'loadHttpStubsDirectory' "files"@ is equivalent to,
--
-- @
-- [ 'stubUrl' \"https:\/\/www.example.com\/hello\" & 'bodyL' .~ \"Hello\"
-- , 'stubUrl' \"https:\/\/www.example.com\/world\" & 'bodyL' .~ \"World\"
-- , 'stubUrl' \"http:\/\/localhost:3000\/hello?world=1\" & 'bodyL' .~ \"Hello 2\"
-- ]
-- @
--
-- NB. This function currently abuses the fact that @/@ within filenames is the
-- same for URLs, and so will not work on Windows. Patches welcome.
loadHttpStubsDirectory :: FilePath -> IO [HttpStub]
loadHttpStubsDirectory dir = do
  paths <- filterM doesFileExist =<< globDir1 "**/*" dir

  let pathUrls = mapMaybe (\p -> (,) p <$> toUrl p) paths

  for pathUrls $ \(path, url) -> do
    bs <- BSL.readFile path
    pure $ httpStubUrl url & bodyL .~ bs
 where
  toUrl p = do
    relative <- stripPrefix (addTrailingPathSeparator dir) p
    asum
      [ ("https://" <>) <$> stripPrefix "https/" relative
      , ("http://" <>) <$> stripPrefix "http/" relative
      ]

class HasHttpStubs env where
  httpStubsL :: Lens' env [HttpStub]

instance HasHttpStubs [HttpStub] where
  httpStubsL = id

newtype ReaderHttpStubs m a = ReaderHttpStubs {unwrap :: m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader env)

instance (MonadReader env m, HasHttpStubs env) => MonadHttp (ReaderHttpStubs m) where
  httpLbs req = do
    stubs <- view httpStubsL
    pure $ httpStubbed stubs req

newtype HttpStubsT m a = HttpStubsT {unwrap :: ReaderT [HttpStub] m a}
  deriving newtype (Functor, Applicative, Monad, MonadReader [HttpStub])
  deriving (MonadHttp) via ReaderHttpStubs (HttpStubsT m)

runHttpStubsT :: HttpStubsT m a -> [HttpStub] -> m a
runHttpStubsT f = runReaderT f.unwrap

-- $docs
--
-- Stubbing is accomplished by holding a list of 'HttpStub' somewhere, which
-- defines how to respond to requests that match. The simplest way to do so
-- is to use the 'IsString' instance:
--
-- > stubs :: [HttpStub]
-- > stubs =
-- >   [ "https://example.com"
-- >   ]
--
-- You can now use,
--
-- @
-- 'httpStubbed' stubs :: Request -> Response ByteString
-- @
--
-- Anywhere you need an HTTP requesting function and it will respond 200 with an
-- empty body for any @GET@ requests made to this domain.
--
-- Stubbed responses can be modified through lenses:
--
-- > stubs :: [HttpStub]
-- > stubs =
-- >   [ "https://example.com"
-- >       & statusL .~ status400
-- >       & bodyL .~ "Let's test a Bad Request"
-- >   ]
--
-- The string is passed to 'parseRequest_', so anything valid there is valid
-- here, such as setting the method:
--
-- > data MyItem = MyItem
-- >   { -- ...
-- >   }
-- >   deriving stock Generic
-- >   deriving anyclass ToJSON
-- >
-- > stubs :: [HttpStub]
-- > stubs =
-- >   [ "POST https://example.com/items"
-- >       & json [MyItem]
-- >       -- ^ Now matches requests with JSON in the Accept Header only
-- >       --   Responds with Content-Type JSON
-- >       --   Responds with a body of the JSON-encoded items
-- >   ]
--
-- == 'MonadHttp'
--
-- Once we have the @stubs@, we can set up a 'MonadHttp' context that uses it:
--
-- > data TestApp = TestApp
-- >   { appHttpStubs :: [HttpStubs]
-- >   }
-- >
-- > -- Assume TestAppT is a ReaderT TestApp
-- > instance MonadHttp (TestAppT m a) where
-- >   httpLbs req = do
-- >     stubs <- asks appHttpStubs
-- >     pure $ httpStubbed stubs req
--
-- Additionally, there are tools for @DerivingVia@ or running things in a
-- concrete 'HttpStubsT' stack.
--
-- == Handling Un-stubbed Requests
--
-- When no stubs match a given request, we call 'error' -- this seems uncouth,
-- but is actually the best possible behavior for the intended use-case in
-- (e.g.) HSpec:
--
-- ![Error screenshot](https://files.pbrisbin.com/screenshots/screenshot.281851.png)
--
-- One other reasonable behavior would be to respond 404 to any un-matched
-- requests. This can be accomplished by adding a "match anything" stub at the
-- end:
--
-- > stubs :: [HttpStub]
-- > stubs =
-- >   [ -- ...
-- >   , -- ...
-- >   , httpStub "Anything" MatchAnything
-- >       & statusL .~ status404
-- >       & bodyL .~ "Not found"
-- >   ]
