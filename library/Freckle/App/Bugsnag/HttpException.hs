module Freckle.App.Bugsnag.HttpException
  ( httpExceptionBeforeNotify

    -- * Re-exports
  , HttpException
  ) where

import Freckle.App.Prelude

import Data.Bugsnag (Exception (..))
import qualified Data.ByteString.Char8 as BS8
import Freckle.App.Exception.Types (AnnotatedException)
import qualified Freckle.App.Exception.Types as Annotated
import Network.Bugsnag
  ( BeforeNotify
  , setGroupingHash
  , updateEventFromOriginalException
  , updateExceptions
  )
import Network.HTTP.Client (HttpException (..), host, method)

httpExceptionBeforeNotify :: BeforeNotify
httpExceptionBeforeNotify =
  updateEventFromOriginalException @(AnnotatedException HttpException)
    (asHttpException . Annotated.exception)

asHttpException :: HttpException -> BeforeNotify
asHttpException (HttpExceptionRequest req content) =
  setGroupingHash (decodeUtf8 $ host req) <> update
 where
  update = updateExceptions $ \ex ->
    ex
      { exception_errorClass = "HttpExceptionRequest"
      , exception_message =
          Just
            . decodeUtf8
            $ method req
              <> " request to "
              <> host req
              <> " failed: "
              <> BS8.pack (show content)
      }
asHttpException (InvalidUrlException url msg) = updateExceptions $ \ex ->
  ex
    { exception_errorClass = "InvalidUrlException"
    , exception_message = Just $ pack $ url <> " is invalid: " <> msg
    }
