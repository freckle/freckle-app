module Freckle.App.Bugsnag.Message
  ( messageBeforeNotify
  ) where

import Freckle.App.Prelude

import Control.Exception.Annotated (AnnotatedException)
import qualified Control.Exception.Annotated as Annotated
import qualified Data.Bugsnag as Bugsnag
import Network.Bugsnag
  ( BeforeNotify
  , updateEventFromOriginalException
  , updateExceptions
  )

-- | Use the output of 'displayException' as the exception message
--
-- If the exception type is @'AnnotatedException' WhateverExceptionType@,
-- the 'AnnotatedException' part will be unwrapped and only the underlying
-- exception, without annotations, will be displayed.
messageBeforeNotify :: BeforeNotify
messageBeforeNotify = updateEventFromOriginalException asAnnotatedException

asAnnotatedException :: AnnotatedException SomeException -> BeforeNotify
asAnnotatedException =
  updateExceptions
    . setErrorMessage
    . pack
    . displayException
    . Annotated.exception

setErrorMessage :: Text -> Bugsnag.Exception -> Bugsnag.Exception
setErrorMessage c ex = ex {Bugsnag.exception_errorClass = c}
