module Freckle.App.Bugsnag.ErrorClass
  ( errorClassBeforeNotify
  ) where

import Freckle.App.Prelude

import Control.Exception.Annotated (AnnotatedException)
import qualified Control.Exception.Annotated as Annotated
import qualified Data.Bugsnag as Bugsnag
import Data.Typeable (Proxy (..), typeRep)
import Network.Bugsnag
  ( BeforeNotify
  , updateEventFromOriginalException
  , updateExceptions
  )

-- | Use the exception's type name at the error class
--
-- If the exception type is @'AnnotatedException' WhateverExceptionType@,
-- the 'AnnotatedException' part will be unwrapped and the error class
-- will be @WhateverExceptionType@.
errorClassBeforeNotify :: BeforeNotify
errorClassBeforeNotify = updateEventFromOriginalException asAnnotatedException

asAnnotatedException :: AnnotatedException SomeException -> BeforeNotify
asAnnotatedException = updateExceptions . setErrorClass . someErrorClass . Annotated.exception

-- | Unwrap the 'SomeException' newtype to get the actual underlying type name
someErrorClass :: SomeException -> Text
someErrorClass (SomeException (_ :: e)) = pack $ show $ typeRep $ Proxy @e

setErrorClass :: Text -> Bugsnag.Exception -> Bugsnag.Exception
setErrorClass c ex = ex {Bugsnag.exception_errorClass = c}
