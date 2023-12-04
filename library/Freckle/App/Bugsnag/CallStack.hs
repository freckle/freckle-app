module Freckle.App.Bugsnag.CallStack
  ( callStackBeforeNotify
  , attachCallStack
  , callStackToStackFrames
  , callSiteToStackFrame

    -- * Re-exports
  , CallStack
  , SrcLoc
  , StackFrame
  ) where

import Freckle.App.Prelude

import Control.Exception.Annotated (annotatedExceptionCallStack)
import Data.Bugsnag (Exception (..), StackFrame (..), defaultStackFrame)
import qualified Data.Text as T
import Freckle.App.Exception.Types (AnnotatedException)
import GHC.Stack (CallStack, SrcLoc (..), getCallStack)
import Network.Bugsnag (BeforeNotify, updateExceptions)
import Network.Bugsnag.BeforeNotify (updateEventFromOriginalException)

-- | Copy the call stack from an AnnotatedException
callStackBeforeNotify :: BeforeNotify
callStackBeforeNotify =
  updateEventFromOriginalException @(AnnotatedException SomeException) $ \e ->
    foldMap attachCallStack $ annotatedExceptionCallStack e

attachCallStack :: CallStack -> BeforeNotify
attachCallStack cs =
  updateExceptions $ \ex ->
    ex {exception_stacktrace = callStackToStackFrames cs}

-- | Converts a GHC call stack to a list of stack frames suitable
--   for use as the stacktrace in a Bugsnag exception
callStackToStackFrames :: CallStack -> [StackFrame]
callStackToStackFrames = fmap callSiteToStackFrame . getCallStack

callSiteToStackFrame :: (String, SrcLoc) -> StackFrame
callSiteToStackFrame (str, loc) =
  defaultStackFrame
    { stackFrame_method = T.pack str
    , stackFrame_file = T.pack $ srcLocFile loc
    , stackFrame_lineNumber = srcLocStartLine loc
    , stackFrame_columnNumber = Just $ srcLocStartCol loc
    }
