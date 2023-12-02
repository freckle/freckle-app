module Freckle.App.Bugsnag.CallStack
  ( callStackToBugsnag
  ) where

import Data.Bugsnag (StackFrame (..), defaultStackFrame)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Maybe (Maybe (..))
import Data.String (String)
import qualified Data.Text as T
import GHC.Stack (CallStack, SrcLoc (..), getCallStack)

-- | Converts a GHC call stack to a list of stack frames suitable
--   for use as the stacktrace in a Bugsnag exception
callStackToBugsnag :: CallStack -> [StackFrame]
callStackToBugsnag = fmap callSiteToBugsnag . getCallStack

callSiteToBugsnag :: (String, SrcLoc) -> StackFrame
callSiteToBugsnag (str, loc) =
  defaultStackFrame
    { stackFrame_method = T.pack str
    , stackFrame_file = T.pack $ srcLocFile loc
    , stackFrame_lineNumber = srcLocStartLine loc
    , stackFrame_columnNumber = Just $ srcLocStartCol loc
    }
