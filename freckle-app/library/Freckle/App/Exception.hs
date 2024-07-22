module Freckle.App.Exception
  ( -- * Re-export of our current preferred implementation module

    -- | Currently 'MonadUnliftIO'-based
      module Freckle.App.Exception.MonadUnliftIO

    -- * Helpers that are agnostic to either implementation
  , annotatedExceptionMessage
  , annotatedExceptionMessageFrom
  ) where

import Prelude

import Control.Exception.Annotated (annotatedExceptionCallStack)
import Control.Exception.Annotated qualified as AnnotatedException
import Control.Monad.Logger.Aeson (Message (..), (.=))
import Data.Aeson (object)
import Freckle.App.Exception.MonadUnliftIO
import GHC.Exception (prettyCallStack)

-- | Construct a log 'Message' for any @'AnnotatedException' exception@
--
-- Produces a log message that works with Datadog /Standard Attributes/.
--
-- <https://docs.datadoghq.com/standard-attributes/?search=error.>
--
-- @
-- Exception
--    error.message: {displayException on underlying exception}
--    error.stack: {prettyCallStack from the annotation, if available}
-- @
--
-- You are expected to call this with a @TypeApplication@, for example:
--
-- @
-- 'catch' myAction $ 'logError' . 'annotatedExceptionMessage' @MyException
-- @
annotatedExceptionMessage :: Exception ex => AnnotatedException ex -> Message
annotatedExceptionMessage = annotatedExceptionMessageFrom $ const "Exception"

-- | Like 'annotatedExceptionMessage', but use the supplied function to
--   construct an initial 'Message' that it will augment.
annotatedExceptionMessageFrom
  :: Exception ex => (ex -> Message) -> AnnotatedException ex -> Message
annotatedExceptionMessageFrom f ann = case f ex of
  msg :# series -> msg :# series <> ["error" .= errorObject]
 where
  ex = AnnotatedException.exception ann
  errorObject =
    object
      [ "message" .= displayException ex
      , "stack" .= (prettyCallStack <$> annotatedExceptionCallStack ann)
      ]
