-- | Similar to "Yesod.Test" from the yesod-test package.
--
-- Differences:
--
-- * Contains a few additional conveniences, notably
--   "Freckle.Test.Yesod.JsonAssertions"
-- * Actions in the 'YesodExample' monad are generalized to a
--   'MonadYesodExample' constraint, allowing tests to be written
--   in custom monads more easily.
module Freckle.Test.Yesod (module X) where

import Freckle.Test.Yesod.BodyAssertions as X
import Freckle.Test.Yesod.Cookies as X
import Freckle.Test.Yesod.Foundation as X
import Freckle.Test.Yesod.GetBody as X
import Freckle.Test.Yesod.HeaderAssertions as X
import Freckle.Test.Yesod.JsonAssertions as X
import Freckle.Test.Yesod.MakingRequests as X
import Freckle.Test.Yesod.MonadYesodExample as X
import Freckle.Test.Yesod.RequestBuilder as X
import Freckle.Test.Yesod.SResponse as X
import Freckle.Test.Yesod.StatusAssertions as X
