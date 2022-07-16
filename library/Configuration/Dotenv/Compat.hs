{-# LANGUAGE CPP #-}

module Configuration.Dotenv.Compat
  ( module X
  ) where

import Configuration.Dotenv as X

#if !MIN_VERSION_dotenv(0,6,0)
-- The Config type was here in older versions
import Configuration.Dotenv.Types as X
#endif
