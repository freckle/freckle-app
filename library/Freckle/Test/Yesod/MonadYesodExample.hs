{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Freckle.Test.Yesod.MonadYesodExample
  ( MonadYesodExample (..)
  )
where

import Freckle.App.Prelude

import Control.Monad.State (StateT)
import Yesod.Core (Yesod)
import Yesod.Test (YesodExample)

class (MonadIO m, Yesod site) => MonadYesodExample site m | m -> site where
  liftYesodExample :: YesodExample site a -> m a

instance Yesod site => MonadYesodExample site (YesodExample site) where
  liftYesodExample = id

instance MonadYesodExample site m => MonadYesodExample site (StateT s m) where
  liftYesodExample = lift . liftYesodExample

instance MonadYesodExample site m => MonadYesodExample site (ReaderT r m) where
  liftYesodExample = lift . liftYesodExample
