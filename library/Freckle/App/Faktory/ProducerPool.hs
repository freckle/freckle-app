{-# LANGUAGE ApplicativeDo #-}

module Freckle.App.Faktory.ProducerPool
  ( FaktoryProducerPool
  , FaktoryProducerPoolConfig (..)
  , envFaktoryProducerPoolConfig
  , HasFaktoryProducerPool (..)
  , createFaktoryProducerPool
  ) where

import Freckle.App.Prelude

import Control.Lens (Lens')
import Data.Pool
  ( Pool
  , defaultPoolConfig
  , newPool
  , setNumStripes
  )
import Faktory.Producer qualified as Faktory
import Faktory.Settings qualified as Faktory
import Freckle.App.Env qualified as Env
import Yesod.Core.Lens (envL, siteL)
import Yesod.Core.Types (HandlerData)

data FaktoryProducerPoolConfig = FaktoryProducerPoolConfig
  { faktoryProducerPoolConfigStripes :: Int
  -- ^ The number of stripes (distinct sub-pools) to maintain.
  -- The smallest acceptable value is 1.
  , faktoryProducerPoolConfigIdleTimeout :: NominalDiffTime
  -- ^ Amount of time for which an unused resource is kept open.
  -- The smallest acceptable value is 0.5 seconds.
  --
  -- The elapsed time before destroying a resource may be a little
  -- longer than requested, as the reaper thread wakes at 1-second
  -- intervals.
  , faktoryProducerPoolConfigSize :: Int
  -- ^ Maximum number of resources to keep open per stripe.  The
  -- smallest acceptable value is 1.
  --
  -- Requests for resources will block if this limit is reached on a
  -- single stripe, even if other stripes have idle resources
  -- available.
  }
  deriving stock (Show)

-- | Same defaults as 'Database.Persist.Sql.ConnectionPoolConfig'
defaultFaktoryProducerPoolConfig :: FaktoryProducerPoolConfig
defaultFaktoryProducerPoolConfig = FaktoryProducerPoolConfig 1 600 10

envFaktoryProducerPoolConfig
  :: Env.Parser Env.Error FaktoryProducerPoolConfig
envFaktoryProducerPoolConfig = do
  poolSize <- Env.var Env.auto "FAKTORY_PRODUCER_POOL_SIZE" $ Env.def 10
  pure $
    defaultFaktoryProducerPoolConfig {faktoryProducerPoolConfigSize = poolSize}

type FaktoryProducerPool = Pool Faktory.Producer

class HasFaktoryProducerPool env where
  faktoryProducerPoolL :: Lens' env FaktoryProducerPool

instance HasFaktoryProducerPool site => HasFaktoryProducerPool (HandlerData child site) where
  faktoryProducerPoolL = envL . siteL . faktoryProducerPoolL

createFaktoryProducerPool
  :: Faktory.Settings -> FaktoryProducerPoolConfig -> IO FaktoryProducerPool
createFaktoryProducerPool faktorySettings poolConfig =
  newPool
    $ setNumStripes
      (Just $ faktoryProducerPoolConfigStripes poolConfig)
    $ defaultPoolConfig
      (Faktory.newProducer faktorySettings)
      Faktory.closeProducer
      (realToFrac $ faktoryProducerPoolConfigIdleTimeout poolConfig)
      (faktoryProducerPoolConfigSize poolConfig)
