module Freckle.App.Scientist
  ( experimentPublishDatadog
  ) where

import Freckle.App.Prelude

import Data.List.NonEmpty as NE
import qualified Freckle.App.Datadog as Datadog
import Scientist
import Scientist.Duration

-- | Publish experiment durations to Datadog
--
-- * Experiments are labeled "science.${name}"
-- * Control results are tagged with "variant:control"
-- * Candidates are tagged with "variant:control-${i}"
--
experimentPublishDatadog
  :: ( MonadReader env m
     , MonadUnliftIO m
     , Datadog.HasDogStatsClient env
     , Datadog.HasDogStatsTags env
     )
  => Result c a b
  -> m ()
experimentPublishDatadog result = for_ (resultDetails result) $ \details -> do
  let statName = "science." <> resultDetailsExperimentName details
  Datadog.gauge statName [("variant", "control")]
    $ durationToSeconds
    $ resultControlDuration
    $ resultDetailsControl details
  for_ (NE.zip (resultDetailsCandidates details) (NE.fromList [1 ..]))
    $ \(candidate, i :: Int) -> do
        Datadog.gauge statName [("variant", "candidate-" <> tshow i)]
          $ durationToSeconds
          $ resultCandidateDuration candidate
