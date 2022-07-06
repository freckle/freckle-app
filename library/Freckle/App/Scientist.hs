module Freckle.App.Scientist
  ( experimentPublishDatadog
  ) where

import Freckle.App.Prelude

import Freckle.App.Stats (HasStatsClient)
import qualified Freckle.App.Stats as Stats
import Scientist
import Scientist.Duration

-- | Publish experiment durations using "Freckle.App.Stats"
--
-- * Experiments are labeled "science.${name}"
-- * Results are tagged with "variant:${name}"
--
experimentPublishDatadog
  :: (MonadReader env m, MonadUnliftIO m, HasStatsClient env)
  => Result c a b
  -> m ()
experimentPublishDatadog result = for_ (resultDetails result) $ \details -> do
  let
    statName = "science." <> resultDetailsExperimentName details
    ResultControl {..} = resultDetailsControl details

  Stats.tagged [("variant", resultControlName)]
    $ Stats.gauge statName
    $ durationToSeconds resultControlDuration

  for_ (resultDetailsCandidates details) $ \ResultCandidate {..} ->
    Stats.tagged [("variant", "candidate-" <> resultCandidateName)]
      $ Stats.gauge statName
      $ durationToSeconds resultCandidateDuration
