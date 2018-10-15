{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module LifeCycle
    ( -- * phase transition
      PhaseChange(..)
    , PhaseAction(..)
    , phaseTrans
    , freezePhase
    , thawPhase
    )
where

import Control.Lens

import Types



-- * Phase transition matrix

data PhaseChange
    = PhaseTimeout
    | AllIdeasAreMarked { _phaseChangeTimeout :: Timestamp }
    | RevertJuryPhaseToRefinement { _phaseChangeTimeout :: Timestamp }
    | RevertVotingPhaseToJury
    | RevertResultPhaseToVoting { _phaseChangeTimeout :: Timestamp }
  deriving (Eq, Show)

data PhaseAction
    = JuryPhasePrincipalEmail
    | JuryPhaseCircumvent
    | ResultPhaseModeratorEmail
    | UnmarkAllIdeas
    -- FIXME: Add more action here.
  deriving (Eq, Show)

freezePhase :: Timestamp -> Phase -> Phase
freezePhase now = (phaseStatus     %~ freezeStatus)
                . (phaseWildFrozen .~ Frozen)
  where
    freezeStatus = \case
        ActivePhase{_phaseEnd} -> FrozenPhase{_phaseLeftover = _phaseEnd `diffTimestamps` now}
        s                      -> s

thawPhase :: Timestamp -> Phase -> Phase
thawPhase now = (phaseStatus     %~ thawStatus)
              . (phaseWildFrozen .~ NotFrozen)
  where
    thawStatus = \case
        FrozenPhase{_phaseLeftover} -> ActivePhase{_phaseEnd = _phaseLeftover `addTimespan` now}
        s                           -> s

phaseTrans :: Phase -> PhaseChange -> Maybe (Phase, [PhaseAction])
phaseTrans (PhaseRefinement ActivePhase{}) PhaseTimeout
    = Just (PhaseJury, [JuryPhaseCircumvent])
phaseTrans PhaseJury AllIdeasAreMarked{_phaseChangeTimeout}
    = Just (PhaseVoting (ActivePhase _phaseChangeTimeout), [])
phaseTrans (PhaseVoting ActivePhase{}) PhaseTimeout
    = Just (PhaseResult, [ResultPhaseModeratorEmail])
phaseTrans PhaseJury RevertJuryPhaseToRefinement{_phaseChangeTimeout}
    = Just (PhaseRefinement (ActivePhase _phaseChangeTimeout), [])
phaseTrans (PhaseVoting ActivePhase{}) RevertVotingPhaseToJury
    = Just (PhaseJury, [])
phaseTrans PhaseResult RevertResultPhaseToVoting{_phaseChangeTimeout}
    = Just (PhaseVoting (ActivePhase _phaseChangeTimeout), [])

-- Others considered invalid (throw an error later on).
phaseTrans _ _ = Nothing
