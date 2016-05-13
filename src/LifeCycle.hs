{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module LifeCycle
      -- * phase transition
    ( PhaseChange(..)
    , PhaseAction(..)
    , phaseTrans
    , freezePhase
    , thawPhase

      -- * capabilities
    , UserCapability(..)
    , userCapabilities
    , IdeaCapability(..)
    , ideaCapabilities
    , CommentCapability(..)
    , commentCapabilities
    , TopicCapability(..)
    , topicCapabilities
    )
where

import Control.Lens
import Data.Monoid
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP

import Types


-- * Phase transition matrix

data PhaseChange
    = RefinementPhaseTimeOut
    | AllIdeasAreMarked { _phaseChangeVotPhaseEnd :: Timestamp }
    | VotingPhaseTimeOut
    | RevertJuryPhaseToRefinement
    | RevertVotingPhaseToJury
    | RevertResultPhaseToVoting
    | PhaseFreeze { _phaseChangeFreezeNow :: Timestamp }
    | PhaseThaw { _phaseChangeThawNow :: Timestamp }
  deriving (Eq, Show)

data PhaseAction
    = JuryPhasePrincipalEmail
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
phaseTrans (PhaseRefinement ActivePhase{}) RefinementPhaseTimeOut
    = Just (PhaseJury, [JuryPhasePrincipalEmail])
phaseTrans PhaseJury (AllIdeasAreMarked {_phaseChangeVotPhaseEnd})
    = Just (PhaseVoting (ActivePhase _phaseChangeVotPhaseEnd), [])
phaseTrans (PhaseVoting ActivePhase{}) VotingPhaseTimeOut
    = Just (PhaseResult, [ResultPhaseModeratorEmail])
phaseTrans (PhaseVoting ActivePhase{}) RevertJuryPhaseToRefinement
    = Just (PhaseJury, [])
phaseTrans (PhaseVoting ActivePhase{}) RevertVotingPhaseToJury
    = Just (PhaseJury, [])
phaseTrans (PhaseVoting ActivePhase{}) RevertResultPhaseToVoting
    = Just (PhaseJury, [])

-- Freezing and thawing.
--
-- There are no frozen variants of @PhaseJury@ and @PhaseResult@.
-- Freezing or thawing those phases has no effect.  (We do not throw
-- an exception for these because that would require to handle this
-- case in other places where it is less convenient, I think.)
phaseTrans phase (PhaseFreeze now)
    = Just (freezePhase now phase, [])
phaseTrans phase (PhaseThaw now)
    = Just (thawPhase now phase, [])

-- Others considered invalid (throw an error later on).
phaseTrans _ _ = Nothing


-- * User capabilities

-- FIXME: Extend the list
data UserCapability
    = CanCreateTopic
  deriving (Eq, Show)

userCapabilities :: IdeaSpace -> Role -> [UserCapability]
userCapabilities _s = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanCreateTopic]
    Principal        -> []
    Admin            -> []


-- * Idea Capabilities

-- | What a user can do with an idea.
--
-- The view of an idea is default and controlled by access control.
data IdeaCapability
    = CanLike
    | CanVote
    | CanComment
    | CanVoteComment
    | CanMarkFeasiblity -- also can add jury statement
    | CanMarkWinner
    | CanAddCreatorStatement
    | CanEditCreatorStatement
    | CanEdit
    | CanMoveBetweenTopics  -- also move between (and into and out of) topics
  deriving (Enum, Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaCapability

ideaCapabilities :: AUID User -> Role -> Idea -> Phase -> [IdeaCapability]
ideaCapabilities u r i p =
       phaseCap u r i p
    <> editCap u r i

editCap :: AUID User -> Role -> Idea -> [IdeaCapability]
editCap uid r i = [CanEdit | r == Moderator || i ^. createdBy == uid]

allowedDuringFreeze :: [IdeaCapability]
allowedDuringFreeze = [ CanComment
                      , CanMarkFeasiblity
                      , CanAddCreatorStatement
                      , CanMarkWinner
                      ]

filterIfFrozen :: Phase -> [IdeaCapability] -> [IdeaCapability]
filterIfFrozen p | isPhaseFrozen p = filter (`elem` allowedDuringFreeze)
                 | otherwise       = id

phaseCap :: AUID User -> Role -> Idea -> Phase -> [IdeaCapability]
phaseCap u r i p = filterIfFrozen p $ case p of
    PhaseWildIdea{}   -> wildIdeaCap i r
    PhaseRefinement{} -> phaseRefinementCap i r
    PhaseJury         -> phaseJuryCap i r
    PhaseVoting{}     -> phaseVotingCap i r
    PhaseResult       -> phaseResultCap u i r

wildIdeaCap :: Idea -> Role -> [IdeaCapability]
wildIdeaCap _i = \case
    Student    _clss -> [CanLike, CanComment, CanVoteComment, CanMoveBetweenTopics]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanMoveBetweenTopics]
    Principal        -> []
    Admin            -> []

phaseRefinementCap :: Idea -> Role -> [IdeaCapability]
phaseRefinementCap _i = \case
    Student    _clss -> [CanComment, CanVoteComment, CanMoveBetweenTopics]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanMoveBetweenTopics]
    Principal        -> []
    Admin            -> []  -- FIXME: should be allowed to thaw; capture here when capabilities affect more than a couple of UI elements

phaseJuryCap :: Idea -> Role -> [IdeaCapability]
phaseJuryCap _i = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> [CanMarkFeasiblity]
    Admin            -> []

phaseVotingCap :: Idea -> Role -> [IdeaCapability]
phaseVotingCap i = \case
    Student    _clss -> [CanVote | isFeasibleIdea i]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> []

phaseResultCap :: AUID User -> Idea -> Role -> [IdeaCapability]
phaseResultCap u i = \case
    Student    _clss -> [CanAddCreatorStatement | u `isCreatorOf` i, isWinning i]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> mconcat [ [CanMarkWinner] <>
                                  [CanEditCreatorStatement | ideaHasCreatorStatement i]
                                  | isFeasibleIdea i ]
    Principal        -> []
    Admin            -> []


-- ** Helpers

isCreatorOf :: HasMetaInfo a => AUID User -> a -> Bool
isCreatorOf u = (u ==) . view createdBy


-- * Comment Capabilities

-- These capabilities are specific to a particular comment. Using IdeaCapability would
-- be too coarse and would not allow distinguish that authors can delete only their own
-- comments.
data CommentCapability
    = CanReplyComment
      -- To reply to a comment you need both this capability and the MakeComment capability
      -- for the corresponding idea.
    | CanDeleteComment
    | CanEditComment
  deriving (Enum, Eq, Ord, Show, Read, Generic)

instance SOP.Generic CommentCapability

canDeleteComment :: AUID User -> Role -> Comment -> Bool
canDeleteComment uid role comment = uid `isCreatorOf` comment || role == Moderator

commentCapabilities :: AUID User -> Role -> Comment -> [CommentCapability]
commentCapabilities uid role comment
    | comment ^. commentDeleted = []
    | otherwise =
        [CanDeleteComment | canDeleteComment uid role comment] <>
        [CanReplyComment  ] <>
        [CanEditComment   | uid `isCreatorOf` comment]


-- * Topic capabilities

-- FIXME: Extend the list
data TopicCapability
    = CanPhaseForwardTopic
    | CanPhaseBackwardTopic
    | CanEditTopic -- FIXME: Separate move ideas to topic and change title desc.
  deriving (Eq, Show)

topicCapabilities :: Phase -> Role -> [TopicCapability]
topicCapabilities = \case
    p | isPhaseFrozen p -> const []
    PhaseWildIdea{}     -> topicWildIdeaCaps
    PhaseRefinement{}   -> topicRefinementCaps
    PhaseJury           -> topicJuryCaps
    PhaseVoting{}       -> topicVotingCaps
    PhaseResult         -> topicResultCaps

topicWildIdeaCaps :: Role -> [TopicCapability]
topicWildIdeaCaps = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> []

topicRefinementCaps :: Role -> [TopicCapability]
topicRefinementCaps = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanEditTopic]
    Principal        -> []
    Admin            -> [CanPhaseForwardTopic]

topicJuryCaps :: Role -> [TopicCapability]
topicJuryCaps = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> [CanPhaseForwardTopic]

topicVotingCaps :: Role -> [TopicCapability]
topicVotingCaps = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> [CanPhaseForwardTopic, CanPhaseBackwardTopic]

topicResultCaps :: Role -> [TopicCapability]
topicResultCaps = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> []
