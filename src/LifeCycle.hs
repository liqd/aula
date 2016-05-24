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

      -- * Visibility
    , ItemVisibility(..)
    , ideaItemVisibility

      -- * capabilities
    , UserCapability(..)
    , userCapabilities
    , IdeaCapability(..)
    , ideaCapabilities
    , ideaCapabilitiesInAllPhase
    , CommentCapability(..)
    , commentCapabilities
    , TopicCapability(..)
    , topicCapabilities
    )
where

import Control.Lens
import Control.Monad (join)
import Data.Monoid
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP

import Types


-- | modify this function to determine whether the 'Admin' role is all-powerful (@isThere == True@)
-- or can only do things that 'Admin's need to do (@isThere == False@).
thereIsAGod :: (Bounded a, Enum a) => [a] -> [a]
thereIsAGod nope = if isThere then [minBound..] else nope
  where
    isThere = True


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
    = Just (PhaseJury, [JuryPhasePrincipalEmail])
phaseTrans PhaseJury (AllIdeasAreMarked {_phaseChangeTimeout})
    = Just (PhaseVoting (ActivePhase _phaseChangeTimeout), [])
phaseTrans (PhaseVoting ActivePhase{}) PhaseTimeout
    = Just (PhaseResult, [ResultPhaseModeratorEmail])
phaseTrans (PhaseJury) (RevertJuryPhaseToRefinement {_phaseChangeTimeout})
    = Just (PhaseRefinement (ActivePhase _phaseChangeTimeout), [])
phaseTrans (PhaseVoting ActivePhase{}) RevertVotingPhaseToJury
    = Just (PhaseJury, [])
phaseTrans (PhaseResult) (RevertResultPhaseToVoting {_phaseChangeTimeout})
    = Just (PhaseVoting (ActivePhase _phaseChangeTimeout), [])

-- Others considered invalid (throw an error later on).
phaseTrans _ _ = Nothing


-- * User capabilities

-- FIXME: Extend the list
data UserCapability
    = CanCreateTopic
    | CanEditUser
  deriving (Eq, Show, Enum, Bounded)


userCapabilities :: Role -> [UserCapability]
userCapabilities = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanCreateTopic, CanEditUser]
    Principal        -> []
    Admin            -> thereIsAGod []


-- * Idea Capabilities

-- | What a user can do with an idea.
--
-- The view of an idea is default and controlled by access control.
data IdeaCapability
    = CanLike
    | CanVoteIdea
    | CanComment
    | CanVoteComment
    | CanMarkFeasiblity -- also can add jury statement
    | CanMarkWinner
    | CanAddCreatorStatement
    | CanEditCreatorStatement
    | CanEditAndDelete
    | CanMoveBetweenTopics  -- also move between (and into and out of) topics
  deriving (Enum, Bounded, Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaCapability

data ItemVisibility
    = Active
    | NonActive
    | NonCapable
  deriving (Enum, Bounded, Eq, Ord, Show, Read, Generic)

ideaItemVisibility :: AUID User -> Role -> Idea -> Phase -> IdeaCapability -> ItemVisibility
ideaItemVisibility u r i p c
    | c `elem` ideaCapabilities u r i p         = Active
    | c `elem` ideaCapabilitiesInAllPhase u r i = NonActive
    | otherwise                                 = NonCapable

-- ASSUMPTION: The list of a phase should be part of the AllPhase list.
ideaCapabilitiesInAllPhase :: AUID User -> Role -> Idea -> [IdeaCapability]
ideaCapabilitiesInAllPhase u r i = join
    [ wildIdeaCap u i r
    , phaseRefinementCap u i r
    , phaseJuryCap i r
    , phaseVotingCap i r
    , phaseResultCap u i r
    ]

ideaCapabilities :: AUID User -> Role -> Idea -> Phase -> [IdeaCapability]
ideaCapabilities = phaseCap

editCap :: AUID User -> Idea -> [IdeaCapability]
editCap uid i = [CanEditAndDelete | i ^. createdBy == uid]

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
    PhaseWildIdea{}   -> wildIdeaCap u i r
    PhaseRefinement{} -> phaseRefinementCap u i r
    PhaseJury         -> phaseJuryCap i r
    PhaseVoting{}     -> phaseVotingCap i r
    PhaseResult       -> phaseResultCap u i r

wildIdeaCap :: AUID User -> Idea -> Role -> [IdeaCapability]
wildIdeaCap u i = \case
    Student    _clss -> [CanLike, CanComment, CanVoteComment, CanMoveBetweenTopics] <> editCap u i
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanEditAndDelete, CanComment, CanVoteComment, CanMoveBetweenTopics]
    Principal        -> []
    Admin            -> thereIsAGod []

phaseRefinementCap :: AUID User -> Idea -> Role -> [IdeaCapability]
phaseRefinementCap u i = \case
    Student    _clss -> [CanComment, CanVoteComment, CanMoveBetweenTopics] <> editCap u i
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanEditAndDelete, CanComment, CanVoteComment, CanMoveBetweenTopics]
    Principal        -> []
    Admin            -> thereIsAGod []  -- FIXME: should be allowed to thaw; capture here when capabilities affect more than a couple of UI elements

phaseJuryCap :: Idea -> Role -> [IdeaCapability]
phaseJuryCap _i = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> [CanMarkFeasiblity]
    Admin            -> thereIsAGod []

phaseVotingCap :: Idea -> Role -> [IdeaCapability]
phaseVotingCap i = \case
    Student    _clss -> [CanVoteIdea | isFeasibleIdea i]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> thereIsAGod []

phaseResultCap :: AUID User -> Idea -> Role -> [IdeaCapability]
phaseResultCap u i = \case
    Student    _clss -> [CanAddCreatorStatement | u `isCreatorOf` i, isWinning i]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> mconcat [ [CanMarkWinner] <>
                                  [CanEditCreatorStatement | ideaHasCreatorStatement i]
                                  | isFeasibleIdea i ]
    Principal        -> []
    Admin            -> thereIsAGod []


-- ** Helpers

isCreatorOf :: HasMetaInfo a => AUID User -> a -> Bool
isCreatorOf u = (u ==) . view createdBy


-- * Comment Capabilities

-- These capabilities are specific to a particular comment. Using IdeaCapability would
-- be too coarse and would not allow distinguish that authors can delete only their own
-- comments (we need the individual 'Comment' as a function argument for that).
data CommentCapability
    = CanReplyComment
      -- To reply to a comment you need both this capability and the 'CanComment' capability
      -- for the corresponding idea.
      -- FIXME: we shouldn't make a difference between the right to make a comment and the
      -- right to make a sub-comment.
    | CanDeleteComment
    | CanEditComment
  deriving (Enum, Eq, Ord, Show, Read, Generic)

instance SOP.Generic CommentCapability

commentCapabilities :: AUID User -> Role -> Comment -> Phase -> [CommentCapability]
commentCapabilities uid role comment phase
    | comment ^. commentDeleted = []
    | ongoingDebate phase = mconcat $
        [[CanReplyComment]] <>
        [[CanDeleteComment, CanEditComment] | uid `isCreatorOf` comment || role == Moderator]
    | otherwise = mconcat
        [[CanDeleteComment, CanEditComment] | role == Moderator]
  where
    ongoingDebate = \case
        PhaseWildIdea{}   -> True
        PhaseRefinement{} -> True
        _                 -> False


-- * Topic capabilities

-- FIXME: Extend the list
data TopicCapability
    = CanPhaseForwardTopic
    | CanPhaseBackwardTopic
    | CanEditTopic -- FIXME: Separate move ideas to topic and change title desc.
    | CanCreateIdea
    | CanVoteTopic  -- (name for symmetry with 'CanVoteIdea'; needed only for delegation here)
  deriving (Eq, Show, Enum, Bounded)

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
    Student    _clss -> [CanCreateIdea]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> thereIsAGod []

topicRefinementCaps :: Role -> [TopicCapability]
topicRefinementCaps = \case
    Student    _clss -> [CanCreateIdea]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanEditTopic, CanPhaseForwardTopic]
    Principal        -> []
    Admin            -> thereIsAGod [CanPhaseForwardTopic]

topicJuryCaps :: Role -> [TopicCapability]
topicJuryCaps = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> thereIsAGod [CanPhaseForwardTopic, CanPhaseBackwardTopic]

topicVotingCaps :: Role -> [TopicCapability]
topicVotingCaps = \case
    Student    _clss -> [CanVoteTopic]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanPhaseForwardTopic]
    Principal        -> []
    Admin            -> thereIsAGod [CanPhaseForwardTopic, CanPhaseBackwardTopic]

topicResultCaps :: Role -> [TopicCapability]
topicResultCaps = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> thereIsAGod [CanPhaseBackwardTopic]
