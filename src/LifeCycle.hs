{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module LifeCycle
      -- * phase transition
    ( PhaseChange(..)
    , PhaseAction(..)
    , phaseTrans

      -- * capabilities
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
    | RefinementPhaseMarkedByModerator
    | AllIdeasAreMarked { _phaseChangeVotPhaseEnd :: Timestamp }
    | VotingPhaseTimeOut
    | VotingPhaseSetbackToJuryPhase
    | PhaseFreeze { _phaseChangeFreezeNow :: Timestamp }
    | PhaseThaw { _phaseChangeThawNow :: Timestamp }
  deriving (Eq, Show)

data PhaseAction
    = JuryPhasePrincipalEmail
    | ResultPhaseModeratorEmail
    | UnmarkAllIdeas
    -- FIXME: Add more action here.
  deriving (Eq, Show)


phaseTrans :: Phase -> PhaseChange -> Maybe (Phase, [PhaseAction])
phaseTrans PhaseRefinement{} RefinementPhaseTimeOut
    = Just (PhaseJury, [JuryPhasePrincipalEmail])
phaseTrans PhaseRefinement{} RefinementPhaseMarkedByModerator
    = Just (PhaseJury, [JuryPhasePrincipalEmail])
phaseTrans PhaseJury (AllIdeasAreMarked {_phaseChangeVotPhaseEnd})
    = Just (PhaseVoting _phaseChangeVotPhaseEnd, [])
phaseTrans PhaseVoting{} VotingPhaseTimeOut
    = Just (PhaseResult, [ResultPhaseModeratorEmail])
phaseTrans PhaseVoting{} VotingPhaseSetbackToJuryPhase
    = Just (PhaseJury, [UnmarkAllIdeas])
-- Freezing and thawing.
--
-- There are no frozen variants of @PhaseJury@ and @PhaseResult@.
-- Freezing or thawing those phases has no effect.  (We do not throw
-- an exception for these because that would require to handle this
-- case in other places where it is less convenient, I think.)
phaseTrans PhaseRefinement{_refPhaseEnd} (PhaseFreeze now)
    = Just (PhaseRefFrozen {_refPhaseLeftover = _refPhaseEnd `diffTimestamps` now}, [])
phaseTrans PhaseRefFrozen{_refPhaseLeftover} (PhaseThaw now)
    = Just (PhaseRefinement {_refPhaseEnd = _refPhaseLeftover `addTimespan` now}, [])
phaseTrans PhaseJury PhaseFreeze{} = Just (PhaseJury, [])
phaseTrans PhaseJury PhaseThaw{} = Just (PhaseJury, [])
phaseTrans PhaseVoting{_votPhaseEnd} (PhaseFreeze now)
    = Just (PhaseVotFrozen {_votPhaseLeftover = _votPhaseEnd `diffTimestamps` now}, [])
phaseTrans PhaseVotFrozen{_votPhaseLeftover} (PhaseThaw now)
    = Just (PhaseVoting {_votPhaseEnd = _votPhaseLeftover `addTimespan` now}, [])
phaseTrans PhaseResult PhaseFreeze{} = Just (PhaseResult, [])
phaseTrans PhaseResult PhaseThaw{} = Just (PhaseResult, [])
-- Others considered invalid (throw an error later on).
phaseTrans _ _ = Nothing


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
    | CanEdit
    | CanMoveBetweenTopics  -- also move between (and into and out of) topics
  deriving (Enum, Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaCapability

ideaCapabilities :: AUID User -> Role -> Idea -> Phase -> [IdeaCapability]
ideaCapabilities u r i p =
       phaseCap u r i p
    <> editCap u r i
    <> moveBetweenTopicsCap r

editCap :: AUID User -> Role -> Idea -> [IdeaCapability]
editCap uid r i = [CanEdit | r == Moderator || i ^. createdBy == uid]

moveBetweenTopicsCap :: Role -> [IdeaCapability]
moveBetweenTopicsCap r = [CanMoveBetweenTopics | r ==  Moderator]

phaseCap :: AUID User -> Role -> Idea -> Phase -> [IdeaCapability]
phaseCap u r i p = case p of
    PhaseWildIdea     -> wildIdeaCap i r
    PhaseWildFrozen   -> wildFrozenCap i r
    PhaseRefinement{} -> phaseRefinementCap i r
    PhaseRefFrozen{}  -> phaseRefFrozenCap i r
    PhaseJury         -> phaseJuryCap i r
    PhaseVoting{}     -> phaseVotingCap i r
    PhaseVotFrozen{}  -> phaseVotFrozenCap i r
    PhaseResult       -> phaseResultCap u i r

wildIdeaCap :: Idea -> Role -> [IdeaCapability]
wildIdeaCap _i = \case
    Student    _clss -> [CanLike, CanComment, CanVoteComment]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> []

wildFrozenCap :: Idea -> Role -> [IdeaCapability]
wildFrozenCap _i = \case
    Student    _clss -> [CanComment]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> []

phaseRefinementCap :: Idea -> Role -> [IdeaCapability]
phaseRefinementCap _i = \case
    Student    _clss -> [CanComment, CanVoteComment]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> []

phaseRefFrozenCap :: Idea -> Role -> [IdeaCapability]
phaseRefFrozenCap _i = \case
    Student    _clss -> [CanComment]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
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
    Student    _clss -> onFeasibleIdea i [CanVote]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> []

phaseVotFrozenCap :: Idea -> Role -> [IdeaCapability]
phaseVotFrozenCap _i = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> []

phaseResultCap :: AUID User -> Idea -> Role -> [IdeaCapability]
phaseResultCap u i = \case
    Student    _clss -> [CanAddCreatorStatement | u `isCreatorOf` i]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> onFeasibleIdea i [CanMarkWinner]
    Principal        -> []
    Admin            -> []


-- ** Helpers

onFeasibleIdea :: Idea -> [IdeaCapability] -> [IdeaCapability]
onFeasibleIdea i cs = if isFeasibleIdea i then cs else []

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
  deriving (Eq, Show)

topicCapabilities :: Role -> Phase -> [TopicCapability]
topicCapabilities Admin (PhaseRefinement _) = [CanPhaseForwardTopic]
topicCapabilities Admin PhaseJury           = [CanPhaseForwardTopic]
topicCapabilities Admin (PhaseVoting _)     = [CanPhaseForwardTopic, CanPhaseBackwardTopic]
topicCapabilities Admin PhaseResult         = []
topicCapabilities _     _                   = []
