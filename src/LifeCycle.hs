{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns   #-}

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
  deriving (Eq, Show)

data PhaseAction
    = JuryPhasePrincipalEmail
    | ResultPhaseModeratorEmail
    | UnmarkAllIdeas
    -- FIXME: Add more action here.
  deriving (Eq, Show)


phaseTrans :: Phase -> PhaseChange -> Maybe (Phase, [PhaseAction])
phaseTrans (PhaseRefinement _) RefinementPhaseTimeOut
    = Just (PhaseJury, [JuryPhasePrincipalEmail])
phaseTrans (PhaseRefinement _) RefinementPhaseMarkedByModerator
    = Just (PhaseJury, [JuryPhasePrincipalEmail])
phaseTrans PhaseJury (AllIdeasAreMarked {_phaseChangeVotPhaseEnd})
    = Just (PhaseVoting _phaseChangeVotPhaseEnd, [])
phaseTrans (PhaseVoting _) VotingPhaseTimeOut
    = Just (PhaseResult, [ResultPhaseModeratorEmail])
phaseTrans (PhaseVoting _) VotingPhaseSetbackToJuryPhase
    = Just (PhaseJury, [UnmarkAllIdeas])
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

ideaCapabilities :: AUID User -> Role -> Idea -> Maybe Phase -> [IdeaCapability]
ideaCapabilities u r i mp =
       phaseCap u r i mp
    <> editCap u r i
    <> moveBetweenTopicsCap r

editCap :: AUID User -> Role -> Idea -> [IdeaCapability]
editCap uid r i = [CanEdit | r == Moderator || i ^. createdBy == uid]

moveBetweenTopicsCap :: Role -> [IdeaCapability]
moveBetweenTopicsCap r = [CanMoveBetweenTopics | r ==  Moderator]

phaseCap :: AUID User -> Role -> Idea -> Maybe Phase -> [IdeaCapability]
phaseCap _ r i Nothing  = wildIdeaCap i r
phaseCap u r i (Just p) = case p of
    PhaseRefinement _ -> phaseRefinementCap i r
    PhaseJury         -> phaseJuryCap i r
    PhaseVoting     _ -> phaseVotingCap i r
    PhaseResult       -> phaseResultCap u i r

wildIdeaCap :: Idea -> Role -> [IdeaCapability]
wildIdeaCap _i = \case
    Student    _clss -> [CanLike, CanComment, CanVoteComment]
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
  deriving (Enum, Eq, Ord, Show, Read, Generic)

instance SOP.Generic CommentCapability

canDeleteComment :: AUID User -> Role -> Comment -> Bool
canDeleteComment uid role comment = uid `isCreatorOf` comment || role == Moderator

commentCapabilities :: AUID User -> Role -> Comment -> [CommentCapability]
commentCapabilities uid role comment =
    [CanDeleteComment | canDeleteComment uid role comment ] <>
    [CanReplyComment  | not $ comment ^. commentDeleted   ]


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
