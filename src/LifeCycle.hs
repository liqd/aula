{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module LifeCycle
      -- * phase transition
    ( PhaseChange(..)
    , PhaseAction(..)
    , phaseTrans
    , freezePhase
    , thawPhase

      -- * capabilities
    , Capability(..)
    , CapCtx(..), capCtxUser, capCtxIdea, capCtxPhase, capCtxComment
    , capabilities
    )
where

import Control.Lens
import Data.Maybe
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


-- * Capabilities

-- | What a user can do with an idea.
--
-- The view of an idea is default and controlled by access control.
-- FIXME: clarify relationship of 'CanEditTopic' with 'CanMoveBetweenLocations' (in the types?)
data Capability
    -- Idea
    = CanView
    | CanLike
    | CanVote
    | CanComment
    | CanVoteComment
    | CanMarkFeasiblity  -- also can add jury statement
    | CanMarkWinner
    | CanAddCreatorStatement
    | CanEditCreatorStatement
    | CanEditAndDelete
    | CanMoveBetweenLocations
    -- Comment
    | CanReplyComment
    | CanDeleteComment
    | CanEditComment
    -- Topic
    | CanPhaseForwardTopic
    | CanPhaseBackwardTopic
    | CanViewTopic
    | CanEditTopic
    | CanCreateIdea
    -- User
    | CanCreateTopic
    | CanEditUser
  deriving (Enum, Bounded, Eq, Ord, Show, Read, Generic)

instance SOP.Generic Capability

-- TODO: the current context does not provide with the IdeaSpace, which seems
-- required to restrict students to their class.
data CapCtx = CapCtx
    { _capCtxUser    :: User
    , _capCtxPhase   :: Maybe Phase
    , _capCtxIdea    :: Maybe Idea
    , _capCtxComment :: Maybe Comment
    }
  deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''CapCtx

instance SOP.Generic CapCtx

capabilities :: CapCtx -> [Capability]
capabilities ctx = mconcat $
       [ userCapabilities r ]
    <> [ ideaCapabilities (u ^. _Id) r i p    | i <- l mi, p <- l mp ]
    <> [ commentCapabilities (u ^. _Id) r c p | c <- l mc, p <- l mp ]
    <> [ topicCapabilities p r                | p <- l mp ]
  where
    u  = ctx ^. capCtxUser
    r  = u ^. userRole
    mp = ctx ^. capCtxPhase
    mi = ctx ^. capCtxIdea
    mc = ctx ^. capCtxComment

    l = maybeToList


-- ** User capabilities

userCapabilities :: Role -> [Capability]
userCapabilities = \case
    Student    _clss -> [CanVote]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanCreateTopic, CanEditUser]
    Principal        -> []
    Admin            -> thereIsAGod []


-- * Idea Capabilities


ideaCapabilities :: AUID User -> Role -> Idea -> Phase -> [Capability]
ideaCapabilities uid r i p = CanView : phaseCap uid r i p

editCap :: AUID User -> Idea -> [Capability]
editCap uid i = [CanEditAndDelete | i ^. createdBy == uid]

allowedDuringFreeze :: [Capability]
allowedDuringFreeze = [ CanComment
                      , CanMarkFeasiblity
                      , CanAddCreatorStatement
                      , CanMarkWinner
                      ]

filterIfFrozen :: Phase -> [Capability] -> [Capability]
filterIfFrozen p | isPhaseFrozen p = filter (`elem` allowedDuringFreeze)
                 | otherwise       = id

phaseCap :: AUID User -> Role -> Idea -> Phase -> [Capability]
phaseCap u r i p = filterIfFrozen p $ case p of
    PhaseWildIdea{}   -> wildIdeaCap u i r
    PhaseRefinement{} -> phaseRefinementCap u i r
    PhaseJury         -> phaseJuryCap i r
    PhaseVoting{}     -> phaseVotingCap i r
    PhaseResult       -> phaseResultCap u i r

wildIdeaCap :: AUID User -> Idea -> Role -> [Capability]
wildIdeaCap u i = \case
    Student    _clss -> [CanLike, CanComment, CanVoteComment] <> editCap u i
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanEditAndDelete, CanComment, CanVoteComment, CanMoveBetweenLocations]
    Principal        -> []
    Admin            -> thereIsAGod []

phaseRefinementCap :: AUID User -> Idea -> Role -> [Capability]
phaseRefinementCap u i = \case
    Student    _clss -> [CanComment, CanVoteComment] <> editCap u i
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanEditAndDelete, CanComment, CanVoteComment, CanMoveBetweenLocations]
    Principal        -> []
    Admin            -> thereIsAGod []  -- FIXME: should be allowed to thaw; capture here when capabilities affect more than a couple of UI elements

phaseJuryCap :: Idea -> Role -> [Capability]
phaseJuryCap _i = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> [CanMarkFeasiblity]
    Admin            -> thereIsAGod []

phaseVotingCap :: Idea -> Role -> [Capability]
phaseVotingCap _i = \case
    Student    _clss -> [CanVote]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> thereIsAGod []

phaseResultCap :: AUID User -> Idea -> Role -> [Capability]
phaseResultCap u i = \case
    Student    _clss -> [CanAddCreatorStatement | u `isCreatorOf` i, isWinning i]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> mconcat [ [CanMarkWinner] <>
                                  [CanEditCreatorStatement | ideaHasCreatorStatement i]
                                  | isFeasibleIdea i ]
    Principal        -> []
    Admin            -> thereIsAGod []


-- *** Helpers

isCreatorOf :: HasMetaInfo a => AUID User -> a -> Bool
isCreatorOf u = (u ==) . view createdBy


-- ** Comment Capabilities

commentCapabilities :: AUID User -> Role -> Comment -> Phase -> [Capability]
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


-- ** Topic capabilities

topicCapabilities :: Phase -> Role -> [Capability]
topicCapabilities = (\f r -> CanViewTopic : f r) . \case
    p | isPhaseFrozen p -> const []
    PhaseWildIdea{}     -> topicWildIdeaCaps
    PhaseRefinement{}   -> topicRefinementCaps
    PhaseJury           -> topicJuryCaps
    PhaseVoting{}       -> topicVotingCaps
    PhaseResult         -> topicResultCaps

topicWildIdeaCaps :: Role -> [Capability]
topicWildIdeaCaps = \case
    Student    _clss -> [CanCreateIdea]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> thereIsAGod []

topicRefinementCaps :: Role -> [Capability]
topicRefinementCaps = \case
    Student    _clss -> [CanCreateIdea]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanEditTopic, CanPhaseForwardTopic]
    Principal        -> []
    Admin            -> thereIsAGod [CanPhaseForwardTopic]

topicJuryCaps :: Role -> [Capability]
topicJuryCaps = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> thereIsAGod [CanPhaseForwardTopic, CanPhaseBackwardTopic]

topicVotingCaps :: Role -> [Capability]
topicVotingCaps = \case
    Student    _clss -> [CanVote]
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanPhaseForwardTopic]
    Principal        -> []
    Admin            -> thereIsAGod [CanPhaseForwardTopic, CanPhaseBackwardTopic]

topicResultCaps :: Role -> [Capability]
topicResultCaps = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> []
    Principal        -> []
    Admin            -> thereIsAGod [CanPhaseBackwardTopic]
