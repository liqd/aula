{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE TypeFamilies    #-}

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
    , elemCaps
    , Clickable(..)
    , unClickable
    , eitherClickableGrayedOut

    , userCapabilities
    , ideaCapabilities
    , commentCapabilities
    , topicCapabilities
    )
where

import qualified Control.Exception
import           Control.Lens
import           Control.Monad (join)
import           Data.List ((\\), find, nub)
import           Data.Monoid
import qualified Generics.SOP as SOP
import           GHC.Generics (Generic)

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
    = CanLike
    | CanVote
    | CanComment
    | CanVoteComment
    | CanMarkFeasiblity -- also can add jury statement
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
    | CanEditTopic
    | CanCreateIdea
    -- User
    | CanCreateTopic
    | CanEditUser
  deriving (Enum, Bounded, Eq, Ord, Show, Read, Generic)

instance SOP.Generic Capability

elemCaps :: Capability -> [Clickable Capability] -> Bool
elemCaps c (find ((c ==) . _unClickable) -> Just _) = True
elemCaps _ _ = False


data Clickable a
    = Clickable { _unClickable :: a }
    | GrayedOut { _unClickable :: a }
  deriving (Eq, Functor, Ord, Read, Show, Generic)

instance SOP.Generic a => SOP.Generic (Clickable a)

makeLenses ''Clickable

eitherClickableGrayedOut :: (Eq cap, Monad m, trans ~ (m () -> m ()))
    => [Clickable cap] -> cap -> trans -> trans -> trans
eitherClickableGrayedOut caps cap clickable_ grayedout
    | cble && gout = Control.Exception.assert False $ error "clickable eliminator: internal error."
    | cble         = clickable_
    | gout         = grayedout
    | otherwise    = const $ pure ()
  where
    cble = Clickable cap `elem` caps
    gout = GrayedOut cap `elem` caps


-- ** User capabilities

userCapabilities :: Role -> [Capability]
userCapabilities = \case
    Student    _clss -> []
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanCreateTopic, CanEditUser]
    Principal        -> []
    Admin            -> thereIsAGod []


-- * Idea Capabilities

ideaCapabilities :: AUID User -> Role -> Idea -> Phase -> [Clickable Capability]
ideaCapabilities u r i p =
    let activeCaps = nub $ ideaCapabilitiesInPhase u r i p
        allCaps    = nub $ ideaCapabilitiesInAllPhases u r i
    in (Clickable <$> activeCaps) <> (GrayedOut <$> (allCaps \\ activeCaps))

-- | The union of the idea caps of all phases
ideaCapabilitiesInAllPhases :: AUID User -> Role -> Idea -> [Capability]
ideaCapabilitiesInAllPhases u r i = join
    [ wildIdeaCap u i r
    , phaseRefinementCap u i r
    , phaseJuryCap i r
    , phaseVotingCap i r
    , phaseResultCap u i r
    ]

ideaCapabilitiesInPhase :: AUID User -> Role -> Idea -> Phase -> [Capability]
ideaCapabilitiesInPhase = phaseCap

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
    Student    _clss -> [CanLike, CanComment, CanVoteComment, CanMoveBetweenLocations] <> editCap u i
    ClassGuest _clss -> []
    SchoolGuest      -> []
    Moderator        -> [CanEditAndDelete, CanComment, CanVoteComment, CanMoveBetweenLocations]
    Principal        -> []
    Admin            -> thereIsAGod []

phaseRefinementCap :: AUID User -> Idea -> Role -> [Capability]
phaseRefinementCap u i = \case
    Student    _clss -> [CanComment, CanVoteComment, CanMoveBetweenLocations] <> editCap u i
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
phaseVotingCap i = \case
    Student    _clss -> [CanVote | isFeasibleIdea i]
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
topicCapabilities = \case
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
