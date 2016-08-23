{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.Idiom
where

import Control.Lens
import Control.Monad (forM, unless)
import Data.Function (on)
import Data.Functor.Infix ((<$$>))
import Data.Maybe (catMaybes)
import Data.List (sortBy)
import GHC.Generics (Generic)
import Servant.Missing (throwError500)

import qualified Data.Map as Map
import qualified Data.Monoid
import qualified Data.Tree as Tree
import qualified Generics.SOP as SOP

import LifeCycle
import Types
import Persistent.Pure


-- | Users can like an idea / vote on it iff they are students with access to the idea's space.
getVotersForIdea :: Idea -> Query [User]
getVotersForIdea = getVotersForSpace . view (ideaLocation . ideaLocationSpace)

-- | Users can like an idea / vote on it iff they are students with access to the idea's space.
getVotersForSpace :: IdeaSpace -> Query [User]
getVotersForSpace space = filter hasAccess <$> getActiveUsers
  where
    hasAccess = case space of
        SchoolSpace   -> has $ userRoles . _Student
        ClassSpace cl -> cl & elemOf userSchoolClasses


-- | @_ideaStatsQuorum@ is the number of likes (quorum votes) needed for the quorum to be
-- reached.
data IdeaStats = IdeaStats
    { _ideaStatsIdea       :: Idea
    , _ideaStatsPhase      :: Phase
    , _ideaStatsQuorum     :: Int
    , _ideaStatsNoOfVoters :: Int
    }
  deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''IdeaStats

instance SOP.Generic IdeaStats

currentPhaseWildIdea :: EQuery Phase
currentPhaseWildIdea = views dbFreeze PhaseWildIdea

getIdeaStats :: Idea -> EQuery IdeaStats
getIdeaStats idea = do
    voters <- length <$> getVotersForIdea idea
    quPercent <- quorum idea
    let quVotesRequired = voters * quPercent `div` 100
    phase :: Phase
        <- case idea ^. ideaMaybeTopicId of
            Nothing -> currentPhaseWildIdea
            Just tid -> view topicPhase <$> (maybe404 =<< findTopic tid)
    pure $ IdeaStats idea phase quVotesRequired voters

ideaReachedQuorum :: IdeaStats -> Bool
ideaReachedQuorum i = reached >= needed
  where
    reached = numLikes $ _ideaStatsIdea i
    needed  = _ideaStatsQuorum i

quorumForSpace :: IdeaSpace -> Query Percent
quorumForSpace = \case
    SchoolSpace  -> view dbSchoolQuorum
    ClassSpace _ -> view dbClassQuorum

-- | Calculate the quorum for a given idea.
quorum :: Idea -> Query Percent
quorum = quorumForSpace . view (ideaLocation . ideaLocationSpace)

-- | Return the current system time with the day set to the date on which phases opened
-- today end.  When running the phase change trigger at midnight, find all dates that lie in the
-- past.
phaseEndDurationDays :: Timestamp -> DurationDays -> Timestamp
phaseEndDurationDays now (DurationDays days) =
    now & _Timestamp . _utctDay . julianDay +~ fromIntegral days

phaseEndRefinement :: Timestamp -> Query Timestamp
phaseEndRefinement = views dbElaborationDuration . phaseEndDurationDays

phaseEndVote :: Timestamp -> Query Timestamp
phaseEndVote = views dbVoteDuration . phaseEndDurationDays


-- | Returns the Just topic of an idea if the idea is assocaited with a topic, Nothing
-- if the idea is a wild idea, or throws an error if the topic is missing.
ideaTopic :: Idea -> MQuery Topic
ideaTopic idea = case idea ^. ideaLocation of
    IdeaLocationSpace _ ->
        pure Nothing
    IdeaLocationTopic _ topicId -> do
        -- (failure to match the following can only be caused by an inconsistent state)
        Just topic <- findTopic topicId
        pure $ Just topic

ideaPhase :: Idea -> MQuery Phase
ideaPhase = (view topicPhase <$$>) . ideaTopic

-- | FIXME:
--
-- 1. this function is only called with predicates of the form @(PhaseConstructor ==)@, so the error
--    message could be much more helpful if we used a type @Phase@ instead of @Phase -> Bool@ as a
--    check.
-- 2. naming convention (needs to be introduced in coding style): @check...@ does not crash if
--    something goes wrong.  use @assert...@ for that.
-- 3. the calls to this function have been witnessed to crash on demo systems, most likely due to
--    the fact that admin-as-god can see buttons that trigger this.  this is nothing to worry about
--    too much, but it's quite ugly conceptually.  admin-as-god is ugly.
checkInPhase :: (Phase -> Bool) -> Idea -> Topic -> EQuery ()
checkInPhase isPhase idea topic =
    unless (isPhase phase) $ throwError500 msg
  where
    phase = topic ^. topicPhase
    msg = unwords
        [ "Idea", show (idea ^. _Id), "is not in the correct phase."
        , "Current phase:", show phase
        ]

-- | Checks if all ideas associated with the topic are marked, feasible or not feasible.
checkAllIdeasMarked :: Topic -> Query Bool
checkAllIdeasMarked topic = all isMarkedIdea <$> findIdeasByTopic topic
  where
    isMarkedIdea = has $ ideaJuryResult . _Just

deactivateUser :: AUID User -> AUpdate ()
deactivateUser uid = withUser uid . userSettings . userSettingsPassword .= UserPassDeactivated

getUserViews :: Query [UserView]
getUserViews = makeUserView <$$> getAllUsers

findActiveUser :: AUID User -> MQuery User
findActiveUser uid = (((^? activeUser) . makeUserView) =<<) <$> findUser uid

-- This operation is idempotent, that is, freezing a frozen state
-- has no effect and similarly for thawing. Otherwise we'd need to make
-- sure the form treats vacuus setting of the freeze state as invalid.
--
-- Note that this operation is atomic, thus ensuring that all topics
-- are frozen or all are not frozen. Problem could arise, e.g., when
-- two admins concurrently freeze and thaw, with different speeds.
saveAndEnactFreeze :: Timestamp -> Freeze -> AUpdate ()
saveAndEnactFreeze now shouldBeFrozenOrNot = do
  dbFreeze .= shouldBeFrozenOrNot
  let change = case shouldBeFrozenOrNot of
          NotFrozen -> thawPhase
          Frozen    -> freezePhase
      freezeOrNot topic = setTopicPhase (topic ^. _Id) . change now $ topic ^. topicPhase
  topics <- liftAQuery getTopics
  mapM_ freezeOrNot topics


-- * voting logic

countVotes :: Getting Data.Monoid.Any IdeaVoteValue a -> Idea -> Int
countVotes v = length . filter (has v) . map (view ideaVoteValue) . Map.elems . view ideaVotes

-- | If an idea is accepted, it has a chance at getting implemented.  The distinction between
-- "accepted" (automatically determined) and "winning" (decided by moderator) is necessary because
-- two accepted ideas may be contradictory.  Identifying such contradictions requires AI and natural
-- language skills beyond those of the aula system.
--
-- We have discussed two alternative quorum rules, and both are implemented in the code for the
-- record (one dead, one live).  The live one checks that there are more yesses than nos, and that
-- the sum of both has reached the quorum percentage.
ideaAccepted :: IdeaStats -> Bool
ideaAccepted (IdeaStats idea _ _ numVoters) = _ideaAcceptedByMajority
  where
    _ideaAcceptedByMajority = nyes > nno && ntotal >= quo
    _ideaAcceptedByQuorum   = nyes >= quo && nno < quo

    quo    = numVoters `div` 3
    nyes   = countVotes _Yes idea
    nno    = countVotes _No idea
    ntotal = nyes + nno

-- | An un-normalized number for the popularity of an idea.  Can be an arbitrary integer, but higher
-- always means more popular.  This is the number by which feasible ideas are ordered in the result
-- phase.
newtype Support = Support Int
  deriving (Eq, Ord, Show, Read)

ideaSupport :: Phase -> Idea -> Support
ideaSupport = \case
    PhaseWildIdea{}   -> ideaLikeSupport
    PhaseRefinement{} -> ideaLikeSupport
    PhaseJury{}       -> ideaLikeSupport
    PhaseVoting{}     -> ideaVoteSupport
    PhaseResult{}     -> ideaVoteSupport

ideaLikeSupport :: Idea -> Support
ideaLikeSupport = Support . numLikes

ideaVoteSupport :: Idea -> Support
ideaVoteSupport = ideaVoteSupportByAbsDiff

ideaVoteSupportByAbsDiff :: Idea -> Support
ideaVoteSupportByAbsDiff idea = Support $ countVotes _Yes idea - countVotes _No idea


-- * voting

-- | Find the delegatees of the given user for the given scope
findDelegatees :: AUID User -> DScope -> EQuery [User]
findDelegatees uid scope = do
    delegateesInScope uid scope
    >>= mapM (findUser . view delegationFrom)
    >>= pure . catMaybes


-- | Some delegates and their direct delegatees.  Used in delegation tabs both in topic and in user
-- profile (see 'topicDelegateeLists', 'userDelegateeLists', resp.).  In topic, it contains all
-- delegates with their direct delegatees in the topic's 'DScope'; in the user profile, it contains
-- all direct delegatees of the user and *their* direct delegatees.
newtype DelegateeLists = DelegateeLists { unDelegateeLists :: [(User, [User])] }
  deriving (Eq, Show, Read)

-- | Collects some delegation scopes with the DelegateeLists, in some cases, the
-- first level users in DelegateeLists are delegates sometimes delegatees.
newtype DelegationListsMap = DelegationListsMap { unDelegationListsMap :: [(DScopeFull, DelegateeLists)] }
  deriving (Eq, Show, Read)

-- | 'DelegationLists' should be ordered by power and, if first argument is 'True', omit delegates
-- with no delegatees.
delegateeLists :: Bool -> [(User, [User])] -> DelegateeLists
delegateeLists omitEmpty = DelegateeLists . s . f
  where
    s = sortBy (flip compare `on` (length . snd))
    f = if omitEmpty then filter (not . null . snd) else id

-- | Call 'userDelegateeLists' for all 'DScopes' the user is involved with.
userDelegationListsMap :: AUID User -> EQuery DelegationListsMap
userDelegationListsMap uid = do
    let runScope dscope = (dscope,) <$> userDelegateeLists uid (fullDScopeToDScope dscope)
    dscopes <- delegationScopeForest =<< maybe404 =<< findUser uid
    DelegationListsMap <$> runScope `mapM` concatMap Tree.flatten dscopes

-- | On first level returns the delegates for the scopes
-- applicable to the user; on the second level the users who
-- delegated their votes to the first level user on the given scope.
--
-- u0 -> (s1,u1) <- [u3,u4,u5,u0]
--    -> (s2,u2) <- [u3,u6,u8,u0]
userDelegateListsMap :: AUID User -> EQuery DelegationListsMap
userDelegateListsMap delegatee = do
    ds <- delegates delegatee
    let runScope (Delegation dscope _delegatee delegate) = do
            dsFull <- dscopeFull dscope
            firstLevelUser <- maybe404 =<< findUser delegate
            secondLevelUsers <- findDelegatees (firstLevelUser ^. _Id) dscope
            pure (dsFull, delegateeLists True [(firstLevelUser, secondLevelUsers)])
    DelegationListsMap <$> mapM runScope ds

-- | Delegation tree for the given user and scope.
-- The first level contains all the delegatees of the given user
userDelegateeLists :: AUID User -> DScope -> EQuery DelegateeLists
userDelegateeLists uid scope = do
    firstLevelDelegatees <- findDelegatees uid scope
    delegateeLists False
        <$> forM firstLevelDelegatees
                (\user -> (,) user <$> findDelegatees (user ^. _Id) scope)

-- | Delegation tree for the given scope, the first level contains
-- all the users who can vote in the given topic.
topicDelegateeLists :: AUID Topic -> EQuery DelegateeLists
topicDelegateeLists topicId = do
    let scope = DScopeTopicId topicId
    topic <- maybe404 =<< findTopic topicId
    voters <- getVotersForSpace (topic ^. topicIdeaSpace)
    delegateeLists True <$> forM voters (\user ->
                            (,) user <$> findDelegatees (user ^. _Id) scope)

getVote :: AUID User -> AUID Idea -> EQuery (Maybe (User, IdeaVoteValue))
getVote uid iid = do
    idea  <- maybe404 =<< findIdea iid
    let mVoteValue = idea ^? ideaVotes . at uid . _Just
    case mVoteValue of
        Nothing -> pure Nothing
        Just vv -> do
            voter <- maybe404 =<< findUser (vv ^. ideaVoteDelegate)
            pure $ Just (voter, vv ^. ideaVoteValue)

getLike :: AUID User -> AUID Idea -> EQuery (Maybe (User, IdeaLike))
getLike uid iid = do
    idea <- maybe404 =<< findIdea iid
    let mLike = idea ^? ideaLikes . at uid . _Just
    case mLike of
        Nothing -> pure Nothing
        Just vl -> do
            liker <- maybe404 =<< findUser (vl ^. ideaLikeDelegate)
            pure $ Just (liker, vl)

studentsInIdeaSpace :: IdeaSpace -> EQuery [User]
studentsInIdeaSpace spc = fltr <$> cllct spc
  where
    fltr :: [User] -> [User]
    fltr = filter (has $ userRoles . _Student)

    cllct :: IdeaSpace -> EQuery [User]
    cllct = \case
        SchoolSpace    -> getActiveUsers
        ClassSpace cls -> getUsersInClass cls

studentsInDScope :: DScope -> EQuery [User]
studentsInDScope (DScopeIdeaSpace spc)
    = studentsInIdeaSpace spc
studentsInDScope (DScopeTopicId tid)
    = findTopic tid >>= maybe404 >>=
      studentsInIdeaSpace . view topicIdeaSpace
