{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.Idiom
where

import Control.Lens
import Control.Monad (forM, unless)
import Data.Functor.Infix ((<$$>))
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Servant.Missing (throwError500)

import qualified Data.Map as Map
import qualified Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
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
    reached = length . view ideaLikes $ _ideaStatsIdea i
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
ideaLikeSupport = Support . length . view ideaLikes

ideaVoteSupport :: Idea -> Support
ideaVoteSupport = ideaVoteSupportByAbsDiff

ideaVoteSupportByAbsDiff :: Idea -> Support
ideaVoteSupportByAbsDiff idea = Support $ countVotes _Yes idea - countVotes _No idea


-- * voting

votingPower :: AUID User -> DScope -> EQuery [User]
votingPower vid scope = do
    hiearchy <- scopeHiearchy scope
    vs <- Set.toList <$> voters hiearchy (Set.singleton vid) vid
    catMaybes <$> forM vs findUser
  where
    voters (path :: [DScope]) (discovered :: Set (AUID User)) (user :: AUID User) = do
        -- Set.fromList O(n*log n) is better than nub O(n^2)
        oneStepNewDelegatees
            <- (`Set.difference` discovered) . Set.fromList . fmap _delegationFrom . concat
                <$> forM path (scopeDelegatees user)
        allNewDelegatees <- Set.unions
            <$> forM (Set.toList oneStepNewDelegatees)
                     (voters path (oneStepNewDelegatees `Set.union` discovered))
        pure (discovered `Set.union` allNewDelegatees)

scopeDelegatees :: AUID User -> DScope -> Query [Delegation]
scopeDelegatees uid scope =
    filter ((&&) <$> ((uid ==) . view delegationTo)
                 <*> ((scope ==) . view delegationScope))
    <$> allDelegations

getVote :: AUID User -> AUID Idea -> EQuery (Maybe (User, IdeaVoteValue))
getVote uid iid = do
    idea  <- maybe404 =<< findIdea iid
    let mVoteValue = idea ^? ideaVotes . at uid . _Just
    case mVoteValue of
        Nothing -> pure Nothing
        Just vv -> do
            voter <- maybe404 =<< findUser (vv ^. ideaVoteDelegate)
            pure $ Just (voter, vv ^. ideaVoteValue)

scopeHiearchy :: DScope -> EQuery [DScope]
scopeHiearchy = \case
    DScopeGlobal           -> pure [DScopeGlobal]
    s@(DScopeIdeaSpace {}) -> pure [s, DScopeGlobal]
    t@(DScopeTopicId tid)  -> do
        space <- _topicIdeaSpace <$> (maybe404 =<< findTopic tid)
        (t:) <$> scopeHiearchy (DScopeIdeaSpace space)
    i@(DScopeIdeaId iid)     -> do
        loc <- _ideaLocation <$> (maybe404 =<< findIdea iid)
        (i:) <$> scopeHiearchy (case loc of
            IdeaLocationSpace s    -> DScopeIdeaSpace s
            IdeaLocationTopic _s t -> DScopeTopicId   t)

studentsInIdeaSpace :: IdeaSpace -> EQuery [User]
studentsInIdeaSpace spc = fltr <$> cllct spc
  where
    fltr :: [User] -> [User]
    fltr = filter (has $ userRoles . _Student)

    cllct :: IdeaSpace -> EQuery [User]
    cllct = \case
        SchoolSpace       -> getActiveUsers
        ClassSpace school -> getUsersInClass school

studentsInDScope :: DScope -> EQuery [User]
studentsInDScope DScopeGlobal
    = studentsInIdeaSpace SchoolSpace
studentsInDScope (DScopeIdeaSpace spc)
    = studentsInIdeaSpace spc
studentsInDScope (DScopeTopicId tid)
    = findTopic tid >>= maybe404 >>=
      studentsInIdeaSpace . view topicIdeaSpace
studentsInDScope (DScopeIdeaId iid)
    = findIdea iid >>= maybe404 >>=
      studentsInIdeaSpace . view (ideaLocation . ideaLocationSpace)
