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
import Control.Monad (filterM, forM, unless)
import Data.Functor.Infix ((<$$>))
import Data.List (find)
import Data.Maybe (catMaybes, mapMaybe)
import GHC.Generics (Generic)
import Servant.Missing (throwError500)

import qualified Data.Map as Map
import qualified Data.Monoid
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
    hasAccess u = case space of
        SchoolSpace   -> isStudent u
        ClassSpace cl -> u `isStudentInClass` cl

    isStudent (view userRole -> (Student _)) = True
    isStudent _                              = False

    isStudentInClass (view userRole -> (Student cl')) cl = cl' == cl
    isStudentInClass _ _ = False


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

getIdeaStats :: Idea -> EQuery IdeaStats
getIdeaStats idea = do
    voters <- length <$> getVotersForIdea idea
    quPercent <- quorum idea
    let quVotesRequired = voters * quPercent `div` 100
    phase :: Phase
        <- maybe404 =<< case idea ^. ideaMaybeTopicId of
            Nothing -> views dbFreeze (Just . PhaseWildIdea)
            Just tid -> view topicPhase <$$> findTopic tid
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
-- two accepted ideas may be contradictory.  Identifying such contradictions requires language
-- skills beyond those of the aula system.
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

delegateesOf :: User -> DelegationContext -> EQuery [User]
delegateesOf v ctx = do
    hiearchy <- scopeHiearchy ctx
    vs <- voters hiearchy vid
    catMaybes <$> forM vs findUser
  where
    vid = v ^. _Id
    voters (path :: [DelegationContext]) (w :: AUID User) = do
        -- ASSUMPTION: Only one delegate per topic
        -- TODO: Write comment
        -- HINT: I am not my own supporter.
        let supporter x = do
                delegations <- filter ((x ==) . view delegationFrom) <$> allDelegations
                pure $ case mapMaybe (\t -> find ((t ==) . view delegationContext) delegations) path of
                    []    -> False
                    d : _ -> (d ^. delegationTo) == vid

        ds  <- map (view delegationFrom) <$> scopeDelegatees w ctx
        sps <- filterM supporter ds
        vts <- mconcat <$> forM sps (voters path)
        pure (w:vts)

scopeDelegatees :: AUID User -> DelegationContext -> Query [Delegation]
scopeDelegatees uid ctx =
    filter ((&&) <$> ((uid ==) . view delegationTo)
                 <*> ((ctx ==) . view delegationContext))
    <$> allDelegations

getVote :: AUID User -> AUID Idea -> EQuery (Maybe (User, IdeaVoteValue))
getVote uid iid = do
    idea  <- maybe404 =<< findIdea iid
    let mVoteValue = idea ^? ideaVotes . at uid . _Just
    case mVoteValue of
        Nothing -> pure Nothing
        Just vv -> do
            voter <- maybe404 =<< findUser (vv ^. ideaVoteVoter)
            pure $ Just (voter, vv ^. ideaVoteValue)

scopeHiearchy :: DelegationContext -> EQuery [DelegationContext]
scopeHiearchy = \case
    DlgCtxGlobal           -> pure [DlgCtxGlobal]
    s@(DlgCtxIdeaSpace {}) -> pure [s, DlgCtxGlobal]
    t@(DlgCtxTopicId tid)  -> do
        space <- _topicIdeaSpace <$> (maybe404 =<< findTopic tid)
        (t:) <$> scopeHiearchy (DlgCtxIdeaSpace space)
    i@(DlgCtxIdeaId iid)     -> do
        loc <- _ideaLocation <$> (maybe404 =<< findIdea iid)
        (i:) <$> scopeHiearchy (case loc of
            IdeaLocationSpace s    -> DlgCtxIdeaSpace s
            IdeaLocationTopic _s t -> DlgCtxTopicId   t)

delegationContextFull :: DelegationContext -> EQuery DelegationContextFull
delegationContextFull = \case
    DlgCtxGlobal          -> pure DlgCtxGlobalFull
    DlgCtxIdeaSpace space -> pure $ DlgCtxIdeaSpaceFull space
    DlgCtxTopicId   tid   -> DlgCtxTopicFull <$> (maybe404 =<< findTopic tid)
    DlgCtxIdeaId    iid   -> DlgCtxIdeaFull <$> (maybe404 =<< findIdea iid)

-- FIXME: Display only students
usersForIdeaSpace :: IdeaSpace -> EQuery [User]
usersForIdeaSpace = \case
    SchoolSpace       -> getActiveUsers
    ClassSpace school -> getUsersInClass school
