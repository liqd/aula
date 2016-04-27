{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.Idiom
where

import Control.Exception (assert)
import Control.Lens
import Control.Monad (unless)
import Data.Functor.Infix ((<$$>))
import Data.Time
import GHC.Generics (Generic)
import Servant.Missing (throwError500)

import qualified Data.Map as Map (size)
import qualified Generics.SOP as SOP

import LifeCycle
import Types
import Persistent.Pure


-- | Number of likes / number of voters >= gobally configured quorum.
ideaQuorumOk :: AUID Idea -> Query Bool
ideaQuorumOk iid = do
    Just idea <- findIdea iid -- FIXME: Not found
    numVoters <- length <$> getVotersForIdea idea
    let numVotes = Map.size (view ideaLikes idea)
    ((numVotes * 100 `div` numVoters) >=) <$> quorum idea

-- | Users can like an idea / vote on it iff they are students with access to the idea's space.
getVotersForIdea :: Idea -> Query [User]
getVotersForIdea idea = filter hasAccess <$> getActiveUsers
  where
    hasAccess u = case idea ^. ideaLocation . ideaLocationSpace of
        SchoolSpace   -> isStudent u
        ClassSpace cl -> u `isStudentInClass` cl

    isStudent (view userRole -> (Student _)) = True
    isStudent _                              = False

    isStudentInClass (view userRole -> (Student cl')) cl = cl' == cl
    isStudentInClass _ _ = False


-- | @_listInfoForIdeaQuorum@ is the number of likes (quorum votes) needed for the quorum to be
-- reached.
data ListInfoForIdea = ListInfoForIdea
    { _listInfoForIdeaIt         :: Idea
    , _listInfoForIdeaPhase      :: Phase
    , _listInfoForIdeaQuorum     :: Int
    , _listInfoForIdeaNoOfVoters :: Int
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic ListInfoForIdea

getListInfoForIdea :: Idea -> EQuery ListInfoForIdea
getListInfoForIdea idea = do
    vs <- getVotersForIdea idea
    quPercent <- quorum idea
    let quVotesRequired = length vs * quPercent `div` 100
    phase :: Phase
        <- maybe404 =<< case idea ^. ideaMaybeTopicId of
            Nothing -> do
                dbFrozen <- view dbFreeze
                return . Just $ if dbFrozen == Frozen
                                then PhaseWildFrozen
                                else PhaseWildIdea
            Just tid -> view topicPhase <$$> findTopic tid
    voters <- length <$> getVotersForIdea idea
    pure $ ListInfoForIdea idea phase quVotesRequired voters

-- | Calculate the quorum for a given idea.
quorum :: Idea -> Query Percent
quorum idea = case idea ^. ideaLocation . ideaLocationSpace of
    SchoolSpace  -> view dbSchoolQuorum
    ClassSpace _ -> view dbClassQuorum

-- | Return the current system time with the day set to the date on which phases opened
-- today end.  When running the phase change trigger at midnight, find all dates that lie in the
-- past.
phaseEnd :: Timestamp -> DurationDays -> Query Timestamp
phaseEnd (Timestamp now) (DurationDays days) = do
    let day' :: Integer = toModifiedJulianDay (utctDay now) + fromIntegral days
    return . Timestamp $ now { utctDay = ModifiedJulianDay day' }

phaseEndRefinement :: Timestamp -> Query Timestamp
phaseEndRefinement now = view dbElaborationDuration >>= phaseEnd now

phaseEndVote :: Timestamp -> Query Timestamp
phaseEndVote now = view dbVoteDuration >>= phaseEnd now


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
    isMarkedIdea i = case i ^? ideaJuryResult . _Just . ideaJuryResultValue of
        Just (NotFeasible _) -> True
        Just (Feasible _)    -> True
        _                    -> False

deactivateUser :: AUID User -> AUpdate ()
deactivateUser uid
    = modifyUser uid (userSettings . userSettingsPassword .~ UserPassDeactivated)

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
          NotFrozen -> PhaseThaw   now
          Frozen    -> PhaseFreeze now
      freezeOrNot topic = do
          case phaseTrans (topic ^. topicPhase) change of
            Nothing -> return ()  -- ignored, probably repeated freeze or thaw
            Just (phase', actions) -> do
                let !_A = assert (null actions) ()  -- wrong monad to execute'em
                setTopicPhase (topic ^. _Id) phase'
  topics <- liftAQuery getTopics
  mapM_ freezeOrNot topics
