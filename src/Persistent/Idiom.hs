{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.Idiom
where

import Control.Lens
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Data.Time

import qualified Data.Map as Map (size)

import Types
import Persistent.Api


-- | Number of likes / number of voters >= gobally configured quorum.
ideaQuorumOk :: PersistM m => AUID Idea -> m Bool
ideaQuorumOk iid = do
    Just idea <- findIdea iid -- FIXME: Not found
    numVoters <- length <$> getVotersForIdea idea
    let numVotes = Map.size (view ideaLikes idea)
    ((numVotes * 100 `div` numVoters) >=) <$> quorum idea

-- | Users can like an idea / vote on it iff they are students with access to the idea's space.
getVotersForIdea :: PersistM m => Idea -> m [User]
getVotersForIdea idea = filter hasAccess <$> getUsers
  where
    hasAccess u = case idea ^. ideaLocation . ideaLocationSpace of
        SchoolSpace   -> isStudent u
        ClassSpace cl -> u `isStudentInClass` cl

    isStudent (view userRole -> (Student _)) = True
    isStudent _                              = False

    isStudentInClass (view userRole -> (Student cl')) cl = cl' == cl
    isStudentInClass _ _ = False

-- | Users can like an idea / vote on it iff they are students with access to the idea's space.
getNumVotersForIdea :: PersistM m => Idea -> m (Idea, Int)
getNumVotersForIdea idea = (,) idea . length <$> getVotersForIdea idea

-- | Calculate the quorum for a given idea.
quorum :: PersistM m => Idea -> m Percent
quorum idea = case idea ^. ideaLocation . ideaLocationSpace of
    SchoolSpace  -> getDb dbSchoolQuorum
    ClassSpace _ -> getDb dbClassQuorum

-- | Return the current system time with the day set to the date on which phases opened
-- today end.  When running the phase change trigger at midnight, find all dates that lie in the
-- past.
phaseEnd :: PersistM m => Int -> m Timestamp
phaseEnd days = do
    Timestamp timestamp <- getCurrentTimestamp
    let day' :: Integer = toModifiedJulianDay (utctDay timestamp) + fromIntegral days
    return . Timestamp $ timestamp { utctDay = ModifiedJulianDay day' }

phaseEndRefinement :: PersistM m => m Timestamp
phaseEndRefinement = do
    DurationDays (days :: Int) <- getDb dbElaborationDuration
    phaseEnd days

phaseEndVote :: PersistM m => m Timestamp
phaseEndVote = do
    DurationDays (days :: Int) <- getDb dbVoteDuration
    phaseEnd days


-- | Returns the Just topic of an idea if the idea is assocaited with a topic, Nothing
-- if the idea is a wild idea, or throws an error if the topic is missing.
ideaTopic :: PersistM m => Idea -> m (Maybe Topic)
ideaTopic idea = case idea ^. ideaLocation of
    IdeaLocationSpace _ ->
        pure Nothing
    IdeaLocationTopic _ topicId -> do
        -- (failure to match the following can only be caused by an inconsistent state)
        Just topic <- findTopic topicId
        pure $ Just topic

ideaPhase :: PersistM m => Idea -> m (Maybe Phase)
ideaPhase = fmap (fmap (view topicPhase)) . ideaTopic

checkPhaseJury :: PersistM m => Topic -> m ()
checkPhaseJury topic =
    when (topic ^. topicPhase /= PhaseJury) . throwError $
        persistError "Idea is not in the jury phase"

-- | Checks if all ideas associated with the topic are marked, feasible or not feasible.
checkAllIdeasMarked :: PersistM m => Topic -> m Bool
checkAllIdeasMarked topic = all isMarkedIdea <$> findIdeasByTopic topic
  where
    -- FIXME: Better lens expression
    isMarkedIdea i = case fmap (view ideaResultValue) (view ideaResult i) of
        Just (NotFeasible _) -> True
        Just (Feasible _)    -> True
        _                    -> False
