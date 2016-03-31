{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.Idiom
where

import Control.Lens
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

-- | Return the current system time with the day set to the date on which refinement phases opened
-- today end.  When running the phase change trigger at midnight, find all dates that lie in the
-- past.
phaseEndRefinement :: PersistM m => m Timestamp
phaseEndRefinement = do
    Timestamp timestamp <- getCurrentTimestamp
    DurationDays (days :: Int) <- getDb dbElaborationDuration
    let day' :: Integer = toModifiedJulianDay (utctDay timestamp) + fromIntegral days
    return . Timestamp $ timestamp { utctDay = ModifiedJulianDay day' }
