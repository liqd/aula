{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module AulaTests.Stories.Interpreter.Action
    ( run
    )
where

import Control.Lens
import Control.Monad (join, unless)
import Control.Monad.Free
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.List (find)
import qualified Data.Map as Map (size)
import Data.String.Conversions

import Action
import Persistent
import Types
import Frontend.Testing as Action (makeTopicTimeout)

import AulaTests.Stories.DSL

-- | Client state stores information about the assumptions
-- of the state of server states, it is also can be used
-- to simulate web clients state.
data ClientState = ClientState {
      _csIdeaSpace :: Maybe IdeaSpace
    , _csUser      :: Maybe User
    }
  deriving (Eq, Show)

initialClientState :: ClientState
initialClientState = ClientState Nothing Nothing

makeLenses ''ClientState

run :: (ActionM m) => Behavior a -> m a
run = fmap fst . flip runStateT initialClientState . runClient

-- FIXME: Check pre and post conditions
runClient :: (ActionM m) => Behavior a -> StateT ClientState m a
runClient (Pure r) = pure r

runClient (Free (Login l k)) = do
    join . lift $ do
        u <- mquery $ findUserByLogin l
        step $ Action.login (u ^. _Id)
        postcondition $ do
            u' <- currentUser
            assert (u, u') (u == u')
            return $ csUser .= Just u'
    runClient k

runClient (Free (Logout k)) = do
    precondition . lift $ do
        l <- isLoggedIn
        l `shouldBe` True
    step $ lift Action.logout
    postcondition . lift $ do
        l <- isLoggedIn
        l `shouldBe` False
    runClient k

runClient (Free (SelectIdeaSpace s k)) = do
    let (Right i :: Either String IdeaSpace) = parseIdeaSpace s
    found <- fmap (elem i) . lift $ query getSpaces
    unless found . error $ "No idea space is found" <> cs s
    csIdeaSpace .= Just i
    runClient k

runClient (Free (CreateIdea t d c k)) = do
    Nothing <- precondition $ findIdeaByTitle t
    Just i <- use csIdeaSpace
    _ <- step . lift $ Action.createIdea
        (ProtoIdea t (Markdown d) c (IdeaLocationSpace i))
    Just _idea <- postcondition $ findIdeaByTitle t
    runClient k

runClient (Free (LikeIdea t k)) = do
    Just idea <- precondition $ findIdeaByTitle t
    _ <- step . lift $ Action.likeIdea (idea ^. _Id)
    postcondition $ do
        Just idea' <- findIdeaByTitle t
        let noOfLikes  = Map.size $ idea  ^. ideaLikes
        let noOfLikes' = Map.size $ idea' ^. ideaLikes
        -- FIXME: The same user can like only once
        noOfLikes' `shouldBe` (noOfLikes + 1)
    runClient k

runClient (Free (CreateTopic it tt td k)) = do
    Just idea <- precondition $ findIdeaByTitle it
    Just ideaSpace <- use csIdeaSpace
    _ <- lift $ do
        end <- query phaseEndRefinement
        Action.createTopic
            (ProtoTopic tt (Markdown td) "http://url.com" ideaSpace [idea ^. _Id] end)
    postcondition $ return ()
    runClient k

runClient (Free (TimeoutTopic t k)) = do
    Just topic <- precondition $ findTopicByTitle t
    _ <- step . lift $ Action.makeTopicTimeout (topic ^. _Id)
    postcondition $ do
        Just topic' <- findTopicByTitle t
        let phase1 = topic ^. topicPhase
        let phase2 = topic' ^. topicPhase
        unless (phase1 `followingPhase` phase2) . fail $ show (phase1, phase2)
    runClient k

runClient (Free (MarkIdea t v k)) = do
    Just idea <- precondition $ findIdeaByTitle t
    _ <- step . lift $ case v of
        Left v'  -> Action.markIdeaInJuryPhase (idea ^. _Id) v'
        Right v' -> Action.markIdeaInResultPhase (idea ^. _Id) v'
    postcondition $ do
        Just idea' <- findIdeaByTitle t
        case v of
            Left  v' -> (idea' ^? ideaJuryResult . _Just . ideaJuryResultValue) `shouldBe` (Just v')
            Right v' -> (idea' ^? ideaVoteResult . _Just . ideaVoteResultValue) `shouldBe` (Just v')
    runClient k

runClient (Free (VoteIdea t v k)) = do
    Just idea <- precondition $ findIdeaByTitle t
    _ <- step . lift $ Action.voteIdea (idea ^. _Id) v
    postcondition $ do
        Just idea' <- findIdeaByTitle t
        let noOfVotes  = Map.size $ idea  ^. ideaVotes
        let noOfVotes' = Map.size $ idea' ^. ideaVotes
        -- FIXME: The same user can vote only once
        noOfVotes' `shouldBe` (noOfVotes + 1)
    runClient k

-- * helpers

findIdeaByTitle :: (ActionM m) => IdeaTitle -> StateT ClientState m (Maybe Idea)
findIdeaByTitle t = fmap (find ((t ==) . view ideaTitle)) . lift $ query getIdeas

findTopicByTitle :: (ActionM m) => IdeaTitle -> StateT ClientState m (Maybe Topic)
findTopicByTitle t = fmap (find ((t ==) . view topicTitle)) . lift $ query getTopics

assert :: (Show msg, Monad m) => msg -> Bool -> m ()
assert _ True  = return ()
assert msg False = error $ "assertion failed: " <> show msg
    -- FIXME: give source code location of the call.

shouldBe :: (Monad m, Eq a, Show a) => a -> a -> m ()
shouldBe actual expected = assert (actual, expected) (actual == expected)
    -- FIXME: give source code location of the call.

-- ** Denotations for test step sections

precondition :: Monad m => m a -> m a
precondition = id

step :: Monad m => m a -> m a
step = id

postcondition :: Monad m => m a -> m a
postcondition = id
