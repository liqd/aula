{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module AulaTests.Stories.Interpreter.Action where

import Control.Lens
import Control.Monad (join, unless)
import Control.Monad.Free
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.List (find)
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
        u <- amquery $ findUserByLogin l
        Action.login (u ^. _Id)
        u' <- currentUser
        assert (u, u') (u == u')
        return $ csUser .= Just u'
    runClient k

runClient (Free (Logout k)) = do
    lift $ do
        Action.logout
    runClient k

runClient (Free (SelectIdeaSpace s k)) = do
    let (Right i :: Either String IdeaSpace) = parseIdeaSpace s
    found <- fmap (elem i) . lift $ aquery getSpaces
    unless found . error $ "No idea space is found" <> cs s
    csIdeaSpace .= Just i
    runClient k

runClient (Free (CreateIdea t d c k)) = do
    Just i <- use csIdeaSpace
    _ <- lift $ Action.createIdea
        (ProtoIdea t (Markdown d) c (IdeaLocationSpace i))
    runClient k

runClient (Free (LikeIdea t k)) = do
    Just idea <- findIdeaByTitle t
    _ <- lift $ Action.likeIdea (idea ^. _Id)
    runClient k

runClient (Free (CreateTopic it tt td k)) = do
    Just idea <- findIdeaByTitle it
    Just ideaSpace <- use csIdeaSpace
    _ <- lift $ do
        end <- aquery phaseEndRefinement
        Action.createTopic
            (ProtoTopic tt (Markdown td) "http://url.com" ideaSpace [idea ^. _Id] end)
    runClient k

-- FIXME: Handle Voting phase timeouts
runClient (Free (TimeoutTopic t k)) = do
    Just topic <- findTopicByTitle t
    _ <- lift $ Action.makeTopicTimeout (topic ^. _Id)
    runClient k

runClient (Free (MarkIdea t v k)) = do
    Just idea <- findIdeaByTitle t
    _ <- lift $ case v of
        Left v'  -> Action.markIdeaInJuryPhase (idea ^. _Id) v'
        Right v' -> Action.markIdeaInResultPhase (idea ^. _Id) v'
    runClient k

runClient (Free (VoteIdea t v k)) = do
    Just idea <- findIdeaByTitle t
    _ <- lift $ Action.voteIdea (idea ^. _Id) v
    runClient k

-- * helpers

findIdeaByTitle :: (ActionM m) => IdeaTitle -> StateT ClientState m (Maybe Idea)
findIdeaByTitle t = fmap (find ((t ==) . view ideaTitle)) . lift $ aquery getIdeas

findTopicByTitle :: (ActionM m) => IdeaTitle -> StateT ClientState m (Maybe Topic)
findTopicByTitle t = fmap (find ((t ==) . view topicTitle)) . lift $ aquery getTopics

assert :: (Show msg, ActionM m) => msg -> Bool -> m ()
assert _ True  = return ()
assert msg False = error $ "assertion failed: " <> show msg
    -- FIXME: give source code location of the call.
