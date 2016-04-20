{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
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

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad (join, unless)
import Control.Monad.Free
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.List (find)
import qualified Data.Map as Map (elems, size)
import Data.String.Conversions

import Action
import Persistent
import Types
import Frontend.Core
import qualified Frontend.Page as Page
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

type ActionClient m a = StateT ClientState m a

-- FIXME: Check pre and post conditions
-- FIXME: Find comment by part of its description
runClient :: (ActionM m) => Behavior a -> ActionClient m a
runClient (Pure r) = pure r

runClient (Free (Login l k)) = do
    join . lift $ do
        u <- mquery $ findUserByLogin l
        step (Page.login ^. formProcessor $ u)
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
    let location = IdeaLocationSpace i
    _ <- step . lift . (Page.createIdea location ^. formProcessor) $
        ProtoIdea t (Markdown d) (Just c) location
    Just _idea <- postcondition $ findIdeaByTitle t
    runClient k

runClient (Free (EditIdea ot nt d c k)) = do
    idea <- precondition $ do
        Just idea <- findIdeaByTitle ot
        Nothing <- findIdeaByTitle nt
        pure idea
    _ <- step . lift . (Page.editIdea (idea ^. _Id) ^. formProcessor) $
        ProtoIdea nt (Markdown d) (Just c) (idea ^. ideaLocation)
    postcondition $ do
        Nothing <- findIdeaByTitle ot
        Just _idea <- findIdeaByTitle nt
        pure ()
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
        end <- getCurrentTimestamp >>= \now -> query $ phaseEndRefinement now
        (Page.createTopic ideaSpace ^. formProcessor) $
            ProtoTopic tt (Markdown td) "http://url.com" ideaSpace [idea ^. _Id] end
    postcondition $ return ()
    runClient k

runClient (Free (EditTopic ot nt d k)) = do
    topic <- precondition $ do
        Just topic <- findTopicByTitle ot
        Nothing <- findTopicByTitle nt
        pure topic
    _ <- step . lift $ do
        let editTopicPage = Page.editTopic (topic ^. _Id)
        -- FIXME: Add idea handling
        (editTopicPage ^. formProcessor) $ EditTopicData nt (Markdown d) []
    postcondition $ do
        Nothing <- findTopicByTitle ot
        Just _topic <- findTopicByTitle nt
        pure ()
    runClient k

runClient (Free (TimeoutTopic t k)) = do
    Just topic <- precondition $ findTopicByTitle t
    _ <- step . lift $ Action.makeTopicTimeout (topic ^. _Id)
    postcondition $ do
        Just topic' <- findTopicByTitle t
        let phase1 = topic ^. topicPhase
        let phase2 = topic' ^. topicPhase
        unless (phase2 `followsPhase` phase1) . fail . ("runClient: " ++) $ show (phase1, phase2)
    runClient k

runClient (Free (MarkIdea t v k)) = do
    Just idea <- precondition $ findIdeaByTitle t
    _ <- step . lift $ case v of
        Left v'  -> (Page.judgeIdea (idea ^. _Id) (ideaJuryResultValueToType v') ^. formProcessor) v'
        Right v' -> Action.markIdeaInResultPhase (idea ^. _Id) v'
    postcondition $ do
        Just idea' <- findIdeaByTitle t
        case v of
            Left  v' -> (idea' ^? ideaJuryResult . _Just . ideaJuryResultValue) `shouldBe` Just v'
            Right v' -> (idea' ^? ideaVoteResult . _Just . ideaVoteResultValue) `shouldBe` Just v'
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

runClient (Free (CommentIdea t c k)) = do
    Just idea <- precondition $ findIdeaByTitle t
    _ <- step . lift $ (Page.commentIdea (idea ^. ideaLocation) (idea ^. _Id) ^. formProcessor) (Markdown c)
    postcondition $ checkIdeaComment t c
    runClient k

runClient (Free (ReplyComment t cp c k)) = do
    Just (idea, Just comment) <- precondition $ findIdeaAndComment t cp
    _ <- step . lift $
        (Page.replyCommentIdea (idea ^. ideaLocation) (idea ^. _Id) (comment ^. _Id) ^. formProcessor) (Markdown c)
    postcondition $ checkIdeaComment t c
    runClient k

runClient (Free (VoteOnComment t cp v k)) = do
    -- FIXME: Check if the user already voted, if yes the number of votes
    -- should be the same.
    Just (idea, Just comment) <- precondition $ findIdeaAndComment t cp
    _ <- step . lift $ do
        Action.voteIdeaComment (idea ^. ideaLocation) (idea ^. _Id) (comment ^. _Id) v
    postcondition $ do
        Just (_idea, Just comment') <- findIdeaAndComment t cp
        let noOfVotes  = Map.size $ comment  ^. commentVotes
        let noOfVotes' = Map.size $ comment' ^. commentVotes
        noOfVotes' `shouldBe` (noOfVotes + 1)
    runClient k

runClient (Free (VoteOnCommentReply t c1 c2 v k)) = do
    -- FIXME: Check if the user already voted, if yes the number of votes
    -- should be the same.
    Just (idea, Just (comment1, Just comment2)) <-
        precondition $ findIdeaAndCommentComment t c1 c2
    _ <- step . lift $ do
        Action.voteIdeaCommentReply
            (idea ^. ideaLocation)
            (idea ^. _Id)
            (comment1 ^. _Id)
            (comment2 ^. _Id)
            v
    postcondition $ do
        Just (_idea, Just (_comment1, Just comment2')) <-
            findIdeaAndCommentComment t c1 c2
        let noOfVotes  = Map.size $ comment2  ^. commentVotes
        let noOfVotes' = Map.size $ comment2' ^. commentVotes
        noOfVotes' `shouldBe` (noOfVotes + 1)
        runClient k

-- * helpers

findIdeaByTitle :: (ActionM m) => IdeaTitle -> ActionClient m (Maybe Idea)
findIdeaByTitle t = lift $ query (findIdeaBy ideaTitle t)

findTopicByTitle :: (ActionM m) => IdeaTitle -> ActionClient m (Maybe Topic)
findTopicByTitle t = lift $ query (findTopicBy topicTitle t)

findCommentByText :: Idea -> CommentText -> Maybe Comment
findCommentByText i t = find ((t ==) . unMarkdown . _commentText) . Map.elems $ i ^. ideaComments

findCommentCommentByText :: Comment -> CommentText -> Maybe Comment
findCommentCommentByText c t = find ((t ==) . unMarkdown . _commentText) . Map.elems $ c ^. commentReplies

findIdeaAndComment :: (ActionM m) => IdeaTitle -> CommentText -> ActionClient m (Maybe (Idea, Maybe Comment))
findIdeaAndComment it cp =
    fmap (fmap (id &&& flip findCommentByText cp)) (findIdeaByTitle it)

findIdeaAndCommentComment
    :: (ActionM m)
    => IdeaTitle -> CommentText -> CommentText
    -> ActionClient m (Maybe (Idea, Maybe (Comment, Maybe Comment)))
findIdeaAndCommentComment it c1 c2 =
    (_Just . _2 . _Just %~ (id &&& flip findCommentCommentByText c2))
    <$> findIdeaAndComment it c1

checkIdeaComment :: (ActionM m) => IdeaTitle -> CommentText -> ActionClient m ()
checkIdeaComment t c = do
    Just idea' <- findIdeaByTitle t
    let Just _comment = findCommentByText idea' c
    return ()

assert :: (Show msg, Monad m) => msg -> Bool -> m ()
assert _ True  = return ()
assert msg False = error $ "assertion failed: " <> show msg
    -- FIXME: give source code location of the call.

shouldBe :: (Monad m, Eq a, Show a) => a -> a -> m ()
shouldBe actual expected =
    assert
        (unwords [show actual, "should be", show expected])
        (actual == expected)
    -- FIXME: give source code location of the call.

-- ** Notations for test step sections

precondition :: Monad m => m a -> m a
precondition = id

step :: Monad m => m a -> m a
step = id

postcondition :: Monad m => m a -> m a
postcondition = id
