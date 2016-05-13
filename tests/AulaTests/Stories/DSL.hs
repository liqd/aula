{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module AulaTests.Stories.DSL where

import Control.Monad.Free
import Data.String.Conversions

import Types


-- * domain model ("the nouns")

type IdeaSpaceName = ST
type IdeaTitle = ST
type IdeaDescription = ST
type TopicTitle = ST
type TopicDescription = ST
type CommentText = ST
type Statement = ST
type ReportText = ST


-- * the dsl ("the action sentences")

data Step a where
    -- User actions
    Login               :: UserLogin -> a -> Step a
    Logout              :: a -> Step a
    SelectIdeaSpace     :: IdeaSpaceName -> a -> Step a
    CreateIdea          :: IdeaTitle -> IdeaDescription -> Category -> a -> Step a
    EditIdea            :: IdeaTitle -> IdeaTitle -> IdeaDescription -> Category -> a -> Step a
    LikeIdea            :: IdeaTitle -> a -> Step a
    DeleteIdea          :: IdeaTitle -> a -> Step a
    ReportIdea          :: IdeaTitle -> a -> Step a
    CreateTopic         :: IdeaTitle -> TopicTitle -> TopicDescription -> a -> Step a
    EditTopic           :: TopicTitle -> TopicTitle -> TopicDescription -> a -> Step a
    MarkIdea            :: IdeaTitle -> Either IdeaJuryResultValue IdeaVoteResultValue -> a -> Step a
    VoteIdea            :: IdeaTitle -> IdeaVoteValue -> a -> Step a
    MoveIdea            :: IdeaTitle -> TopicTitle -> TopicTitle -> a -> Step a
    CommentOnIdea       :: IdeaTitle -> CommentText -> a -> Step a
    RevokeWinner        :: IdeaTitle -> a -> Step a
    ReplyComment        :: IdeaTitle -> CommentText -> CommentText -> a -> Step a
    VoteOnComment       :: IdeaTitle -> CommentText -> UpDown -> a -> Step a
    VoteOnCommentReply  :: IdeaTitle -> CommentText -> CommentText -> UpDown -> a -> Step a
    ReportComment       :: IdeaTitle -> CommentText -> ReportText -> a -> Step a
    ReportCommentReply  :: IdeaTitle -> CommentText -> CommentText -> ReportText -> a -> Step a
    DeleteComment       :: IdeaTitle -> CommentText -> a -> Step a
    SetCreatorStatement :: IdeaTitle -> Statement -> a -> Step a
    SetFreeze           :: Freeze -> a -> Step a

    -- System events, these events probably need a test support, API, etc...
    TimeoutTopic     :: TopicTitle -> a -> Step a
  deriving Functor

type Behavior = Free Step

login :: UserLogin -> Behavior ()
login l = liftF $ Login l ()

logout :: Behavior ()
logout = liftF $ Logout ()

selectIdeaSpace :: IdeaSpaceName -> Behavior ()
selectIdeaSpace n = liftF $ SelectIdeaSpace n ()

createIdea :: IdeaTitle -> IdeaDescription -> Category -> Behavior ()
createIdea title desc cat = liftF $ CreateIdea title desc cat ()

editIdea :: IdeaTitle -> IdeaTitle -> IdeaDescription -> Category -> Behavior ()
editIdea oldTitle newTitle desc cat = liftF $ EditIdea oldTitle newTitle desc cat ()

likeIdea :: IdeaTitle -> Behavior ()
likeIdea title = liftF $ LikeIdea title ()

deleteIdea :: IdeaTitle -> Behavior ()
deleteIdea title = liftF $ DeleteIdea title ()

reportIdea :: IdeaTitle -> Behavior ()
reportIdea title = liftF $ ReportIdea title ()

createTopic :: IdeaTitle -> TopicTitle -> TopicDescription -> Behavior ()
createTopic ititle ttitle tdesc = liftF $ CreateTopic ititle ttitle tdesc ()

editTopic :: TopicTitle -> TopicTitle -> TopicDescription -> Behavior ()
editTopic oldTitle newTitle desc = liftF $ EditTopic oldTitle newTitle desc ()

timeoutTopic :: TopicTitle -> Behavior ()
timeoutTopic title = liftF $ TimeoutTopic title ()

markIdea :: IdeaTitle -> Either IdeaJuryResultValue IdeaVoteResultValue -> Behavior ()
markIdea title value = liftF $ MarkIdea title value ()

voteIdea :: IdeaTitle -> IdeaVoteValue -> Behavior ()
voteIdea title vote = liftF $ VoteIdea title vote ()

moveIdea :: IdeaTitle -> TopicTitle -> TopicTitle -> Behavior ()
moveIdea idea oldTopic newTopic = liftF $ MoveIdea idea oldTopic newTopic ()

commentOnIdea :: IdeaTitle -> CommentText -> Behavior ()
commentOnIdea title text = liftF $ CommentOnIdea title text ()

revokeWinner :: IdeaTitle -> Behavior ()
revokeWinner title = liftF $ RevokeWinner title ()

replyComment :: IdeaTitle -> CommentText -> CommentText -> Behavior ()
replyComment title comment text = liftF $ ReplyComment title comment text ()

voteOnComment :: IdeaTitle -> CommentText -> UpDown -> Behavior ()
voteOnComment idea comment vote = liftF $ VoteOnComment idea comment vote ()

voteOnCommentReply :: IdeaTitle -> CommentText -> CommentText -> UpDown -> Behavior ()
voteOnCommentReply idea comment1 comment2 vote =
    liftF $ VoteOnCommentReply idea comment1 comment2 vote ()

reportComment :: IdeaTitle -> CommentText -> ReportText -> Behavior ()
reportComment idea comment1 report = liftF $ ReportComment idea comment1 report ()

reportCommentReply :: IdeaTitle -> CommentText -> CommentText -> ReportText -> Behavior ()
reportCommentReply idea comment1 comment2 report = liftF $ ReportCommentReply idea comment1 comment2 report ()

deleteComment :: IdeaTitle -> CommentText -> Behavior ()
deleteComment idea comment =
    liftF $ DeleteComment idea comment ()

setCreatorStatement :: IdeaTitle -> Statement -> Behavior ()
setCreatorStatement idea statement =
    liftF $ SetCreatorStatement idea statement ()

setFreeze :: Freeze -> Behavior ()
setFreeze shouldBeFrozenOrNot = liftF $ SetFreeze shouldBeFrozenOrNot ()
