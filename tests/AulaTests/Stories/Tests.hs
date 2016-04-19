{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module AulaTests.Stories.Tests where

import Prelude hiding ((.), id)
import Types

import AulaTests.Stories.DSL

-- FIXME: Idea, Topic creation should be done by different users.
topicTimeoutStory :: Behavior ()
topicTimeoutStory = do
    let idea1 = "idea1"
    let idea1a = "idea1a"
    let topic1 = "topic1"
    let topic1a = "topic1a"
    let comment1 = "This is the comment 1"
    let comment2 = "This is the comment 2"
    let comment3 = "This is the comment 3"
    login "admin"
    selectIdeaSpace "school"
    createIdea idea1a "desc" CatRules
    editIdea idea1a idea1 "desc1" CatRules
    likeIdea idea1
    commentIdea idea1 comment1
    replyComment idea1 comment1 comment2
    voteOnComment idea1 comment1 Up
    voteOnCommentReply idea1 comment1 comment2 Up
    commentIdea idea1 comment3
    deleteComment idea1 comment3
    createTopic idea1 topic1a "desc"
    editTopic topic1a topic1 "desc1"
    timeoutTopic topic1
    markIdea idea1 (Left $ Feasible Nothing)
    voteIdea idea1 Yes
    timeoutTopic topic1
    markIdea idea1 (Right $ Winning Nothing)
    logout
