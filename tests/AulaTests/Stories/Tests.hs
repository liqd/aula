{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-missing-signatures #-}

module AulaTests.Stories.Tests where

import Prelude hiding ((.), id)
import Types

import AulaTests.Stories.DSL


idea1 = "idea1"
idea1a = "idea1a"
topic1 = "topic1"
topic1a = "topic1a"
comment1 = "This is the comment 1"
comment2 = "This is the comment 2"

-- FIXME: Idea, Topic creation should be done by different users.
topicTimeoutStory :: Behavior ()
topicTimeoutStory = do
    login "admin"
    selectIdeaSpace "school"
    createIdea idea1a "desc" CatRules
    editIdea idea1a idea1 "desc1" CatRules
    setFreeze Frozen
    likeIdea idea1  -- succeeds, prevented by hiding UI elements
    setFreeze NotFrozen
    commentIdea idea1 comment1
    replyComment idea1 comment1 comment2
    voteOnComment idea1 comment1 Up
    voteOnCommentReply idea1 comment1 comment2 Up
    createTopic idea1 topic1a "desc"
    editTopic topic1a topic1 "desc1"
    timeoutTopic topic1
    markIdea idea1 (Left $ Feasible Nothing)
    setFreeze Frozen
    voteIdea idea1 Yes   -- FIXME: should fail, capabilities should forbid it
    -- FIXME: how to catch the expected error below?
      -- timeoutTopic topic1  -- fails, phase change illegal;
    setFreeze NotFrozen
    voteIdea idea1 Yes   -- now succeeds
    timeoutTopic topic1  -- now succeeds
    markIdea idea1 (Right $ Winning Nothing)
    setCreatorStatement idea1 "Winner"
    revokeWinner idea1
    logout

-- Collection of steps under development, no test design involved.
someUserBehavior :: Behavior ()
someUserBehavior = do
    login "admin"
    selectIdeaSpace "school"
    createIdea idea1 "desc" CatRules
    commentIdea idea1 comment1
    replyComment idea1 comment1 comment2
    reportComment idea1 comment1
    reportCommentReply idea1 comment1 comment2
    deleteComment idea1 comment1
    reportIdea idea1
    deleteIdea idea1
    logout
