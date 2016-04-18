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
    login "admin"
    selectIdeaSpace "school"
    createIdea idea1a "desc" CatRules
    editIdea idea1a idea1 "desc1" CatRules
    likeIdea idea1
    commentIdea idea1 "This is a comment"
    commentOnComment idea1 "This is a comment" "This is a comment1"
    createTopic idea1 topic1 "desc"
    timeoutTopic topic1
    markIdea idea1 (Left $ Feasible Nothing)
    voteIdea idea1 Yes
    timeoutTopic topic1
    markIdea idea1 (Right $ Winning Nothing)
    logout
