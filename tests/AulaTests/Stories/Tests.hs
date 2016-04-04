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
    login "admin"
    selectIdeaSpace "school"
    createIdea idea1 "desc" CatRule
    likeIdea idea1
    createTopic idea1 "topic1" "desc"
    timeoutTopic "topic1"
    markIdea idea1 (Left $ Feasible Nothing)
    voteIdea idea1 Yes
    logout
