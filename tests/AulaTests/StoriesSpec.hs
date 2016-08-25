{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module AulaTests.StoriesSpec where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad (join)
import Servant
import Test.Hspec

import Types (exceptToFail)
import Action.Implementation
import DemoData (genInitialTestDb)
import Logger (nullLog)

import qualified Action
import qualified Persistent
import qualified Persistent.Api as Persistent

import AulaTests (testConfig, withServer)
import AulaTests.Stories.DSL
import AulaTests.Stories.Interpreter.Action
import qualified AulaTests.Stories.Interpreter.WebDriver as WDA
import AulaTests.Stories.Tests


spec :: Spec
spec = describe "stories" $ do
    story_ "Topic in refinement phase times out" topicTimeoutStory
    story_ "Some user behavior" someUserBehavior
    story_ "Back and forth: jury, voting phases" backAndForthJuryVotingPhases
    -- the following tests that when you revert to jury phase to fix an idea and fix it, the change
    -- in the idea verdict will trigger a phase transition back to voting phase.
    story_ "Mark idea not feasable after it is marked" markIdeaAsNotFeasableAfterMarked
    wdStory_ "Edit profile test" editUserProfile


-- | Runs the 'Behavior' represented story with the 'Action' interpreter,
-- calculates the result and compares to the expected value.
story :: (Eq a, Show a) => String -> Behavior a -> a -> Spec
story msg program expected = it msg $ do
    join $ do
        cfg <- testConfig
        Persistent.withPersist cfg $ \(persist :: Persistent.RunPersist) -> do

            let runAction :: Action :~> IO
                runAction = exceptToFail . mkRunAction (Action.ActionEnv persist cfg nullLog Nothing)

            a <- unNat runAction $ do
                  genInitialTestDb
                  AulaTests.Stories.Interpreter.Action.run program
            return $ a `shouldBe` expected

story_ :: String -> Behavior () -> Spec
story_ msg program = story msg program ()

wdStory :: (Eq a, Show a) => String -> Behavior a -> a -> Spec
wdStory msg program expected = do
    describe "@Selenium" . around withServer . it msg $ \wreq -> do
        x <- WDA.run wreq program
        x `shouldBe` Just expected

wdStory_ :: String -> Behavior () -> Spec
wdStory_ msg program = wdStory msg program ()
