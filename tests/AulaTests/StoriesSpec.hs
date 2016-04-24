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
import Control.Monad.Trans.Except
import Servant
import Test.Hspec

import Action.Implementation
import DemoData (genInitialTestDb)

import qualified Action
import qualified Persistent
import qualified Persistent.Api as Persistent

import AulaTests (testConfig)
import AulaTests.Stories.DSL
import AulaTests.Stories.Interpreter.Action
import AulaTests.Stories.Tests


spec :: Spec
spec = describe "stories" $ do
    story_ "Topic in refinement phase times out" topicTimeoutStory
    story_ "Some user behavior" someUserBehavior


-- | Runs the 'Behavior' represented story with the 'Action' interpreter,
-- calculates the result and compares to the expected value.
story :: (Eq a, Show a) => String -> Behavior a -> a -> Spec
story name program expected = it name $ do
    join $ do
        cfg <- testConfig
        Persistent.withPersist cfg $ \(persist :: Persistent.RunPersist) -> do

            let runAction :: Action :~> IO
                -- FIXME: Do not use print.
                runAction = exceptToFail
                        . mkRunAction (Action.ActionEnv persist cfg print)

            a <- unNat runAction $ do
                  genInitialTestDb
                  AulaTests.Stories.Interpreter.Action.run program
            return $ a `shouldBe` expected
  where
    exceptToFail :: (Monad m, Show e) => ExceptT e m :~> m
    exceptToFail = Nat (fmap (either (error . show) id) . runExceptT)

story_ :: String -> Behavior () -> Spec
story_ msg program = story msg program ()
