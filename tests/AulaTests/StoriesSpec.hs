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
import Control.Lens
import Control.Monad (join)
import Control.Monad.Trans.Except
import Data.Time (getCurrentTime)
import Servant
import Test.Hspec

import Action.Implementation
import Config
import CreateRandom (genInitialTestDb)
import Types

import qualified Action
import qualified Persistent

import AulaTests.Stories.DSL
import AulaTests.Stories.Interpreter.Action
import AulaTests.Stories.Tests


spec :: Spec
spec = describe "stories" $ do
    story_ "Topic in refinement phase times out" topicTimeoutStory


-- | Runs the 'Behavior' represented story with the 'Action' interpreter,
-- calculates the result and compares to the expected value.
story :: (Eq a, Show a) => String -> Behavior a -> a -> Spec
story name program expected = it name $ do
    join $ do
        cfg <- (persistenceImpl .~ AcidStateInMem) <$> Config.getConfig DontWarnMissing
        now <- Timestamp <$> getCurrentTime
        Persistent.withPersist cfg $ \(persist :: Persistent.RunPersist) -> do

            let runAction :: Action :~> IO
                runAction = exceptToFail
                        . mkRunAction (Action.ActionEnv persist now cfg)

            a <- unNat runAction $ do
                  genInitialTestDb
                  AulaTests.Stories.Interpreter.Action.run program
            return $ a `shouldBe` expected
  where
    exceptToFail :: (Monad m, Show e) => ExceptT e m :~> m
    exceptToFail = Nat (fmap (either (error . show) id) . runExceptT)

story_ :: String -> Behavior () -> Spec
story_ msg program = story msg program ()
