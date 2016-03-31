{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module AulaTests.Stories where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Servant
import Test.Hspec

import qualified Action
import CreateRandom
import Action.Implementation
import Config
import Persistent.Implementation.STM
import Types

import AulaTests.Stories.DSL
import AulaTests.Stories.Interpreter.Action


spec :: Spec
spec = describe "stories" . it "works" $ do
    x <- liftIO $ runProgram simpleTest
    x `shouldBe` ()



runProgram :: Behavior a -> IO a
runProgram program = do
    config <- Config.getConfig DontWarnMissing
    (persist, closePersist) <- Persistent.Implementation.STM.mkRunPersist

    let runAction :: Action Persistent.Implementation.STM.Persist :~> IO
        runAction = exceptToFail
                  . mkRunAction (Action.ActionEnv persist config)

    unNat (exceptToFail . persist) genInitialTestDb
    a <- unNat runAction $ AulaTests.Stories.Interpreter.Action.run program
    closePersist
    return a


simpleTest :: Behavior ()
simpleTest = do
    login "admin"
    selectIdeaSpace "school"
    createIdea "idea1" "desc" CatRule
    likeIdea "idea1"
    createTopic "idea1" "topic1" "desc"
    timeoutTopic "topic1"
    logout

exceptToFail :: (Monad m, Show e) => ExceptT e m :~> m
exceptToFail = Nat (fmap (either (error . show) id) . runExceptT)