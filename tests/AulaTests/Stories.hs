{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC #-}

module AulaTests.Stories where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad.Free
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.List
import Data.String
import Data.String.Conversions
import Data.Typeable (Typeable, typeOf)
import Lucid (Html, ToHtml, toHtml, renderText)
import Servant
import Servant.Server.Internal.ServantErr
import Test.Hspec
import Test.QuickCheck
import Text.Digestive.Types
import Text.Digestive.View

import qualified Data.Text.Lazy as LT

import qualified Action
import CreateRandom
import Action.Implementation
import Arbitrary
import Config
import Persistent.Implementation.STM
import Types

import AulaTests.Stories.DSL
import AulaTests.Stories.Interpreter.Action


spec :: Spec
spec = describe "stories" $ it "works" $ do
    liftIO $ do
        print "---------------------------"
        runAction program
        print "---------------------------"
    True `shouldBe` True


runAction :: Behavior a -> IO a
runAction program = do
    persist <- Persistent.Implementation.STM.mkRunPersist

    let runAction :: Action Persistent.Implementation.STM.Persist :~> IO
        runAction = Nat (fmap (either (error . show) id) . runExceptT)
                  . mkRunAction (Action.ActionEnv persist Config.devel)

    unNat persist genInitialTestDb
    unNat runAction $ AulaTests.Stories.Interpreter.Action.run program


program :: Behavior ()
program = do
    login "admin"
    selectIdeaSpace "school"
    createIdea (ProtoIdea "title" (Markdown "desc") CatRule (IdeaLocationSpace SchoolSpace))
    logout
