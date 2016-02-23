{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-missing-signatures -fno-warn-incomplete-patterns -fno-warn-unused-imports #-}

module Bla where

import Control.Monad.Free
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.List
import Data.String
import Data.String.Conversions
import Data.Typeable (Typeable, typeOf)
import Lucid (Html, ToHtml, toHtml, renderText)
import Servant (unNat)
import Servant.Server.Internal.ServantErr
import Test.Hspec (Spec, context, it, pendingWith, shouldBe)
import Test.QuickCheck (Arbitrary(..), Gen, forAll, property)
import Test.QuickCheck.Monadic (assert, monadicIO, run, pick)
import Text.Digestive.Types
import Text.Digestive.View

import qualified Data.Text.Lazy as LT

import Action
import Arbitrary ()
import Frontend.Page
import Persistent
import Types

spec :: Spec
spec = return ()


-- the dsl

data BehaviorF a where
    ListIdeaSpaces :: [IdeaSpace]         -> a -> BehaviorF a
    ListWildIdeas  :: [Idea]              -> a -> BehaviorF a
    CreateWildIdea :: (ProtoIdea -> Idea) -> a -> BehaviorF a

type Behavior = Free BehaviorF

listIdeaSpaces :: [IdeaSpace]         -> a -> BehaviorF a
listWildIdeas  :: [Idea]              -> a -> BehaviorF a
lreateWildIdea :: (ProtoIdea -> Idea) -> a -> BehaviorF a


-- properties:
--
-- 1. list spaces, list ideas, create idea, repeat; collect all ideas; collected ideas should always
--    match listed ideas.


-- interpreter: Action




-- interpreter: wreq


-- interpreter: selenium


-- generate page map
