{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC #-}

module AulaTests.Stories.DSL where

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
import Test.Hspec  -- (Spec, context, it, pendingWith, shouldBe)
import Test.QuickCheck  -- (Arbitrary(..), Gen, forAll, property)
import Test.QuickCheck.Monadic (assert, monadicIO, run, pick)
import Text.Digestive.Types
import Text.Digestive.View

import qualified Data.Text.Lazy as LT

import Arbitrary
import Types


-- * domain model ("the nouns")

type IdeaSpaceName = ST


-- * the dsl ("the action sentences")

data Step a where
    Login            :: UserLogin -> a -> Step a
    Logout           :: a -> Step a
    SelectIdeaSpace  :: IdeaSpaceName -> a -> Step a
    CreateIdea       :: ST -> ST -> Category -> a -> Step a
  deriving Functor

type Behavior = Free Step

login :: UserLogin -> Behavior ()
login l = liftF $ Login l ()

logout :: Behavior ()
logout = liftF $ Logout ()

selectIdeaSpace :: IdeaSpaceName -> Behavior ()
selectIdeaSpace n = liftF $ SelectIdeaSpace n ()

createIdea :: ST -> ST -> Category -> Behavior ()
createIdea title desc cat = liftF $ CreateIdea title desc cat ()
