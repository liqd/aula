{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC #-}

module AulaTests.Stories.Interpreter.Action where

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
import Text.Digestive.Types
import Text.Digestive.View

import qualified Data.Text.Lazy as LT

import Arbitrary
import Types

import AulaTests.Stories.DSL


run :: Behavior a -> IO a
run (Pure r)                     = pure r
run (Free (Login l k))           = print ("logged in: " <> show l) >> run k
run (Free (Logout k))            = print "logged out" >> run k
run (Free (SelectIdeaSpace s k)) = print ("select idea space: " <> show s) >> run k
run (Free (CreateIdea pi k))     = print ("create idea: " <> show pi) >> run k
