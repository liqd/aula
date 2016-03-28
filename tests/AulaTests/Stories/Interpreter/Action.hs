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
import Test.Hspec
import Test.QuickCheck
import Text.Digestive.Types
import Text.Digestive.View

import qualified Data.Text.Lazy as LT

import Action
import Persistent
import Arbitrary
import Types

import AulaTests.Stories.DSL


runLog :: Behavior a -> IO a
runLog (Pure r)                     = pure r
runLog (Free (Login l k))           = print ("logged in: " <> show l)         >> runLog k
runLog (Free (Logout k))            = print "logged out"                      >> runLog k
runLog (Free (SelectIdeaSpace s k)) = print ("select idea space: " <> show s) >> runLog k
runLog (Free (CreateIdea pi k))     = print ("create idea: " <> show pi)      >> runLog k

run :: (ActionM r m) => Behavior a -> m a
run (Pure r) = pure r

run (Free (Login l k)) = do
    Just u <- persistent $ findUserByLogin l
    Action.login u
    u' <- currentUser
    assert (u, u') (u == u')
    run k

run (Free (Logout k)) = do
    Action.logout >> run k

run (Free (SelectIdeaSpace s k)) = do
    let (Right i :: Either String IdeaSpace) = parseIdeaSpace s
    persistent $ addIdeaSpaceIfNotExists i
    run k

run (Free (CreateIdea pi k)) = do
    Action.currentUserAddDb Persistent.addIdea pi
    run k


-- * helpers

assert :: (Show msg, ActionM r m) => msg -> Bool -> m ()
assert _ True  = return ()
assert msg False = error $ "assertion failed: " <> show msg
    -- FIXME: give source code location of the call.
