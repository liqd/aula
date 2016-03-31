{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module AulaTests.Stories.Interpreter.Action where

import Control.Lens
import Control.Monad (join, unless)
import Control.Monad.Free
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.String.Conversions

import Action
import Persistent
import Types

import AulaTests.Stories.DSL

-- | Client state stores information about the assumptions
-- of the state of server states, it is also can be used
-- to simulate web clients state.
data ClientState = ClientState {
      _csIdeaSpace :: Maybe IdeaSpace
    , _csUser      :: Maybe User
    }
  deriving (Eq, Show)

initialClientState :: ClientState
initialClientState = ClientState Nothing Nothing

makeLenses ''ClientState

run :: (ActionM r m) => Behavior a -> m a
run = flip evalStateT initialClientState . runClient

-- FIXME: Check pre and post conditions
runClient :: (ActionM r m) => Behavior a -> StateT ClientState m a
runClient (Pure r) = pure r

runClient (Free (Login l k)) = do
    join . lift $ do
        Just u <- persistent $ findUserByLogin l
        Action.login (u ^. _Id)
        u' <- currentUser
        assert (u, u') (u == u')
        return $ csUser .= Just u'
    runClient k

runClient (Free (Logout k)) = do
    lift $ do
        Action.logout
    runClient k

runClient (Free (SelectIdeaSpace s k)) = do
    let (Right i :: Either String IdeaSpace) = parseIdeaSpace s
    found <- fmap (elem i) . lift $ persistent getSpaces
    unless found . error $ "No idea space is found" ++ cs s
    csIdeaSpace .= Just i
    runClient k

runClient (Free (CreateIdea t d c k)) = do
    Just i <- use csIdeaSpace
    _ <- lift $ do
        Action.currentUserAddDb
            Persistent.addIdea
            (ProtoIdea t (Markdown d) c (IdeaLocationSpace i))
    runClient k


-- * helpers

assert :: (Show msg, ActionM r m) => msg -> Bool -> m ()
assert _ True  = return ()
assert msg False = error $ "assertion failed: " <> show msg
    -- FIXME: give source code location of the call.
