{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_GHC -Werror -Wall #-}

-- | The 'Action' module contains an API which
module Action
    ( ActionError
    , Action
    , ActionUserHandler(..)
    , ActionLog(..)
    , ActionPersist(..)
    , ActionM
    , mkRunAction

    , UserState(..)
    , sessionCookie
    , username
    -- FIXME: Remove, only needed by random
    , ActionIO(..)
    )
where

import Control.Lens
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.RWS.Lazy
import Data.String.Conversions (ST)
import Persistent
import Prelude hiding (log)
import Servant

----------------------------------------------------------------------
-- User behaviour

-- | Top level errors can happen
-- FIXME: Create a different type
type ActionExcept = ServantErr

-- | User representation during an action
-- FIXME: Figure out which information is needed here.
data UserState
    = UnknownUser
    | User { _username :: ST, _sessionCookie :: ST }

-- | The actions what a user can do.
-- FIXME: Figure out the exact stack we need to use here.
-- Storing the actual session data, userid etc.
-- We should decide on exact userstate and handle everything here.
newtype Action a = Action (ExceptT ActionExcept (RWST (Persist :~> IO) () UserState IO) a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError ActionExcept
             , MonadReader (Persist :~> IO)
             , MonadState UserState
             )

class Monad m => ActionLog m where
    -- | Log events
    logEvent :: ST -> m ()

class Monad m => ActionPersist m where
    -- | Run @Persist@ computation in the action monad.
    -- Authorization of the action should happen here.
    persistent :: Persist a -> m a

class Monad m => ActionUserHandler m where
    -- | Make the user logged in
    login  :: ST -> m ()
    -- | Make the user log out
    logout :: m ()

class MonadError ActionExcept m => ActionError m

-- | FIXME: Action should not have IO computations
class Monad m => ActionIO m where
    actionIO :: IO a -> m a

class ( ActionLog m
      , ActionPersist m
      , ActionUserHandler m
      , ActionIO m
      , ActionError m
      ) => ActionM m

----------------------------------------------------------------------
-- Construction

-- | Creates a natural transformation from Action to IO
-- FIXME: The ability to change that state.
-- The state should be available after run
mkRunAction :: (Persist :~> IO) -> (UserState -> (Action :~> ExceptT ServantErr IO))
mkRunAction persistNat =
    let run s = ExceptT . fmap (view _1) . runRWSTflip persistNat s . runExceptT . unAction
    in \s -> Nat (run s)
  where
    unAction (Action a) = a
    runRWSTflip r s comp = runRWST comp r s

----------------------------------------------------------------------
-- Combinators

instance ActionUserHandler Action where
    login username = do
        put $ User username "session"
        persistent $ loginUser username

    logout = do
        gets _username >>= persistent . logoutUser
        put $ UnknownUser

instance ActionLog Action where
    logEvent = actionIO . putStrLn . show

instance ActionPersist Action where
    persistent r = ask >>= \(Nat rp) -> actionIO $ rp r

instance ActionError Action

instance ActionIO Action where
    actionIO = Action . liftIO

instance ActionM Action

{-
-- FIXME: This is an example.
-- Authorizes the current user to do something.
authorize :: Action s a -> Action s a
authorize = id
-}

----------------------------------------------------------------------
-- Lens

makeLenses ''UserState
