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

-- | Top level errors can happen.
--
-- FIXME: Create a different type
type ActionExcept = ServantErr

-- | User representation during an action
-- FIXME: Figure out which information is needed here.
data UserState
    = UnknownUser
    | User { _username :: ST, _sessionCookie :: ST }

-- | The actions a user can perform.
--
-- FIXME:
-- - Figure out the exact stack we need to use here.
-- - Store the actual session data, userid etc.
-- - We should decide on exact userstate and handle everything here.
newtype Action a = Action (ExceptT ActionExcept (RWST (Persist :~> IO) () UserState IO) a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
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

class ( ActionLog m
      , ActionPersist m
      , ActionUserHandler m
      , ActionError m
      , MonadIO m  -- FIXME: can we get rid of this?  (it is needed in "CreateRandom", but also for
                   -- 'ActionLog', 'ActionPersist'.)
      ) => ActionM m

----------------------------------------------------------------------
-- Construction

-- | Creates a natural transformation from Action to IO
-- FIXME: The ability to change that state.
-- The state should be available after run
mkRunAction :: (Persist :~> IO) -> UserState -> (Action :~> ExceptT ServantErr IO)
mkRunAction persistNat = \s -> Nat (run s)
  where
    run s = ExceptT . fmap (view _1) . runRWSTflip persistNat s . runExceptT . unAction
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
        put UnknownUser

instance ActionLog Action where
    logEvent = liftIO . print

instance ActionPersist Action where
    persistent r = ask >>= \(Nat rp) -> liftIO $ rp r

instance ActionError Action

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
