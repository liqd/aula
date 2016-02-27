{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_GHC -Werror -Wall #-}

-- | The 'Action' module contains an API which
module Action
    ( -- * constraint types
      ActionM
    , ActionLog(logEvent)
    , ActionPersist(persistent)
    , ActionUserHandler(login, logout)
    , ActionError
    , ActionExcept

      -- * concrete monad type (abstract)
    , Action
    , mkRunAction

      -- * user handling
    , currentUser
    , modifyCurrentUser

      -- * user state
    , UserState(UserLoggedOut, UserLoggedIn), sessionCookie, username
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
import Types

-- FIXME: Remove. It is scaffolding to generate random data
import Test.QuickCheck (arbitrary, generate)

----------------------------------------------------------------------
-- constraint types

-- | User representation during an action
-- FIXME: Figure out which information is needed here.
data UserState
    = UserLoggedOut
    | UserLoggedIn { _username :: ST, _sessionCookie :: ST }

makeLenses ''UserState

class ( ActionLog m
      , ActionPersist m
      , ActionUserHandler m
      , ActionError m
      ) => ActionM m

instance ActionM Action

class Monad m => ActionLog m where
    -- | Log events
    logEvent :: ST -> m ()

instance ActionLog Action where
    logEvent = Action . liftIO . print

class Monad m => ActionPersist m where
    -- | Run @Persist@ computation in the action monad.
    -- Authorization of the action should happen here.
    persistent :: Persist a -> m a

instance ActionPersist Action where
    persistent r = Action $ ask >>= \(Nat rp) -> liftIO $ rp r

class Monad m => ActionUserHandler m where
    -- | Make the user logged in
    login  :: ST -> m ()
    -- | Read the actual user state
    userState :: m UserState
    -- | Make the user log out
    logout :: m ()

instance ActionUserHandler Action where
    login user = do
        put $ UserLoggedIn user "session"
        persistent $ loginUser user

    userState = get

    logout = do
        use username >>= persistent . logoutUser
        put UserLoggedOut

class MonadError ActionExcept m => ActionError m

instance ActionError Action

instance GenArbitrary Action where
    genArbitrary = Action . liftIO $ generate arbitrary

----------------------------------------------------------------------
-- concrete monad type; user state

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
             , MonadError ActionExcept
             , MonadReader (Persist :~> IO)
             , MonadState UserState
             )

-- | Top level errors can happen.
--
-- FIXME: Create a different type
type ActionExcept = ServantErr

-- | Creates a natural transformation from Action to IO
--
-- FIXME:
-- - The ability to change the state is missing.
-- - The state should be available after run.
mkRunAction :: (Persist :~> IO) -> UserState -> (Action :~> ExceptT ServantErr IO)
mkRunAction persistNat = \s -> Nat (run s)
  where
    run s = ExceptT . fmap (view _1) . runRWSTflip persistNat s . runExceptT . unAction
    unAction (Action a) = a
    runRWSTflip r s comp = runRWST comp r s

----------------------------------------------------------------------
-- Action Combinators

-- | Returns the current user
currentUser :: (ActionPersist m, ActionUserHandler m) => m User
currentUser =
    loggedInUser
    >>= persistent . findUserByLogin
    >>= \ (Just user) -> return user

-- | Modify the current user.
modifyCurrentUser :: (ActionPersist m, ActionUserHandler m) => (User -> User) -> m ()
modifyCurrentUser f =
  currentUser >>= persistent . flip modifyUser f . (^. _Id)

----------------------------------------------------------------------
-- Action Helpers

loggedInUser :: (ActionUserHandler m) => m ST
loggedInUser = userState >>= \case
    UserLoggedOut -> error "User is logged out" -- FIXME: Change ActionExcept and reuse here.
    UserLoggedIn user _session -> return user
