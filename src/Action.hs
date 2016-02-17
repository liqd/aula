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
    , mkRunAction
    , logEvent
    , login
    , persistent

    , UserState(..)
    , sessionCookie
    , username
    -- FIXME: Remove, only needed by random
    , actionIO
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
type ActionError = ServantErr

-- | User representation during an action
-- FIXME: Figure out which information is needed here.
data UserState
    = UnknownUser
    | User { _username :: ST, _sessionCookie :: ST }

-- | The actions what a user can do.
-- FIXME: Figure out the exact stack we need to use here.
-- Storing the actual session data, userid etc.
-- We should decide on exact userstate and handle everything here.
newtype Action a = Action (ExceptT ActionError (RWST (Persist :~> IO) () UserState IO) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError ServantErr
           , MonadReader (Persist :~> IO)
           , MonadState UserState
           )

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

-- | LiftIO action to Action monad, but hide this implementation detail.
actionIO :: IO a -> Action a
actionIO = Action . liftIO

----------------------------------------------------------------------
-- Combinators

-- | Make the user logged in
login :: ST -> Action ()
login username = do
    put $ User username "session"
    persistent $ loginUser username

-- | Log events
logEvent :: String -> Action ()
logEvent = actionIO . putStrLn

-- | Run @Persist@ computation in the action monad.
persistent :: Persist a -> Action a
persistent r = ask >>= \(Nat rp) -> actionIO $ rp r

{-
-- FIXME: This is an example.
-- Authorizes the current user to do something.
authorize :: Action s a -> Action s a
authorize = id
-}

----------------------------------------------------------------------
-- Lens

makeLenses ''UserState
