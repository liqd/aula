{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

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

      -- * extras
    , ActionTempCsvFiles(popTempCsvFile, cleanupTempCsvFiles), decodeCsv
    )
where

import Control.Exception (SomeException(SomeException), catch)
import Control.Lens
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class
import Control.Monad.RWS.Lazy
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.Char (ord)
import Data.String.Conversions (ST, LBS)
import Persistent
import Prelude hiding (log)
import Servant
import Servant.Missing
import Types

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv
import qualified Data.Vector as V

-- FIXME: Remove. It is scaffolding to generate random data
import Test.QuickCheck (arbitrary, generate)


----------------------------------------------------------------------
-- constraint types

-- | User representation during an action
-- FIXME: Figure out which information is needed here.
data UserState
    = UserLoggedOut
    | UserLoggedIn { _username :: UserLogin, _sessionCookie :: ST }

makeLenses ''UserState

class ( ActionLog m
      , ActionPersist r m
      , ActionUserHandler m
      , ActionError m
      , ActionTempCsvFiles m
      ) => ActionM r m

instance PersistM r => ActionM r (Action r)

class Monad m => ActionLog m where
    -- | Log events
    logEvent :: ST -> m ()

instance ActionLog (Action r) where
    logEvent = Action . liftIO . print

-- | A monad that can include actions changing a persistent state.
--
-- @r@ is determined by @m@, because @m@ is intended to be the program's
-- action monad, so @r@ is just the persistent implementation chosen
-- to be used in the action monad.
class (PersistM r, Monad m) => ActionPersist r m | m -> r where
    -- | Run @Persist@ computation in the action monad.
    -- Authorization of the action should happen here.
    -- FIXME: Rename atomically, and only call on
    -- complex computations.
    persistent :: r a -> m a

instance PersistM r => ActionPersist r (Action r) where
    persistent r = Action $ ask >>= \(Nat rp) -> liftIO $ rp r

instance MonadIO (Action r) where
    liftIO = Action . liftIO

instance PersistM r => PersistM (Action r) where
    -- getDb :: AulaGetter a -> m a
    getDb = persistent . getDb
    -- modifyDb :: AulaSetter a -> (a -> a) -> m ()
    modifyDb setter = persistent . modifyDb setter

class Monad m => ActionUserHandler m where
    -- | Make the user logged in
    login  :: UserLogin -> m ()
    -- | Read the actual user state
    userState :: m UserState
    -- | Make the user log out
    logout :: m ()

instance PersistM r => ActionUserHandler (Action r) where
    login user = do
        put $ UserLoggedIn user "session"
        persistent $ loginUser user

    userState = get

    logout = do
        currentUser >>= persistent . logoutUser . view userLogin
        put UserLoggedOut

class MonadError ActionExcept m => ActionError m

instance ActionError (Action r)

instance GenArbitrary r => GenArbitrary (Action r) where
    genArbitrary = Action . liftIO $ generate arbitrary


----------------------------------------------------------------------
-- concrete monad type; user state

-- | The actions a user can perform.
--
-- FIXME:
-- - Figure out the exact stack we need to use here.
-- - Store the actual session data, userid etc.
-- - We should decide on exact userstate and handle everything here.
--
-- FUTUREWORK: Move action implementation to another module and hide behind
-- an API, similarly as it's done with persistent implementation,
-- to reveal and mark (and possibly fix) where the implementation is hardwired.
newtype Action r a = Action (ExceptT ActionExcept (RWST (r :~> IO) () UserState IO) a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError ActionExcept
             , MonadReader (r :~> IO)
             , MonadState UserState
             )

-- | Top level errors can happen.
--
-- FIXME: Create a different type
type ActionExcept = ServantErr

-- | Creates a natural transformation from Action to the servant handler monad.
--
-- FIXME:
-- - The ability to change the state is missing.
-- - The state should be available after run.
mkRunAction :: (r :~> IO) -> UserState -> (Action r :~> ExceptT ServantErr IO)
mkRunAction persistNat = \s -> Nat (run s)
  where
    run s = ExceptT . fmap (view _1) . runRWSTflip persistNat s . runExceptT . unAction
    unAction (Action a) = a
    runRWSTflip r s comp = runRWST comp r s


----------------------------------------------------------------------
-- Action Combinators

-- | Returns the current user
currentUser :: (ActionPersist r m, ActionUserHandler m) => m User
currentUser =
    loggedInUser
    >>= persistent . findUserByLogin
    >>= \ (Just user) -> return user

-- | Modify the current user.
modifyCurrentUser :: (ActionPersist r m, ActionUserHandler m) => (User -> User) -> m ()
modifyCurrentUser f =
  currentUser >>= persistent . flip modifyUser f . (^. _Id)


----------------------------------------------------------------------
-- Action Helpers

loggedInUser :: (ActionUserHandler m) => m UserLogin
loggedInUser = userState >>= \case
    UserLoggedOut -> error "User is logged out" -- FIXME: Change ActionExcept and reuse here.
    UserLoggedIn user _session -> return user


----------------------------------------------------------------------
-- csv temp files

class ActionTempCsvFiles m where
    popTempCsvFile :: (Csv.FromRecord r) => FilePath -> m (Either String [r])
    cleanupTempCsvFiles :: FormData -> m ()

instance ActionTempCsvFiles (Action r) where
    popTempCsvFile = Action . liftIO . (`catch` exceptToLeft) . fmap decodeCsv . LBS.readFile
      where
        exceptToLeft (SomeException e) = return . Left . show $ e

    cleanupTempCsvFiles = Action . liftIO . releaseFormTempFiles

decodeCsv :: Csv.FromRecord r => LBS -> Either String [r]
decodeCsv = fmap V.toList . Csv.decodeWith opts Csv.HasHeader
  where
    opts = Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ';') }
