{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
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
    , isLoggedIn

      -- * user state
    , UserState(UserLoggedOut, UserLoggedIn), sessionCookie, username

      -- * extras
    , ActionTempCsvFiles(popTempCsvFile, cleanupTempCsvFiles), decodeCsv
    )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (SomeException(SomeException), catch)
import Control.Lens
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class
import Control.Monad.RWS.Lazy
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.Char (ord)
import Data.String.Conversions (ST, LBS)
import Prelude hiding (log)
import Servant
import Servant.Missing
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv
import qualified Data.Vector as V

import Persistent
import Types

-- FIXME: Remove. It is scaffolding to generate random data
import Test.QuickCheck (arbitrary, generate)


----------------------------------------------------------------------
-- constraint types

-- | User representation during an action
-- FIXME: Figure out which information is needed here.
data UserState
    = UserLoggedOut
    | UserLoggedIn { _username :: UserLogin, _sessionCookie :: ST }
  deriving (Show, Eq)

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

class (UpdatePersistM r, Monad m) => UpdateAction r m | m -> r where
    -- | Run @Persist@ computation in the action monad.
    -- Authorization of the action should happen here.
    -- FIXME: Rename atomically, and only call on
    -- complex computations.
    update :: r a -> m a

instance PersistM r => ActionPersist r (Action r) where
    persistent r = Action $ ask >>= \(Nat rp) -> liftIO $ rp r

instance UpdatePersistM r => UpdateAction r (Action r) where
    update r = Action $ ask >>= \(Nat rp) -> liftIO $ rp r

instance MonadIO (Action r) where
    liftIO = Action . liftIO

class Monad m => ActionUserHandler m where
    -- | Make the user logged in
    login  :: UserLogin -> m ()
    -- | Read the actual user state
    userState :: m UserState
    -- | Make the user log out
    logout :: m ()

-- | FIXME: every login changes all other logins (replaces the previous one)
instance PersistM r => ActionUserHandler (Action r) where
    login uLogin = do
        put $ UserLoggedIn uLogin "session"
        muser <- persistent $ findUserByLogin uLogin
        case muser of
          Nothing ->
            error $ "ActionUserHandler.login: no such user" <> show uLogin
          Just user ->
            liftIO . atomically $ writeTVar fakeCookieTVar (Just $ view _Id user)

    userState = get

    logout = do
        liftIO . atomically $ writeTVar fakeCookieTVar Nothing
        put UserLoggedOut

class MonadError ActionExcept m => ActionError m

instance ActionError (Action r)

instance GenArbitrary r => GenArbitrary (Action r) where
    genArbitrary = Action . liftIO $ generate arbitrary


----------------------------------------------------------------------
-- concrete monad type; user state

-- FIXME: Until we have cookies, we get the logged-in user from this TVar.
-- When we support cookies, we will obtain tokens from them and look up
-- session from the token in the persistent DB. We will also work
-- with thentos sessions, not just plain users.
fakeCookieTVar :: TVar (Maybe (AUID User))
{-# NOINLINE fakeCookieTVar #-}
fakeCookieTVar = unsafePerformIO (newTVarIO Nothing)

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
-- - The ability to change the state is missing. FIXME: which state? login modifes UserState all right
-- - The state should be available after run. FIXME: which state? what for?
mkRunAction :: forall r. PersistM r
            => (r :~> IO) -> Action r :~> ExceptT ServantErr IO
mkRunAction persistNat = Nat run
  where
    run = ExceptT . fmap (view _1)
        . runRWSTflip persistNat UserLoggedOut . runExceptT . (setCurrentUser >>) . unAction
    unAction (Action a) = a
    runRWSTflip r s comp = runRWST comp r s

    setCurrentUser :: ExceptT ActionExcept (RWST (r :~> IO) () UserState IO) ()
    setCurrentUser = do
      mcurrentUID <- liftIO . atomically $ readTVar fakeCookieTVar
      uState <- case mcurrentUID of
         Nothing -> return UserLoggedOut
         Just uid -> do
           muser <- liftIO . unNat persistNat $ findUser uid
           let uLogin = case muser of
                 Nothing -> error "mkRunAction: currently logged in user not in DB"
                 Just user -> user ^. userLogin
           return $ UserLoggedIn uLogin "session"
      put uState


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

isLoggedIn :: ActionUserHandler m => m Bool
isLoggedIn = (UserLoggedOut /=) <$> userState


----------------------------------------------------------------------
-- Action Helpers

loggedInUser :: ActionUserHandler m => m UserLogin
loggedInUser = userState >>= \case
    UserLoggedOut -> error "User is logged out" -- FIXME: Change ActionExcept and reuse here.
    UserLoggedIn uLogin _session -> return uLogin


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
