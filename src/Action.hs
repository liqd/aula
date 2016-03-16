{-# LANGUAGE ConstraintKinds            #-}
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

{-# OPTIONS_GHC -Werror -Wall -fno-warn-orphans #-}

-- | The 'Action' module contains an API which
module Action
    ( -- * constraint types
      ActionM
    , ActionLog(logEvent)
    , ActionPersist(persistent)
    , ActionUserHandler(login, logout)
    , ActionError
    , ActionExcept(..)
    , ActionEnv(..), config, persistNat

      -- * concrete monad type (abstract)
    , Action
    , mkRunAction

      -- * user handling
    , currentUser
    , modifyCurrentUser
    , isLoggedIn

      -- * user state
    , UserState(..), usUserId, usCsrfToken, usSessionToken

      -- * extras
    , ActionTempCsvFiles(popTempCsvFile, cleanupTempCsvFiles), decodeCsv

    , MonadServantErr, ThrowServantErr(..)
    )
where

import Control.Exception (SomeException(SomeException), catch)
import Control.Lens
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class
import Control.Monad.RWS.Lazy
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, withExceptT)
import Data.Char (ord)
import Data.Maybe (isJust)
import Data.String.Conversions (ST, LBS)
import Prelude hiding (log)
import Servant
import Servant.Missing

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Csv as Csv
import qualified Data.Vector as V

import Persistent
import Types
import Config (Config)
import Thentos.Prelude (DCLabel, MonadLIO(..), MonadRandom(..), evalLIO, LIOState(..), dcBottom)
import Thentos.Action (freshSessionToken)
import Thentos.Types (GetThentosSessionToken(..), ThentosSessionToken)
import Thentos.Frontend.CSRF (HasSessionCsrfToken(..), GetCsrfSecret(..), CsrfToken)

-- FIXME: Remove. It is scaffolding to generate random data
import Test.QuickCheck (arbitrary, generate)


-- * constraint types

-- | User representation during an action
-- FIXME: Figure out which information is needed here.
data UserState = UserState
    { _usSessionToken :: Maybe ThentosSessionToken
    , _usCsrfToken :: Maybe CsrfToken
    , _usUserId :: Maybe (AUID User)
    }
  deriving (Show, Eq)

makeLenses ''UserState

userLoggedOut :: UserState
userLoggedOut = UserState Nothing Nothing Nothing

data ActionEnv r = ActionEnv
    { _persistNat :: r :~> IO
    , _config     :: Config
    }

makeLenses ''ActionEnv

instance GetCsrfSecret (ActionEnv r) where
    csrfSecret = config . csrfSecret

-- | Top level errors can happen.
--
newtype ActionExcept = ActionExcept { unActionExcept :: ServantErr }
    deriving (Eq, Show)

makePrisms ''ActionExcept

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
    logEvent = liftIO . print

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
    persistent r = view persistNat >>= \(Nat rp) -> liftIO $ rp r

instance MonadLIO DCLabel (Action r) where
    liftLIO = liftIO . (`evalLIO` LIOState dcBottom dcBottom)

instance MonadRandom (Action r) where
    getRandomBytes = liftIO . getRandomBytes

instance HasSessionCsrfToken UserState where
    sessionCsrfToken = usCsrfToken

instance GetThentosSessionToken UserState where
    getThentosSessionToken = usSessionToken

instance ThrowError500 ActionExcept where
    error500 = _ServantErr . error500

class ActionError m => ActionUserHandler m where
    -- | Make the user logged in
    login  :: UserLogin -> m ()
    -- | Read the current user state
    userState :: Getting a UserState a -> m a
    -- | Make the user log out
    logout :: m ()

-- | FIXME: every login changes all other logins (replaces the previous one)
instance PersistM r => ActionUserHandler (Action r) where
    login uLogin = do
        muser <- persistent $ findUserByLogin uLogin
        case muser of
            Nothing ->
                throwError500 $ "ActionUserHandler.login: no such user" <> show uLogin
            Just user -> do
                usUserId .= Just (user ^. _Id)
                sessionToken <- freshSessionToken
                usSessionToken .= Just sessionToken

    userState = use

    logout = put userLoggedOut

instance ThrowServantErr ActionExcept where
    _ServantErr = _ActionExcept

class MonadError ActionExcept m => ActionError m

instance ActionError (Action r)

instance GenArbitrary r => GenArbitrary (Action r) where
    genArbitrary = liftIO $ generate arbitrary


-- * concrete monad type; user state

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
newtype Action r a = MkAction { unAction :: ExceptT ActionExcept (RWST (ActionEnv r) () UserState IO) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError ActionExcept
             , MonadReader (ActionEnv r)
             , MonadState UserState
             , MonadIO
             )

-- | Creates a natural transformation from Action to the servant handler monad.
-- See Frontend.runFrontend for the persistency of @UserState@.
mkRunAction :: PersistM r => ActionEnv r -> Action r :~> ExceptT ServantErr IO
mkRunAction env = Nat run
  where
    run = withExceptT unActionExcept . ExceptT . fmap (view _1) . runRWSTflip env userLoggedOut
        . runExceptT . unAction . (checkCurrentUser >>)
    runRWSTflip r s comp = runRWST comp r s

    checkCurrentUser = do
        isValid <- userState $ to validUserState
        unless isValid $ do
            logout
            throwError500 "Invalid internal user session state"


-- * Action Combinators

-- | Returns the current user ID
currentUserId :: ActionUserHandler m => m (AUID User)
currentUserId = userState usUserId >>= \case
    Nothing -> throwError500 "User is logged out"
    Just uid -> pure uid

-- | Returns the current user
currentUser :: (ActionPersist r m, ActionUserHandler m) => m User
currentUser = do
    muser <- persistent . findUser =<< currentUserId
    case muser of
        Just user -> pure user
        Nothing   -> logout >> throwError500 "Unknown user identitifer"

-- | Modify the current user.
modifyCurrentUser :: (ActionPersist r m, ActionUserHandler m) => (User -> User) -> m ()
modifyCurrentUser f = currentUserId >>= persistent . (`modifyUser` f)

isLoggedIn :: ActionUserHandler m => m Bool
isLoggedIn = userState $ to validLoggedIn


-- * Action Helpers

validLoggedIn :: UserState -> Bool
validLoggedIn us = isJust (us ^. usUserId) && isJust (us ^. usSessionToken)

validUserState :: UserState -> Bool
validUserState us = us == userLoggedOut || validLoggedIn us

-- * csv temp files

class ActionTempCsvFiles m where
    popTempCsvFile :: (Csv.FromRecord r) => FilePath -> m (Either String [r])
    cleanupTempCsvFiles :: FormData -> m ()

instance ActionTempCsvFiles (Action r) where
    popTempCsvFile = liftIO . (`catch` exceptToLeft) . fmap decodeCsv . LBS.readFile
      where
        exceptToLeft (SomeException e) = return . Left . show $ e

    cleanupTempCsvFiles = liftIO . releaseFormTempFiles

decodeCsv :: Csv.FromRecord r => LBS -> Either String [r]
decodeCsv = fmap V.toList . Csv.decodeWith opts Csv.HasHeader
  where
    opts = Csv.defaultDecodeOptions { Csv.decDelimiter = fromIntegral (ord ';') }
