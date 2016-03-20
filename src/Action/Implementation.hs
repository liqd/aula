{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-- | The 'Action.Implementation' module contains a monad stack implmententation of the 'Action'
-- interface.
module Action.Implementation
    ( Action
    , mkRunAction
    )
where

import Control.Exception (SomeException(SomeException), catch)
import Control.Lens
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class
import Control.Monad.RWS.Lazy
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, withExceptT)
import Prelude
import Servant
import Servant.Missing
import Thentos.Action (freshSessionToken)
import Thentos.Prelude (DCLabel, MonadLIO(..), MonadRandom(..), evalLIO, LIOState(..), dcBottom)

import qualified Data.ByteString.Lazy as LBS

import Action
import Data.UriPath
import Persistent
import Types

import qualified Frontend.Path as U


-- * concrete monad type

-- | The actions a user can perform.
newtype Action r a = MkAction { unAction :: ExceptT ActionExcept (RWST (ActionEnv r) () UserState IO) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError ActionExcept
             , MonadReader (ActionEnv r)
             , MonadState UserState
             , MonadIO
             )

instance ActionError (Action r)

instance PersistM r => ActionM r (Action r)

instance ActionLog (Action r) where
    logEvent = liftIO . print

instance PersistM r => ActionPersist r (Action r) where
    persistent r = view persistNat >>= \(Nat rp) -> liftIO $ rp r

instance MonadLIO DCLabel (Action r) where
    liftLIO = liftIO . (`evalLIO` LIOState dcBottom dcBottom)

instance MonadRandom (Action r) where
    getRandomBytes = liftIO . getRandomBytes

instance PersistM r => ActionUserHandler (Action r) where
    login uLogin = do
        muser <- persistent $ findUserByLogin uLogin
        case muser of
            Nothing ->
                redirect . absoluteUriPath . relPath $ U.Login
            Just user -> do
                usUserId .= Just (user ^. _Id)
                sessionToken <- freshSessionToken
                usSessionToken .= Just sessionToken

    userState = use

    logout = put userLoggedOut

instance ActionTempCsvFiles (Action r) where
    popTempCsvFile = liftIO . (`catch` exceptToLeft) . fmap decodeCsv . LBS.readFile
      where
        exceptToLeft (SomeException e) = return . Left . show $ e

    cleanupTempCsvFiles = liftIO . releaseFormTempFiles

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
