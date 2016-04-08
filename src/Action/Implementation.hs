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
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class
import Control.Monad.RWS.Lazy
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, withExceptT)
import Data.Elocrypt (mkPassword)
import Data.String.Conversions (cs)
import Data.Time.Clock (getCurrentTime)
import Prelude
import Servant
import Servant.Missing
import Test.QuickCheck  -- FIXME: remove
import Thentos.Action (freshSessionToken)
import Thentos.Prelude (DCLabel, MonadLIO(..), MonadRandom(..), evalLIO, LIOState(..), dcBottom)

import qualified Data.ByteString.Lazy as LBS

import Types
import Action
import Persistent
import Persistent.Api


-- * concrete monad type

-- | The actions a user can perform.
newtype Action a = MkAction { unAction :: ExceptT ActionExcept (RWST ActionEnv () UserState IO) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError ActionExcept
             , MonadReader ActionEnv
             , MonadState UserState
             )

actionIO :: IO a -> Action a
actionIO = MkAction . liftIO

instance GenArbitrary Action where  -- FIXME: remove
    genGen = actionIO . generate

instance HasSendMail ActionExcept ActionEnv Action where
    sendMailToAddress addr msg = MkAction $ sendMailToAddressIO addr msg

instance ActionLog Action where
    logEvent = actionIO . print

-- | FIXME: test this (particularly strictness and exceptions)
instance ActionPersist Action where
    aqueryDb = actionIO =<< view (envRunPersist . rpQuery)

    aupdate ev =
        either (throwError . ActionPersistExcept) pure
            =<< actionIO =<< views (envRunPersist . rpUpdate) ($ ev)

instance MonadLIO DCLabel Action where
    liftLIO = actionIO . (`evalLIO` LIOState dcBottom dcBottom)

instance MonadRandom Action where
    getRandomBytes = actionIO . getRandomBytes

instance ActionRandomPassword Action where
    mkRandomPassword = actionIO $ UserPassInitial . cs . unwords <$> mkPassword `mapM` [4,3,5]

instance ActionCurrentTimestamp Action where
    getCurrentTimestamp = actionIO $ Timestamp <$> getCurrentTime

instance ActionUserHandler Action where
    login uid = do
        usUserId .= Just uid
        sessionToken <- freshSessionToken
        usSessionToken .= Just sessionToken

    userState = use

    logout = put userLoggedOut

instance ActionTempCsvFiles Action where
    popTempCsvFile = actionIO . (`catch` exceptToLeft) . fmap decodeCsv . LBS.readFile
      where
        exceptToLeft (SomeException e) = return . Left . show $ e

    cleanupTempCsvFiles = actionIO . releaseFormTempFiles

-- | Creates a natural transformation from Action to the servant handler monad.
-- See Frontend.runFrontend for the persistency of @UserState@.
mkRunAction :: ActionEnv -> Action :~> ExceptT ServantErr IO
mkRunAction env = Nat run
  where
    run = withExceptT runActionExcept . ExceptT . fmap (view _1) . runRWSTflip env userLoggedOut
        . runExceptT . unAction . (checkCurrentUser >>)
    runRWSTflip r s comp = runRWST comp r s

    checkCurrentUser = do
        isValid <- userState $ to validUserState
        unless isValid $ do
            logout
            throwError500 "Invalid internal user session state"

runActionExcept :: ActionExcept -> ServantErr
runActionExcept (ActionExcept e) = e
runActionExcept (ActionPersistExcept pe) = runPersistExcept pe
runActionExcept (ActionSendMailExcept e) = error500 # show e
