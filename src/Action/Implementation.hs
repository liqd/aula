{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wall -Werror #-}

-- | The 'Action.Implementation' module contains a monad stack implmententation of the 'Action'
-- interface.
module Action.Implementation
    ( Action
    , mkRunAction
    )
where

import Codec.Picture
import Control.Exception (throwIO, try, ErrorCall(ErrorCall), SomeException(SomeException))
import Control.Lens
import Control.Monad.Except (MonadError, throwError, catchError)
import Control.Monad.IO.Class
import Control.Monad.RWS.Lazy
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, withExceptT)
import Crypto.Scrypt (getEncryptedPass, Pass(Pass), encryptPassIO')
import Data.Elocrypt (mkPassword)
import Data.String.Conversions (LBS, cs)
import Data.Time.Clock (getCurrentTime)
import Prelude
import Servant
import Servant.Missing
import Test.QuickCheck  -- FIXME: remove
import Thentos.Action (freshSessionToken)
import Thentos.Prelude (DCLabel, MonadLIO(..), MonadRandom(..), evalLIO, LIOState(..), dcBottom)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS (lines)
import qualified Data.ByteString.Lazy as LBS

import Action
import Config
import Logger.EventLog
import Persistent
import Persistent.Api
import Types


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
actionIO action = do
    eval <- MkAction . liftIO $ try action
    case eval of
        Left msg  -> throwError $ ActionIOExcept msg
        Right val -> pure val

instance GenArbitrary Action where  -- FIXME: remove
    genGen = actionIO . generate

instance HasSendMail ActionExcept ActionEnv Action where
    sendMailToAddress addr msg = MkAction $ do
        logger <- view envLogger
        sendMailToAddressIO logger addr msg

instance ActionLog Action where
    log msg = actionIO =<< views envLogger ($ msg)

    readEventLog = do
        cfg <- viewConfig
        erows <- actionIO . try $ rd cfg
        case erows of
            Left err   -> throwError $ ActionEventLogExcept err
            Right rows -> (EventLog (cs $ cfg ^. exposedUrl) <$> (warmUp `mapM` rows))
                `catchError` (throwError . ActionEventLogExcept . otherErr)
      where
        rd :: Config -> IO [EventLogItemCold]
        rd cfg = (LBS.lines <$> LBS.readFile (cfg ^. logging . eventLogPath))
             >>= zipWithM adecode [1..]

        adecode :: Int -> LBS -> IO EventLogItemCold
        adecode i = either (throwIO . ErrorCall . msg) pure . Aeson.eitherDecode
          where
            msg aesonSays = "readEventLog:" <> show i <> ": " <> aesonSays

        otherErr :: Show msg => msg -> SomeException
        otherErr = SomeException . ErrorCall . (<> ourMsg) . show
          where
            ourMsg = "\nthis could be caused by a stale event log file with dangling references."

-- | FIXME: test this (particularly strictness and exceptions)
instance ActionPersist Action where
    queryDb = actionIO =<< view (envRunPersist . rpQuery)

    update ev =
        either (throwError . ActionPersistExcept) pure
            =<< actionIO =<< views (envRunPersist . rpUpdate) ($ ev)

instance MonadLIO DCLabel Action where
    liftLIO = actionIO . (`evalLIO` LIOState dcBottom dcBottom)

instance MonadRandom Action where
    getRandomBytes = actionIO . getRandomBytes

instance ActionRandomPassword Action where
    mkRandomPassword = actionIO $ InitialPassword . cs . unwords <$> mkPassword `mapM` [4,3,5]
    mkForgottenPassToken = pure "PWDTOKEN" -- TODO

instance ActionEncryptPassword Action where
    encryptPassword =
        actionIO . fmap (ScryptEncryptedPassword . getEncryptedPass) . encryptPassIO' . Pass . cs

instance ActionCurrentTimestamp Action where
    getCurrentTimestamp = actionIO $ Timestamp <$> getCurrentTime

instance ActionUserHandler Action where
    login uid = do
        usUserId .= Just uid
        sessionToken <- freshSessionToken
        usSessionToken .= Just sessionToken

    addMessage msg = usMessages %= (msg:)

    flushMessages = do
        msgs <- userState usMessages
        usMessages .= []
        pure $ reverse msgs

    userState = use

    logout = put userLoggedOut >> addMessage "Danke fürs Mitmachen!"

instance ReadTempFile Action where
    readTempFile = actionIO . LBS.readFile

instance CleanupTempFiles Action where
    cleanupTempFiles = actionIO . releaseFormTempFiles

instance ActionAvatar Action where
    readImageFile = actionIO . readImage
    savePngImageFile p = actionIO . savePngImage p

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
runActionExcept (ActionEventLogExcept e) = error500 # show e
runActionExcept (ActionIOExcept e) = error500 # show e
