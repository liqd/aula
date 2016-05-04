{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Action.Dummy
    ( DummyT(DummyT, unDummyT), Dummy
    , runDummyT, runDummy, runDummyIO
    , notImplemented
    ) where

import Control.Monad.Except (runExceptT)

import Action
import Frontend.Prelude

newtype DummyT e m a = DummyT { unDummyT :: ExceptT e m a }
    deriving (Functor, Applicative, Monad, MonadError e)

type Dummy e = DummyT e Identity

runDummyT :: DummyT e m a -> m (Either e a)
runDummyT = runExceptT . unDummyT

runDummy :: Dummy e a -> Either e a
runDummy = runIdentity . runDummyT

runDummyIO :: Show e => DummyT e IO a -> IO a
runDummyIO m = either (fail . show) pure =<< runDummyT m

notImplemented :: (Monad m, ThrowError500 e) => String -> String -> DummyT e m a
notImplemented meth cl = throwError500 $ unlines
    ["Method ", meth, " from class ", cl, " not implemented for instance Dummy"]

instance Monad m => ReadTempFile (DummyT ActionExcept m) where
    readTempFile     _ = notImplemented "ReadTempFile" "readTempFile"

instance Monad m => CleanupTempFiles (DummyT ActionExcept m) where
    cleanupTempFiles _ = notImplemented "CleanupTempFiles" "cleanupTempFiles"

instance Monad m => ActionAvatar (DummyT ActionExcept m) where
    readImageFile _      = notImplemented "ActionAvatar" "readImageFile"
    savePngImageFile _ _ = notImplemented "ActionAvatar" "savePngImageFile"

instance Monad m => ActionLog (DummyT ActionExcept m) where
    log _ = pure ()
    readEventLog = notImplemented "ActionLog" "readEventLog"

instance Monad m => ActionPersist (DummyT ActionExcept m) where
    queryDb = notImplemented "ActionPersist" "queryDb"
    update _ = notImplemented "ActionPersist" "update"

instance Monad m => ActionRandomPassword (DummyT ActionExcept m) where
    mkRandomPassword = notImplemented "ActionRandomPassword" "mkRandomPassword"

instance Monad m => ActionCurrentTimestamp (DummyT ActionExcept m) where
    getCurrentTimestamp = notImplemented "ActionCurrentTimestamp" "getCurrentTimestamp"

instance Monad m => ActionUserHandler (DummyT ActionExcept m) where
    login _       = pure ()
    logout        = pure ()
    userState _   = notImplemented "ActionUserHandler" "userState"
    addMessage _  = notImplemented "ActionUserHandler" "addMessage"
    flushMessages = notImplemented "ActionUserHandler" "flushMessage"

instance Monad m => MonadReader ActionEnv (DummyT ActionExcept m) where
    ask = notImplemented "MonadReader" "ask"
    local _ _ = notImplemented "MonadReader" "local"

instance Monad m => HasSendMail ActionExcept ActionEnv (DummyT ActionExcept m) where
    sendMailToAddress _ _ = notImplemented "HasSendMail" "sendMailToAddress"
