{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Action.Dummy
    ( DummyT(DummyT, unDummyT), Dummy
    , runDummyT, runDummy
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

notImplemented :: (Monad m, ThrowError500 e) => String -> String -> DummyT e m a
notImplemented meth cl = throwError500 $ unlines
    ["Method ", meth, " from class ", cl, " not implemented for instance Dummy"]

instance Monad m => PersistM (DummyT PersistExcept m) where
    getDb _             = notImplemented "PersistM" "getDb"
    modifyDb _ _        = notImplemented "PersistM" "modifyDb"
    getCurrentTimestamp = notImplemented "PersistM" "getCurrentTimestamp"
    mkRandomPassword    = notImplemented "PersistM" "mkRandomPassword"

instance Monad m => ActionTempCsvFiles (DummyT ActionExcept m) where
    popTempCsvFile _      = notImplemented "PersistM" "popTempCsvFile"
    cleanupTempCsvFiles _ = notImplemented "PersistM" "cleanupTempCsvFiles"

instance Monad m => ActionLog (DummyT ActionExcept m) where
    logEvent _ = pure ()

instance Monad m => ActionPersist (DummyT PersistExcept m) (DummyT ActionExcept m) where
    persistent _ = notImplemented "ActionPersist" "persistent"

instance Monad m => ActionError (DummyT ActionExcept m)

instance Monad m => ActionUserHandler (DummyT ActionExcept m) where
    login _     = pure ()
    logout      = pure ()
    userState _ = notImplemented "ActionUserHandler" "userState"

instance Monad m => ActionM (DummyT PersistExcept m) (DummyT ActionExcept m)
