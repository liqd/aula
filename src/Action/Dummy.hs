{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings     #-}
module Action.Dummy
    ( DummyT(DummyT,unDummyT), Dummy
    , runDummyT, runDummy
    , notImplemented
    ) where

import Control.Monad.Except (runExceptT)

import Action
import Frontend.Prelude

newtype DummyT m a = DummyT { unDummyT :: ExceptT ActionExcept m a }
    deriving (Functor, Applicative, Monad, MonadError ActionExcept)

type Dummy = DummyT Identity

runDummyT :: DummyT m a -> m (Either ActionExcept a)
runDummyT = runExceptT . unDummyT

runDummy :: Dummy a -> Either ActionExcept a
runDummy = runIdentity . runDummyT

notImplemented :: Monad m => String -> String -> DummyT m a
notImplemented meth _class = throwError500 $ unlines
    ["Method ", meth, " from class ", _class, " not implemented for instance Dummy"]

instance Monad m => PersistM (DummyT m) where
    getDb _             = notImplemented "PersistM" "getDb"
    modifyDb _ _        = notImplemented "PersistM" "modifyDb"
    getCurrentTimestamp = notImplemented "PersistM" "getCurrentTimestamp"
    mkRandomPassword    = notImplemented "PersistM" "mkRandomPassword"

instance Monad m => ActionTempCsvFiles (DummyT m) where
    popTempCsvFile _        = notImplemented "PersistM" "popTempCsvFile"
    cleanupTempCsvFiles _   = notImplemented "PersistM" "cleanupTempCsvFiles"

instance Monad m => ActionLog (DummyT m) where
    logEvent _ = pure ()

instance Monad m => ActionPersist (DummyT m) (DummyT m) where
    persistent = id

instance Monad m => ActionError (DummyT m)

instance Monad m => ActionUserHandler (DummyT m) where
    login _     = pure ()
    logout      = pure ()
    userState _ = notImplemented "ActionUserHandler" "userState"

instance Monad m => ActionM (DummyT m) (DummyT m)
