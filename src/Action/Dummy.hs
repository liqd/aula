{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Action.Dummy
    ( DummyT(DummyT, unDummyT), Dummy
    , runDummyT, runDummy
    , notImplemented
    ) where

import Control.Monad.Except (runExceptT)

import Action
import Persistent.Implementation.STM (Persist)
import Frontend.Prelude


newtype DummyT m a = DummyT { unDummyT :: ExceptT ActionExcept m a }
    deriving (Functor, Applicative, Monad, MonadError ActionExcept)

type Dummy = DummyT Identity

runDummyT :: DummyT m a -> m (Either ActionExcept a)
runDummyT = runExceptT . unDummyT

runDummy :: Dummy a -> Either ActionExcept a
runDummy = runIdentity . runDummyT

notImplemented :: Monad m => String -> String -> DummyT m a
notImplemented meth cl = throwError500 $ unlines
    ["Method ", meth, " from class ", cl, " not implemented for instance Dummy"]

instance Monad m => ActionTempCsvFiles (DummyT m) where
    popTempCsvFile _      = notImplemented "PersistM" "popTempCsvFile"
    cleanupTempCsvFiles _ = notImplemented "PersistM" "cleanupTempCsvFiles"

instance Monad m => ActionLog (DummyT m) where
    logEvent _ = pure ()

instance Monad m => ActionPersist Persist (DummyT m) where
    persistent _ = notImplemented "ActionPersist" "persistent"

instance Monad m => ActionError (DummyT m)

instance Monad m => ActionUserHandler (DummyT m) where
    login _     = pure ()
    logout      = pure ()
    userState _ = notImplemented "ActionUserHandler" "userState"

instance Monad m => ActionM Persist (DummyT m)
