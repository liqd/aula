{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeOperators               #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.Implementation.STM (mkRunPersistSTM) where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, modifyTVar')
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Servant.Server ((:~>)(Nat))

import Persistent.Api
import Types

-- FIXME: Remove
import Test.QuickCheck (generate)

newtype Persist a = Persist (ExceptT PersistExcept (ReaderT (TVar AulaData) IO) a)
  deriving (Functor, Applicative, Monad, MonadError PersistExcept)

persistIO :: IO a -> Persist a
persistIO = Persist . liftIO

instance GenArbitrary Persist where
    genGen = persistIO . generate

mkRunPersistSTM :: IO RunPersist
mkRunPersistSTM = do
    tvar <- newTVarIO emptyAulaData
    let run (Persist c) = ExceptT $ runExceptT c `runReaderT` tvar
    pure RunPersist { _rpDesc  = "STM (ephemeral)"
                    , _rpNat   = Nat run
                    , _rpClose = pure ()
                    }

instance PersistM Persist where
    getDb l = Persist . ExceptT . ReaderT $
        fmap (Right . view l) . atomically . readTVar
    modifyDb l f = Persist . ExceptT . ReaderT $
        \state -> fmap Right . atomically $ modifyTVar' state (l %~ f)

    getCurrentTimestamp = persistIO getCurrentTimestampIO
    mkRandomPassword = persistIO mkRandomPasswordIO

-- FIXME: we want tests for all the exception mechanics!
