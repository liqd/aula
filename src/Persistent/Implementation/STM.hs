{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeOperators               #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.Implementation.STM
    ( Persist
    , mkRunPersist
    )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, modifyTVar')
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Servant (ServantErr)
import Servant.Server ((:~>)(Nat))

import Types
import Persistent.Api

-- FIXME: Remove
import Test.QuickCheck (generate)

newtype Persist a = Persist (ExceptT ServantErr (ReaderT (TVar AulaData) IO) a)
  deriving (Functor, Applicative, Monad, MonadError ServantErr)

persistIO :: IO a -> Persist a
persistIO = Persist . liftIO

instance GenArbitrary Persist where
    genGen = persistIO . generate

mkRunPersist :: IO (Persist :~> ExceptT ServantErr IO)
mkRunPersist = do
    tvar <- newTVarIO emptyAulaData
    let run (Persist c) = ExceptT $ runExceptT c `runReaderT` tvar
    return $ Nat run

instance MonadIO Persist where  -- TODO: get rid of this.  use persistIO if we must.
    liftIO = persistIO

instance PersistM Persist where
    getDb l = Persist . ExceptT . ReaderT $
        fmap (Right . view l) . atomically . readTVar
    modifyDb l f = Persist . ExceptT . ReaderT $
        \state -> fmap Right . atomically $ modifyTVar' state (l %~ f)

-- TODO: we want tests for all of this!
-- TODO: newtype PersistExcept similar to ActionExcept
