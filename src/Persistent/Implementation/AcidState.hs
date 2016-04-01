{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Persistent.Implementation.AcidState
    ( Persist
    , mkRunPersist
    , mkRunPersistInMemory
    )
where

import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Acid (AcidState, Query, Update, closeAcidState, makeAcidic, query, update)
import Data.Acid.Local (openLocalStateFrom, createCheckpointAndClose)
import Data.Acid.Memory (openMemoryState)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Servant.Server ((:~>)(Nat))

import Config
import Persistent.Api
import Types

-- FIXME: Remove
import Test.QuickCheck (generate)

newtype Persist a = Persist (ExceptT PersistExcept (ReaderT (AcidState AulaData) IO) a)
  deriving (Functor, Applicative, Monad, MonadError PersistExcept)

persistIO :: IO a -> Persist a
persistIO = Persist . liftIO

instance GenArbitrary Persist where
    genGen = persistIO . generate

mkRunPersistGeneric :: (AulaData -> IO (AcidState AulaData))
                    -> (AcidState AulaData -> IO ())
                    -> IO (Persist :~> ExceptT PersistExcept IO, IO ())
mkRunPersistGeneric openState closeState = do
  db <- openState emptyAulaData
  let rp = Nat (\(Persist c) -> ExceptT $ runExceptT c `runReaderT` db)
  return (rp, closeState db)

mkRunPersist :: Config -> IO (Persist :~> ExceptT PersistExcept IO, IO ())
mkRunPersist cfg = do
    logger cfg "persistence: acid-state (disk)"
    mkRunPersistGeneric (openLocalStateFrom $ cfg ^. dbPath) createCheckpointAndClose

mkRunPersistInMemory :: Config -> IO (Persist :~> ExceptT PersistExcept IO, IO ())
mkRunPersistInMemory cfg = do
    logger cfg "persistence: acid-state (memory)"
    mkRunPersistGeneric openMemoryState closeAcidState

instance MonadIO Persist where
    liftIO = persistIO

askDbM :: Query AulaData AulaData
askDbM = ask

getDbM :: Update AulaData AulaData
getDbM = get

-- FIXME:
-- putDbM :: DbField a -> a -> Update AulaData ()
-- putDbM l = (dbFieldTraversal l .=)
--
-- First step
-- putDbM :: DbField Idea -> Idea -> Update AulaData ()
-- putDbM l = (dbFieldTraversal l .=)

putDbM :: AulaData -> Update AulaData ()
putDbM = put

makeAcidic ''AulaData ['askDbM, 'getDbM, 'putDbM]

instance PersistM Persist where
    getDb l = Persist . ExceptT . ReaderT $ fmap (Right . view l) . flip query AskDbM
    modifyDb l f = Persist . ExceptT . ReaderT $ \state -> fmap Right $ do
      db <- update state GetDbM
      -- FIXME update state (PutDbM l (db ^? dbFieldTraversal l %~ _Just . f))
      update state (PutDbM (db & dbFieldTraversal l %~ f))

    getCurrentTimestamp = persistIO getCurrentTimestampIO
    mkRandomPassword = persistIO mkRandomPasswordIO
