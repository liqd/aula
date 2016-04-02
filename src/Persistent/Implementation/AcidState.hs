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
import Data.Acid (AcidState, Query, Update, closeAcidState, makeAcidic, query, update
                 ,EventResult, EventState, UpdateEvent)
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

getDbM :: Update AulaData AulaData  -- TODO: i think there is a way in acid-state to cast queries
                                    -- into updates.  using that may make this unnecessary.
getDbM = get

-- TODO:
-- putDbM :: DbField a -> a -> Update AulaData ()
-- putDbM l = (dbFieldTraversal l .=)
--
-- First step
-- putDbM :: DbField Idea -> Idea -> Update AulaData ()
-- putDbM l = (dbFieldTraversal l .=)

putDbIdea :: AUID Idea -> Maybe Idea -> Update AulaData ()
putDbIdea i = (dbIdeaMap . at i .=)

putDbUser :: AUID User -> Maybe User -> Update AulaData ()
putDbUser i = (dbUserMap . at i .=)

putDbTopic :: AUID Topic -> Maybe Topic -> Update AulaData ()
putDbTopic i = (dbTopicMap . at i .=)

putDb :: AulaData -> Update AulaData ()
putDb = put

makeAcidic ''AulaData ['askDbM, 'getDbM, 'putDb, 'putDbIdea, 'putDbUser, 'putDbTopic]

type ModifyDbField a = DbField a -> (a -> a) -> AulaData -> AcidState AulaData -> IO ()

applyAulaEvent :: (UpdateEvent event, EventState event ~ AulaData, EventResult event ~ ())
               => (a -> event) -> ModifyDbField a
applyAulaEvent event l f db state =
    case db ^? dbFieldTraversal l of
        Just r  -> update state (event $ f r)
        Nothing -> pure ()

modifyDbField :: ModifyDbField a
modifyDbField l =
    case l of
        DbAt DbIdeas  i -> applyAulaEvent (PutDbIdea  i) l
        DbAt DbUsers  i -> applyAulaEvent (PutDbUser  i) l
        DbAt DbTopics i -> applyAulaEvent (PutDbTopic i) l
        _               -> \f db state -> update state (PutDb (db & dbFieldTraversal l %~ f))
        -- TODO@mf: is this all?  how?

instance PersistM Persist where
    getDb l = Persist . ExceptT . ReaderT $ fmap (Right . view l) . flip query AskDbM
    modifyDb l f = Persist . ExceptT . ReaderT $ \state -> fmap Right $ do
      db <- update state GetDbM
      modifyDbField l f db state
    getCurrentTimestamp = persistIO getCurrentTimestampIO
    mkRandomPassword = persistIO mkRandomPasswordIO
