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
import Data.Monoid
import Data.Set (Set)
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

putDb :: AulaData -> Update AulaData ()
putDb = put

putDbSpaceSet :: Set IdeaSpace -> Update AulaData ()
putDbSpaceSet = (dbSpaceSet .=)

putDbIdea :: AUID Idea -> Maybe Idea -> Update AulaData ()
putDbIdea i = (dbIdeaMap . at i .=)

putDbUser :: AUID User -> Maybe User -> Update AulaData ()
putDbUser i = (dbUserMap . at i .=)

putDbTopic :: AUID Topic -> Maybe Topic -> Update AulaData ()
putDbTopic i = (dbTopicMap . at i .=)

putDbDelegation :: AUID Delegation -> Maybe Delegation -> Update AulaData ()
putDbDelegation i = (dbDelegationMap . at i .=)

putDbElaborationDuration :: DurationDays -> Update AulaData ()
putDbElaborationDuration = (dbElaborationDuration .=)

putDbVoteDuration :: DurationDays -> Update AulaData ()
putDbVoteDuration = (dbVoteDuration .=)

putDbSchoolQuorum :: Percent -> Update AulaData ()
putDbSchoolQuorum = (dbSchoolQuorum .=)

putDbClassQuorum :: Percent -> Update AulaData ()
putDbClassQuorum = (dbClassQuorum .=)

putDbLastId :: Integer -> Update AulaData ()
putDbLastId = (dbLastId .=)

makeAcidic ''AulaData [ 'askDbM, 'getDbM, 'putDb, 'putDbSpaceSet, 'putDbIdea, 'putDbUser
                      , 'putDbTopic, 'putDbDelegation, 'putDbElaborationDuration
                      , 'putDbVoteDuration, 'putDbSchoolQuorum, 'putDbClassQuorum
                      , 'putDbLastId ]

type ModifyDb a = (a -> a) -> AulaData -> AcidState AulaData -> IO ()

applyAulaEvent :: (UpdateEvent event, EventState event ~ AulaData, EventResult event ~ ())
               => (a -> event)
               -> DbLens a
               -> Traversal' a b -> ModifyDb b
applyAulaEvent event l t f db state = update state (event (r & t %~ f))
  where r = db ^. dbLens l

modifyDbTraversal :: DbTraversal a -> ModifyDb a
modifyDbTraversal l@(ll :.: t) =
    case ll of
        DbAt DbIdeas       i  -> applyAulaEvent (PutDbIdea i) ll t
        DbAt DbUsers       i  -> applyAulaEvent (PutDbUser i) ll t
        DbAt DbTopics      i  -> applyAulaEvent (PutDbTopic i) ll t
        DbAt DbDelegations i  -> applyAulaEvent (PutDbDelegation i) ll t
        DbSpaceSet            -> applyAulaEvent PutDbSpaceSet ll t
        DbElaborationDuration -> applyAulaEvent PutDbElaborationDuration ll t
        DbVoteDuration        -> applyAulaEvent PutDbVoteDuration ll t
        DbSchoolQuorum        -> applyAulaEvent PutDbSchoolQuorum ll t
        DbClassQuorum         -> applyAulaEvent PutDbClassQuorum ll t
        DbLastId              -> applyAulaEvent PutDbLastId ll t
        DbId                  -> updatePutDb
        _                     -> \f db state -> fail ("ACID-STATE TODO " <> show ll) >> updatePutDb f db state
  where
    updatePutDb = \f db state -> update state (PutDb (db & dbTraversal l %~ f))

instance PersistM Persist where
    getDb l = Persist . ExceptT . ReaderT $ fmap (Right . view l) . flip query AskDbM
    modifyDb l f = Persist . ExceptT . ReaderT $ \state -> fmap Right $ do
      db <- update state GetDbM
      modifyDbTraversal l f db state
    getCurrentTimestamp = persistIO getCurrentTimestampIO
    mkRandomPassword = persistIO mkRandomPasswordIO
