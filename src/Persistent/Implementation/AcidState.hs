{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Persistent.Implementation.AcidState
    ( mkRunPersistOnDisk
    , mkRunPersistInMemory
    )
where

import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Reader (ask)
import Control.Monad.State (put)
import Data.Acid (AcidState, Query, Update, closeAcidState, makeAcidic, query, update, liftQuery
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

mkRunPersistGeneric :: String -> (AulaData -> IO (AcidState AulaData))
                    -> (AcidState AulaData -> IO ())
                    -> IO RunPersist
mkRunPersistGeneric desc openState closeState = do
  db <- openState emptyAulaData
  let run :: Persist a -> ExceptT PersistExcept IO a
      run (Persist c) = ExceptT $ runExceptT c `runReaderT` db
  pure RunPersist { _rpDesc  = desc
                  , _rpNat   = Nat run
                  , _rpClose = closeState db
                  }

mkRunPersistOnDisk :: Config -> IO RunPersist
mkRunPersistOnDisk cfg =
    mkRunPersistGeneric "acid-state (disk)" (openLocalStateFrom $ cfg ^. dbPath) createCheckpointAndClose

mkRunPersistInMemory :: IO RunPersist
mkRunPersistInMemory = mkRunPersistGeneric "acid-state (memory)" openMemoryState closeAcidState

instance MonadIO Persist where
    liftIO = persistIO

askDbM :: Query AulaData AulaData
askDbM = ask

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

makeAcidic ''AulaData [ 'askDbM, 'putDb, 'putDbSpaceSet, 'putDbIdea, 'putDbUser
                      , 'putDbTopic, 'putDbDelegation, 'putDbElaborationDuration
                      , 'putDbVoteDuration, 'putDbSchoolQuorum, 'putDbClassQuorum
                      , 'putDbLastId ]

type ModifyDb a = (a -> a) -> AcidState AulaData -> IO ()

applyAulaEvent :: (UpdateEvent event, EventState event ~ AulaData, EventResult event ~ ())
               => (a -> event)
               -> DbLens a
               -> Traversal' a b -> ModifyDb b
applyAulaEvent event l t f state = update state $ do  -- TODO: this is not an acid-state transaction.  for once, f can't be serialized!
    db <- liftQuery askDbM
    let r = db ^. dbLens l
    event (r & t %~ f)

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
        -- TODO: remove the following two lines
        DbId                  -> updatePutDb
        _                     -> \f db state -> fail ("ACID-STATE TODO " <> show ll) >> updatePutDb f db state
  where
    updatePutDb = \f db state -> update state (PutDb (db & dbTraversal l %~ f))

instance PersistM Persist where
    getDb l = Persist . ExceptT . ReaderT $ fmap (Right . view l) . flip query AskDbM
    modifyDb l f = Persist . ExceptT . ReaderT $ fmap Right . modifyDbTraversal l f
    getCurrentTimestamp = persistIO getCurrentTimestampIO
    mkRandomPassword = persistIO mkRandomPasswordIO
