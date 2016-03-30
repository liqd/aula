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

import Control.Exception (bracket)
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Acid (AcidState, Query, Update, closeAcidState, makeAcidic, query, update)
import Data.Acid.Local (openLocalState, createCheckpointAndClose)
import Data.Acid.Memory (openMemoryState)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Servant.Server ((:~>)(Nat))

import Persistent.Api
import Types

-- FIXME: Remove
import Test.QuickCheck (generate)

newtype Persist a = Persist (ReaderT (AcidState AulaData) IO a)
  deriving (Functor, Applicative, Monad)

persistIO :: IO a -> Persist a
persistIO = Persist . liftIO

instance GenArbitrary Persist where
    genGen = persistIO . generate

mkRunPersistGeneric :: (AulaData -> IO (AcidState AulaData))
                    -> (AcidState AulaData -> IO ())
                    -> IO (Persist :~> IO)
mkRunPersistGeneric openState closeState = return $
    Nat (\(Persist c) ->
            bracket (openState emptyAulaData)
                    closeState
                    (c `runReaderT`)
        )

mkRunPersist :: IO (Persist :~> IO)
mkRunPersist = mkRunPersistGeneric openLocalState createCheckpointAndClose

mkRunPersistInMemory :: IO (Persist :~> IO)
mkRunPersistInMemory = mkRunPersistGeneric openMemoryState closeAcidState

instance MonadIO Persist where
    liftIO = persistIO

askDbM :: Query AulaData AulaData
askDbM = ask

getDbM :: Update AulaData AulaData
getDbM = get

putDbM :: AulaData -> Update AulaData ()
putDbM = put

makeAcidic ''AulaData ['askDbM, 'getDbM, 'putDbM]

instance PersistM Persist where
    getDb l = Persist . ReaderT $ fmap (view l) . flip query AskDbM
    modifyDb l f = Persist . ReaderT $ \state -> do
      aulaData <- update state GetDbM
      update state (PutDbM . (l %~ f) $ aulaData)
