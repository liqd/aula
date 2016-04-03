{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE UndecidableInstances            #-}
{-# LANGUAGE GADTs                       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TupleSections               #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE ViewPatterns                #-}

-- {-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Persistent.Implementation.AcidState
    ( mkRunPersistOnDisk
    , mkRunPersistInMemory
    )
where

import Control.Exception (assert)
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except  -- (MonadError)
import Control.Monad.Trans.Except  -- (ExceptT(ExceptT), runExceptT)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Acid  -- (AcidState, Query, Update, closeAcidState, makeAcidic, query, update)
import Data.Acid.Core
import Data.Acid.Local (openLocalStateFrom, createCheckpointAndClose)
import Data.Acid.Memory (openMemoryState)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Servant.Server ((:~>)(Nat))

import Config
import Persistent.Pure
import Persistent.Idiom
import Persistent.Api
import Types


mkRunPersistGeneric :: String
                    -> (AulaData -> IO (AcidState AulaData))
                    -> (AcidState AulaData -> IO ())
                    -> IO RunPersist
mkRunPersistGeneric desc openState closeState = do
    db <- openState emptyAulaData
    pure RunPersist { _rpDesc  = desc
                    , _rpQNat  = _  -- \(AQuery q) -> _  -- query  db _ -- <$> q
                    , _rpUNat  = _  -- fmap (update db)
                    , _rpClose = closeState db
                    }

mkRunPersistOnDisk :: Config -> IO RunPersist
mkRunPersistOnDisk cfg =
    mkRunPersistGeneric "acid-state (disk)"
        (openLocalStateFrom $ cfg ^. dbPath) createCheckpointAndClose

mkRunPersistInMemory :: IO RunPersist
mkRunPersistInMemory =
    mkRunPersistGeneric "acid-state (memory)"
        openMemoryState closeAcidState
