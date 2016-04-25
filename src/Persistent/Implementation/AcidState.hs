{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE GADTs                       #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TupleSections               #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}

-- {-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Persistent.Implementation.AcidState
    ( mkRunPersistOnDisk
    , mkRunPersistInMemory
    , AcidState
    )
where

import Control.Concurrent
import Control.Exception hiding (handle)
import Control.Lens
import Data.Acid
import Data.Acid.Local (createCheckpointAndClose)
import Data.Acid.Memory (openMemoryState)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)

import Config
import Daemon
import Logger
import Persistent.Api
import Persistent.Pure


mkRunPersistGeneric :: String
                    -> (AulaData -> IO (AcidState AulaData, a))
                    -> (AcidState AulaData -> a -> IO ())
                    -> IO RunPersist
mkRunPersistGeneric desc openState closeState = do
    (st, handle) <- openState emptyAulaData
    pure RunPersist { _rpDesc   = desc
                    , _rpQuery  = query st AskDb
                    , _rpUpdate = update st
                    , _rpClose  = closeState st handle
                    }

mkRunPersistOnDisk :: SendLogMsg -> Config -> IO RunPersist
mkRunPersistOnDisk logger cfg =
    mkRunPersistGeneric "acid-state (disk)" opn cls
  where
    opn aulaData = do
        st <- openLocalStateFrom (cfg ^. persistConfig . dbPath) aulaData
        let delay = cfg ^. persistConfig . snapshotIntervalMinutes

        let checkpoint = do
                logger $ LogEntry INFO "[create acid-state checkpoint, archive]"
                createCheckpoint st
                createArchive st
        let logException (SomeException e) = do
                logger . LogEntry ERROR $
                    "error creating checkpoint or archiving changelog: " <> cs (show e)

        let daemon = timeoutDaemon logger "checkpoint" delay checkpoint logException
        tid <- daemon ^. start
        pure (st, tid)

    cls st tid = do
        killThread tid
        createCheckpointAndClose st

mkRunPersistInMemory :: IO RunPersist
mkRunPersistInMemory =
    mkRunPersistGeneric "acid-state (memory)"
        (fmap (, ()) . openMemoryState)
        (\st () -> closeAcidState st)
