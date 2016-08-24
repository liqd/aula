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
    , mkRunPersistInMemoryWithState
    , AcidState
    )
where

import Control.Concurrent
import Control.Exception (SomeException(SomeException), handle)
import Control.Lens
import Data.Acid
import Data.Acid.Local (createCheckpointAndClose)
import Data.Acid.Memory (openMemoryState)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import System.Exit

import Config
import Daemon
import Logger
import Persistent.Api
import Persistent.Pure
import Types.Log (LogLevel(..))


mkRunPersistGeneric :: String
                    -> (AulaData -> IO (AcidState AulaData, a))
                    -> (AcidState AulaData -> a -> IO ())
                    -> AulaData
                    -> IO RunPersist
mkRunPersistGeneric desc openState closeState initialState = do
    (st, h) <- openState initialState
    pure RunPersist { _rpDesc   = desc
                    , _rpQuery  = query st AskDb
                    , _rpUpdate = update st
                    , _rpClose  = closeState st h
                    }

mkRunPersistOnDisk :: (LogEntry -> IO ()) -> Config -> IO RunPersist
mkRunPersistOnDisk logger cfg =
    mkRunPersistGeneric "acid-state (disk)" opn cls emptyAulaData
  where
    opn aulaData = do
        st <- explainException $ openLocalStateFrom (cfg ^. persist . dbPath) aulaData
        let delay = cfg ^. persist . snapshotInterval
            checkpoint = createCheckpoint st >> createArchive st
            daemon = timeoutDaemon' logger "create acid-state checkpoint, archive" delay checkpoint
        tid <- daemon ^. start
        pure (st, tid)

    cls st tid = do
        killThread tid
        createCheckpointAndClose st

    explainException :: IO a -> IO a
    explainException = handle $ \(SomeException e) -> do
        let msg = "openLocalStateFrom failed: " <> show e
        logger . LogEntry ERROR . cs $ msg
        putStrLn msg  -- if we write this to stderr and a `print` logger is running, the
                      -- output will be interleaved byte-by-byte.
        exitWith $ ExitFailure 1

mkRunPersistInMemory :: IO RunPersist
mkRunPersistInMemory =
    mkRunPersistGeneric "acid-state (memory)"
        (fmap (, ()) . openMemoryState)
        (\st () -> closeAcidState st)
        emptyAulaData

mkRunPersistInMemoryWithState :: AulaData -> IO RunPersist
mkRunPersistInMemoryWithState =
    mkRunPersistGeneric "acid-state (memory)"
        (fmap (, ()) . openMemoryState)
        (\st () -> closeAcidState st)
