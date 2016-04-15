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
import Control.Lens
import Control.Monad
import Data.Acid
import Data.Acid.Local (createCheckpointAndClose)
import Data.Acid.Memory (openMemoryState)

import Config
import Persistent.Pure
import Persistent.Api


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

mkRunPersistOnDisk :: Config -> IO RunPersist
mkRunPersistOnDisk cfg =
    mkRunPersistGeneric "acid-state (disk)" op cl
  where
    op aulaData = do
        st <- openLocalStateFrom (cfg ^. persistConfig . dbPath) aulaData
        tid <- forkIO . forever $ do
            logger cfg "[create acid-state checkpoint, archive]"
            createCheckpoint st
            createArchive st
            threadDelay (cfg ^. persistConfig . snapshotIntervalMinutes)
            -- FIXME: better logging, handle exceptions
        pure (st, tid)

    cl st tid = do
        killThread tid
        createCheckpointAndClose st

mkRunPersistInMemory :: IO RunPersist
mkRunPersistInMemory =
    mkRunPersistGeneric "acid-state (memory)"
        (\aulaData -> (, ()) <$> openMemoryState aulaData)
        (\st () -> closeAcidState st)
