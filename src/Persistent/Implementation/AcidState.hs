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

import Control.Lens
import Data.Acid
import Data.Acid.Local (createCheckpointAndClose)
import Data.Acid.Memory (openMemoryState)

import Config
import Persistent.Pure
import Persistent.Api


mkRunPersistGeneric :: String
                    -> (AulaData -> IO (AcidState AulaData))
                    -> (AcidState AulaData -> IO ())
                    -> IO RunPersist
mkRunPersistGeneric desc openState closeState = do
    st <- openState emptyAulaData
    pure RunPersist { _rpDesc   = desc
                    , _rpQuery  = query st AskDb
                    , _rpUpdate = update st
                    , _rpClose  = closeState st
                    }

mkRunPersistOnDisk :: Config -> IO RunPersist
mkRunPersistOnDisk cfg =
    mkRunPersistGeneric "acid-state (disk)"
        (openLocalStateFrom $ cfg ^. dbPath) createCheckpointAndClose

mkRunPersistInMemory :: IO RunPersist
mkRunPersistInMemory =
    mkRunPersistGeneric "acid-state (memory)"
        openMemoryState closeAcidState
