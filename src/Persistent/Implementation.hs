{-# LANGUAGE Rank2Types #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.Implementation (mkRunPersist, withPersist)
where

import Control.Lens
import Config
import Persistent.Pure (AulaData)
import Persistent.Api
import Persistent.Implementation.AcidState

withPersist :: Config -> (AcidState AulaData -> IO a) -> IO a
withPersist = withPersist' . mkRunPersist

mkRunPersist :: Config -> IO RunPersist
mkRunPersist cfg =
    case cfg ^. persistenceImpl of
        AcidStateInMem  -> mkRunPersistInMemory
        AcidStateOnDisk -> mkRunPersistOnDisk cfg
