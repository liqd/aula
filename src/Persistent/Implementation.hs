{-# LANGUAGE Rank2Types #-}

-- {-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.Implementation (mkRunPersist)
where

import Control.Lens
import Config
import Persistent.Api
import Persistent.Pure
import Persistent.Implementation.AcidState
import Types

mkRunPersist :: Config -> IO RunPersist
mkRunPersist cfg =
    case cfg ^. persistenceImpl of
        AcidStateInMem  -> mkRunPersistInMemory
        AcidStateOnDisk -> mkRunPersistOnDisk cfg
