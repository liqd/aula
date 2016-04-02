{-# LANGUAGE Rank2Types #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.Implementation (mkRunPersist, withPersist)
where

import Control.Lens
import Config
import Persistent.Api
import Persistent.Implementation.AcidState
import Persistent.Implementation.STM
import Types

withPersist :: Config -> (forall r. (PersistM r, GenArbitrary r) => RunPersistNat IO r -> IO a) -> IO a
withPersist cfg = withPersist' (mkRunPersist cfg)

mkRunPersist :: Config -> IO RunPersist
mkRunPersist cfg =
    case cfg ^. persistenceImpl of
        STM             -> mkRunPersistSTM
        AcidStateInMem  -> mkRunPersistInMemory
        AcidStateOnDisk -> mkRunPersistOnDisk cfg
