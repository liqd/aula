{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.Implementation
    ( Persist
    , mkRunPersist
    , mkRunPersistInMemory
    )
where

#ifdef PERSISTENT_ACID_STATE
import Persistent.Implementation.AcidState
#else
import Persistent.Implementation.STM
#endif
