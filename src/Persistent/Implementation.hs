{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.Implementation
    ( Persist
    , mkRunPersist
    , mkRunPersistInMemory
    )
where

#ifdef PERSISTENT_STM
import Persistent.Implementation.STM
#else
import Persistent.Implementation.AcidState
#endif
