{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wall -Wwarn -fno-warn-orphans #-}

module Persistent.Implementation
    ( Persist
    , mkRunPersist
    )
where

#ifdef USE_STM
import Persistent.Implementation.STM
#elif USE_ACID_STATE
import Persistent.Implementation.AcidState
#else
import Persistent.Implementation.AcidState
#endif
