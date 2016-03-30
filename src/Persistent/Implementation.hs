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
import Persistent.Implementation.ACID_STATE
#else
import Persistent.Implementation.STM
#endif
