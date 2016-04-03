{-# LANGUAGE Rank2Types #-}

-- {-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.Implementation (mkRunPersist, withPersist, withPersist')
where

import Control.Lens
import Config
import Persistent.Api
import Persistent.Pure
import Persistent.Implementation.AcidState
import Types

withPersist :: Config -> (forall r. (PersistM r, GenArbitrary r) => RunPersistNat IO r -> IO a) -> IO a
withPersist cfg = withPersist' (mkRunPersist cfg)

-- | A more low-level variant of 'Persistent.Implementation.withPersist' with the implementation
-- explicit as parameter.
withPersist' :: IO RunPersist -> (forall r. (PersistM r, GenArbitrary r) => RunPersistNat IO r -> IO a) -> IO a
withPersist' mkRunP m = do
    RunPersist desc rp close <- mkRunP -- initialization happens here
    putStrLn $ "persistence: " <> desc -- FIXME: use logger for this (or perhaps log in the construction of Action, where we have a logger?)
    m rp `finally` close               -- closing happens here

mkRunPersist :: Config -> IO RunPersist
mkRunPersist cfg =
    case cfg ^. persistenceImpl of
        AcidStateInMem  -> mkRunPersistInMemory
        AcidStateOnDisk -> mkRunPersistOnDisk cfg
