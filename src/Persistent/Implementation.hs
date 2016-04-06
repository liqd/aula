{-# LANGUAGE Rank2Types #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.Implementation (mkRunPersist, withPersist, withPersist')
where

import Config
import Control.Exception (finally)
import Control.Lens
import Data.Monoid
import Persistent.Api
import Persistent.Implementation.AcidState

withPersist :: Config -> (RunPersist -> IO a) -> IO a
withPersist = withPersist' . mkRunPersist

-- | A more low-level variant of 'Persistent.Implementation.withPersist' with the implementation
-- explicit as parameter.
withPersist' :: IO RunPersist -> (RunPersist -> IO a) -> IO a
withPersist' mkRunP m = do
    rp@(RunPersist desc _ close) <- mkRunP  -- initialization happens here
    putStrLn $ "persistence: " <> desc -- FIXME: use logger for this (or perhaps log in the construction of Action, where we have a logger?)
    m rp `finally` close  -- closing happens here

-- TODO: give this a different name.
mkRunPersist :: Config -> IO RunPersist
mkRunPersist cfg =
    case cfg ^. persistenceImpl of
        AcidStateInMem  -> mkRunPersistInMemory
        AcidStateOnDisk -> mkRunPersistOnDisk cfg
