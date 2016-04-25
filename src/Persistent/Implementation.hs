{-# LANGUAGE Rank2Types #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.Implementation (mkRunPersist, withPersist, withPersist')
where

import Config
import Control.Exception (finally)
import Control.Lens
import Data.Monoid
import Logger
import Persistent.Api
import Persistent.Implementation.AcidState

withPersist :: SendLogMsg -> Config -> (RunPersist -> IO a) -> IO a
withPersist logger = withPersist' . mkRunPersist logger

-- | A more low-level variant of 'Persistent.Implementation.withPersist' with the implementation
-- explicit as parameter.
withPersist' :: IO RunPersist -> (RunPersist -> IO a) -> IO a
withPersist' mkRunP m = do
    rp@(RunPersist desc _ _ close) <- mkRunP  -- initialization happens here
    putStrLn $ "persistence: " <> desc -- FIXME: use logger for this (or perhaps log in the construction of Action, where we have a logger?)
    m rp `finally` close  -- closing happens here

mkRunPersist :: SendLogMsg -> Config -> IO RunPersist
mkRunPersist logger cfg =
    case cfg ^. persistConfig . persistenceImpl of
        AcidStateInMem  -> mkRunPersistInMemory
        AcidStateOnDisk -> mkRunPersistOnDisk logger cfg
