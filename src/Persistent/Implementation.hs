{-# LANGUAGE Rank2Types #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.Implementation (mkRunPersist, withPersist, withPersist')
where

import Config
import Control.Exception (finally)
import Control.Lens
import Data.String.Conversions
import Logger
import Types.Log(LogLevel(INFO))
import Persistent.Api
import Persistent.Implementation.AcidState

withPersist :: Config -> (RunPersist -> IO a) -> IO a
withPersist config = withPersist' config (mkRunPersist config)

-- | A more low-level variant of 'Persistent.Implementation.withPersist' with the implementation
-- explicit as parameter.
withPersist' :: Config -> IO RunPersist -> (RunPersist -> IO a) -> IO a
withPersist' cfg mkRunP m = do
    let logger = aulaLog (cfg ^. logging) . LogEntry INFO . cs
    rp@(RunPersist desc _ _ close) <- mkRunP  -- initialization happens here
    logger $ "persistence: " <> desc
    m rp `finally` close  -- closing happens here

mkRunPersist :: Config -> IO RunPersist
mkRunPersist cfg =
    case cfg ^. persist . persistenceImpl of
        AcidStateInMem  -> mkRunPersistInMemory
        AcidStateOnDisk -> mkRunPersistOnDisk cfg
