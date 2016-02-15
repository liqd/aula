{-# OPTIONS_GHC -Werror -Wall #-}

module Persistent (module Api.Persistent, runPersist)
where

import Servant (unNat)
import Api.Persistent
import System.IO.Unsafe (unsafePerformIO)

-- | FIXME: this should be in moved to a state object that is passed down from 'runFrontend'.
runPersist :: Persist a -> IO a
runPersist = unNat $ unsafePerformIO mkRunPersist
