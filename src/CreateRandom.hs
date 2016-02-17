{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module CreateRandom
where

import Thentos.Prelude

import Action
import Types
import Persistent
import Frontend.Core
import Test.QuickCheck

-- | Create random entities as in the Aula Action monad.
createRandom :: (Arbitrary a, Show a, HasMetaInfo a) =>
                ST -> AulaLens (AMap a) -> Action (Frame (ST `Beside` PageShow a))
createRandom s l = do
   px <- actionIO $ generate arbitrary
   x <- persistent $ addDbEntity l px
   return (Frame frameUserHack (("new " <> s <> " created.") `Beside` PageShow x))
