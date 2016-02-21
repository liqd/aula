{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module CreateRandom
where

import Thentos.Prelude
import Test.QuickCheck (Arbitrary, generate, arbitrary)

import Action
import Frontend.Core
import Persistent
import Types

-- | Create random entities as in the Aula Action monad.
createRandom
    :: ( Arbitrary a, Show a, HasMetaInfo a
       , ActionPersist m, MonadIO m)
    => ST -> AulaLens (AMap a) -> m (Frame (ST `Beside` PageShow a))
createRandom s l = do
   px <- liftIO $ generate arbitrary
   x <- persistent $ addDbEntity l px
   return (Frame frameUserHack (("new " <> s <> " created.") `Beside` PageShow x))
