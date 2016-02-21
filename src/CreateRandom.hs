{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module CreateRandom
where

import Data.Typeable (typeOf)
import Test.QuickCheck (Arbitrary, generate, arbitrary)
import Thentos.Prelude

import Action
import Frontend.Core
import Persistent
import Types

-- | Create random entities as in the Aula Action monad.
createRandom
    :: ( Arbitrary a, Show a, Typeable a, HasMetaInfo a
       , ActionPersist m, MonadIO m)
    => AulaLens (AMap a) -> m (Frame (ST `Beside` PageShow a))
createRandom l = do
   px <- liftIO $ generate arbitrary
   x <- persistent $ addDbEntity l px
   return (Frame frameUserHack (("new " <> (cs . show . typeOf $ x) <> " created.")
                                     `Beside` PageShow x))
