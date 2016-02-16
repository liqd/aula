{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module CreateRandom
where

import Thentos.Prelude

import Types
import Persistent
import Frontend.Core
import Test.QuickCheck

createRandom :: (MonadIO m, Arbitrary a, Show a, HasMetaInfo a) =>
                ST -> AulaLens (AMap a) -> m (Frame (ST `Beside` PageShow a))
createRandom s l = liftIO $ do
   px <- generate arbitrary
   x <- runPersist $ addDbEntity l px
   return (Frame (("new " <> s <> " created.") `Beside` PageShow x))
