{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module CreateRandom
where

import Thentos.Prelude

import Persistent
import Frontend.Core
import Frontend.Html
import Test.QuickCheck

createRandom :: (MonadIO m, Arbitrary a, Show a) => ST -> AulaLens [a] -> m (Frame (ST `Beside` PageShow a))
createRandom s l = liftIO $ do
   x <- generate arbitrary
   runPersist $ addDbEntity l x
   return (Frame (("new " <> s <> " created.") `Beside` PageShow x))
