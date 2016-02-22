{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module CreateRandom
where

import Data.Typeable (typeOf)
import Data.Set (Set, insert)
import Servant
import Test.QuickCheck (Arbitrary, generate, arbitrary)
import Thentos.Prelude

import Action
import Frontend.Core
import Persistent
import Types
import Arbitrary ()

type CreateRandom a = "create_random" :> GetH (Frame (ST `Beside` PageShow a))

genArbitrary :: Arbitrary a => Persist a
genArbitrary = persistIO $ generate arbitrary

-- | Create random entities that have 'MetaInfo' in the Aula Action monad.
createRandom
    :: ( Arbitrary a, Show a, Typeable a, HasMetaInfo a
       , ActionPersist m, ActionIO m)
    => AulaLens (AMap a) -> m (Frame (ST `Beside` PageShow a))
createRandom l = do
   x <- persistent $ addDbEntity l =<< genArbitrary
   return (Frame frameUserHack (("new " <> (cs . show . typeOf $ x) <> " created.")
                                     `Beside` PageShow x))

-- | Create random entities that have no 'MetaInfo'.  (Currently only 'Set' elements.)
createRandomNoMeta
    :: ( Arbitrary a, Ord a, Show a, Typeable a
       , ActionPersist m, ActionIO m)
    => AulaLens (Set a) -> m (Frame (ST `Beside` PageShow a))
createRandomNoMeta l = do
   x <- actionIO $ generate arbitrary
   persistent $ modifyDb l (insert x)
   return (Frame frameUserHack (("new " <> (cs . show . typeOf $ x) <> " created.")
                                     `Beside` PageShow x))

-- | generate one arbitrary item of each type (idea, user, ...)
genInitalDb :: Persist ()
genInitalDb = do
    _firstUser <- bootstrapUser =<< genArbitrary
    _wildIdea <- addIdea =<< genArbitrary
    topicIdea <- addIdea =<< genArbitrary
    _topic <- addTopic . (protoTopicIdeas .~ [topicIdea ^. _Id]) =<< genArbitrary
    return ()
