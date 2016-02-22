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
import Test.QuickCheck (Arbitrary)
import Thentos.Prelude

import Action
import Frontend.Core
import Persistent
import Types
import Arbitrary ()

type CreateRandom a = "create_random" :> GetH (Frame (ST `Beside` PageShow a))

-- | Create random entities that have 'MetaInfo' in the Aula Action monad.
createRandom
    :: ( Arbitrary a, Show a, Typeable a, HasMetaInfo a
       , ActionPersist m, GenData m)
    => AulaLens (AMap a) -> m (Frame (ST `Beside` PageShow a))
createRandom l = do
   x <- persistent . addDbEntity l =<< genData
   return (Frame frameUserHack (("new " <> (cs . show . typeOf $ x) <> " created.")
                                     `Beside` PageShow x))

-- | Create random entities that have no 'MetaInfo'.  (Currently only 'Set' elements.)
createRandomNoMeta
    :: ( Arbitrary a, Ord a, Show a, Typeable a
       , ActionPersist m, GenData m)
    => AulaLens (Set a) -> m (Frame (ST `Beside` PageShow a))
createRandomNoMeta l = do
   x <- genData
   persistent $ modifyDb l (insert x)
   return (Frame frameUserHack (("new " <> (cs . show . typeOf $ x) <> " created.")
                                     `Beside` PageShow x))

-- | generate one arbitrary item of each type (idea, user, ...)
genInitalTestDb :: Persist ()
genInitalTestDb = do
    _firstUser <- bootstrapUser =<< genData
    _wildIdea <- addIdea =<< genData
    topicIdea <- addIdea =<< genData
    _topic <- addTopic . (protoTopicIdeas .~ [topicIdea ^. _Id]) =<< genData
    return ()
