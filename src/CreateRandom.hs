{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
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
    :: ( Arbitrary (Proto a), Show a, FromProto a, Typeable a, HasMetaInfo a
       , ActionPersist m, GenArbitrary m)
    => AulaLens (AMap a) -> m (Frame (ST `Beside` PageShow a))
createRandom l = do
   x <- persistent . addDb l =<< genArbitrary
   return (Frame frameUserHack (("new " <> (cs . show . typeOf $ x) <> " created.")
                                     `Beside` PageShow x))

-- | Create random entities that have no 'MetaInfo'.  (Currently only 'Set' elements.)
createRandomNoMeta
    :: ( Arbitrary a, Ord a, Show a, Typeable a
       , ActionPersist m, GenArbitrary m)
    => AulaLens (Set a) -> m (Frame (ST `Beside` PageShow a))
createRandomNoMeta l = do
   x <- genArbitrary
   persistent $ modifyDb l (insert x)
   return (Frame frameUserHack (("new " <> (cs . show . typeOf $ x) <> " created.")
                                     `Beside` PageShow x))

-- | generate one arbitrary item of each type (idea, user, ...)
genInitalTestDb :: Persist ()
genInitalTestDb = do
    _firstUser <- bootstrapUser =<< genArbitrary
    _wildIdea <- addIdea =<< genArbitrary
    topicIdea <- addIdea =<< genArbitrary
    _topic <- addTopic . (protoTopicIdeas .~ [topicIdea ^. _Id]) =<< genArbitrary
    return ()

frameUserHack :: User
frameUserHack = User
    { _userMeta      = frameUserMetaInfo
    , _userLogin     = "VorNam"
    , _userFirstName = "Vorname"
    , _userLastName  = "Name"
    , _userAvatar    = "https://avatar.com"
    , _userGroups    = nil
    , _userPassword  = EncryptedPass ""
    , _userEmail     = Nothing
    }
  where
    sometime = Timestamp $ read "2016-02-20 17:09:23.325662 UTC"

    frameUserMetaInfo :: MetaInfo User
    frameUserMetaInfo= MetaInfo
        { _metaId              = AUID 1
        , _metaCreatedBy       = AUID 0
        , _metaCreatedByLogin  = nil  -- FIXME: take from 'u'
        , _metaCreatedByAvatar = nil  -- FIXME: take from 'u'
        , _metaCreatedAt       = sometime
        , _metaChangedBy       = AUID 0
        , _metaChangedAt       = sometime
        }
