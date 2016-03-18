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
       , ActionUserHandler m, ActionPersist r m, GenArbitrary m)
    => AulaLens (AMap a) -> m (Frame (ST `Beside` PageShow a))
createRandom l = do
   cUser <- currentUser
   x <- persistent . addDb l . (,) cUser =<< genArbitrary
   return (Frame frameUserHack (("new " <> (cs . show . typeOf $ x) <> " created.")
                                     `Beside` PageShow x))

-- | Create random entities that have no 'MetaInfo'.  (Currently only 'Set' elements.)
createRandomNoMeta
    :: ( Arbitrary a, Ord a, Show a, Typeable a
       , ActionPersist r m, GenArbitrary m)
    => AulaLens (Set a) -> m (Frame (ST `Beside` PageShow a))
createRandomNoMeta l = do
   x <- genArbitrary
   persistent $ modifyDb l (insert x)
   return (Frame frameUserHack (("new " <> (cs . show . typeOf $ x) <> " created.")
                                     `Beside` PageShow x))

-- | Generate one arbitrary item of each type (idea, user, ...)
-- plus one extra user for logging test.
--
-- Note that no user is getting logged in by this code.
genInitialTestDb :: (PersistM m, GenArbitrary m) => m ()
genInitialTestDb = do
    addIdeaSpaceIfNotExists SchoolSpace
    addIdeaSpaceIfNotExists $ ClassSpace (SchoolClass 2016 "7a")
    addIdeaSpaceIfNotExists $ ClassSpace (SchoolClass 2016 "7b")
    addIdeaSpaceIfNotExists $ ClassSpace (SchoolClass 2016 "8a")
    protoU <- genArbitrary
    firstUser <- addFirstUser $ protoU & protoUserLogin    ?~ UserLogin "admin"
                                       & protoUserPassword ?~ UserPassInitial "pssst"
    protoU2 <- genArbitrary
    user2 <- addUser (firstUser,
                      protoU2 & protoUserLogin ?~ UserLogin "admin2"
                              & protoUserPassword ?~ UserPassInitial "pssst2")
    _wildIdea <- addIdea . (,) firstUser =<< genArbitrary
    topicIdea <- addIdea . (,) user2     =<< genArbitrary
    _topic <- addTopic . (,) firstUser . (protoTopicIdeas .~ [topicIdea ^. _Id]) =<< genArbitrary
    return ()

-- FIXME
frameUserHack :: User
frameUserHack = user
  where
    sometime = Timestamp $ read "2016-02-20 17:09:23.325662 UTC"
    user = User
      { _userMeta      = metainfo
      , _userLogin     = "VorNam"
      , _userFirstName = "Vorname"
      , _userLastName  = "Name"
      , _userAvatar    = Nothing
      , _userGroups    = nil
      , _userPassword  = UserPassInitial ""
      , _userEmail     = Nothing
      }
    uid = AUID 0
    oid = AUID 1
    cUser = _Id .~ uid $ user  -- the user creates himself
    metainfo :: MetaInfo User
    metainfo = mkMetaInfo cUser sometime oid
