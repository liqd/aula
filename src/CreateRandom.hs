{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module CreateRandom
where

import Thentos.Prelude

import Action
import Persistent
import Types
import Arbitrary ()

-- | Generate one arbitrary item of each type (idea, user, ...)
-- plus one extra user for logging test.
--
-- Note that no user is getting logged in by this code.
genInitialTestDb :: (ActionM m) => m ()
genInitialTestDb = do
    pure ()  -- TODO recover this, but make it deterministic.
    {-
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
    _wildIdea <- addIdea . (,) firstUser . (protoIdeaLocation . ideaLocationMaybeTopicId .~ Nothing) =<< genArbitrary
    topicIdea <- addIdea . (,) user2     =<< genArbitrary
    _topic <- addTopic . (,) firstUser . (protoTopicIdeas .~ [topicIdea ^. _Id]) =<< genArbitrary
    return ()
    -}

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
      , _userRole      = Admin
      , _userPassword  = UserPassInitial ""
      , _userEmail     = Nothing
      }
    uid = AUID 0
    oid = AUID 1
    cUser = _Id .~ uid $ user  -- the user creates himself
    metainfo :: MetaInfo User
    metainfo = mkMetaInfo cUser sometime oid
