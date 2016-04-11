{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module CreateRandom
where

import Thentos.Prelude

import Action
import Persistent
import Persistent.Api
import Types
import Arbitrary ()

-- | Generate one arbitrary item of each type (idea, user, ...)
-- plus one extra user for logging test.
--
-- Note that no user is getting logged in by this code.
genInitialTestDb :: (ActionPersist m) => m ()
genInitialTestDb = do
    update $ AddIdeaSpaceIfNotExists SchoolSpace
    update . AddIdeaSpaceIfNotExists $ ClassSpace (SchoolClass 2016 "7a")
    update . AddIdeaSpaceIfNotExists $ ClassSpace (SchoolClass 2016 "7b")
    update . AddIdeaSpaceIfNotExists $ ClassSpace (SchoolClass 2016 "8a")

    user1 <- update $ AddFirstUser sometime ProtoUser
        { _protoUserLogin     = Just "admin"
        , _protoUserFirstName = "A."
        , _protoUserLastName  = "Admin"
        , _protoUserRole      = Admin
        , _protoUserPassword  = Just (UserPassInitial "pssst")
        , _protoUserEmail     = Nothing
        }

    user2 <- update (AddUser (UserPassInitial "geheim") (EnvWith user1 sometime ProtoUser
        { _protoUserLogin     = Just "godmin"
        , _protoUserFirstName = "G."
        , _protoUserLastName  = "Godmin"
        , _protoUserRole      = Admin
        , _protoUserPassword  = Just (UserPassInitial "geheim")
        , _protoUserEmail     = Nothing
        }))

    _wildIdea <- update $ AddIdea (EnvWith user1 sometime ProtoIdea
            { _protoIdeaTitle    = "wild-idea-title"
            , _protoIdeaDesc     = Markdown "wild-idea-desc"
            , _protoIdeaCategory = Just CatRules
            , _protoIdeaLocation = IdeaLocationSpace SchoolSpace
            })

    topicIdea <- update $ AddIdea (EnvWith user2 sometime ProtoIdea
            { _protoIdeaTitle    = "topic-idea-title"
            , _protoIdeaDesc     = Markdown "topic-idea-desc"
            , _protoIdeaCategory = Just CatRules
            , _protoIdeaLocation = IdeaLocationSpace SchoolSpace
            })

    topic <- update $ AddTopic (EnvWith user1 sometime ProtoTopic
        { _protoTopicTitle     = "topic-title"
        , _protoTopicDesc      = Markdown "topic-desc"
        , _protoTopicImage     = ""
        , _protoTopicIdeaSpace = SchoolSpace
        , _protoTopicIdeas     = []
        , _protoTopicRefinDays = sometime
        })

    update $ MoveIdeasToLocation [topicIdea ^. _Id] (topicIdeaLocation topic)

    return ()


sometime :: Timestamp
sometime = Timestamp $ read "2016-02-20 17:09:23.325662 UTC"


-- FIXME
frameUserHack :: User
frameUserHack = user
  where
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
