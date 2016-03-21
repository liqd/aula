{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | rule: always add (and expect) trailing slashes.
module Frontend.Path
    ( Top(..)
    , Main(..)
    , Space(..)
    , UserPs(..)
    , AdminPs(..)
    , IdeaPath(..)
    , IdeaMode(..)
    )
where

import GHC.Generics
import Thentos.Prelude
import Data.UriPath

import qualified Generics.SOP as SOP

import Types (AUID, Idea, IdeaSpace, IdeaLocation(..), User, Topic, nil, PermissionContext, SchoolClass)

data Top =
    Top
  | Broken  -- FIXME: for keeping track of missing links.  do not leave lying around in production!
  | TopMain Main
  | TopTesting UriPath
  | TopSamples
  | TopStatic UriPath
  deriving Generic

instance SOP.Generic Top

instance HasPath Top where relPath = top

top :: Top -> UriPath
top Top            = nil
top Broken         = "bröken"
top (TopMain p)    = relPath p
top (TopTesting p) = nil </> "testing" <> p
top TopSamples     = nil </> "samples"
top (TopStatic p)  = nil </> "static" <> p

data Main =
    ListSpaces
  | Space IdeaSpace Space
  | ListUsers
  | User (AUID User) UserPs
  | UserSettings
  | Admin AdminPs
  | DelegationEdit
  | DelegationView
  | Imprint
  | Terms
  | Login (Maybe Bool)
  | Logout
  deriving (Generic, Show)

instance SOP.Generic Main

instance HasPath Main where relPath p = main p nil

main :: Main -> UriPath -> UriPath
main ListSpaces       root = root </> "space"
main (Space sid p)    root = space p (root </> "space" </> uriPart sid)
main ListUsers        root = root </> "user"
main (User uid p)     root = user  p (root </> "user" </> uriPart uid)
main UserSettings     root = root </> "user" </> "settings"
main (Admin p)        root = admin p (root </> "admin")
main DelegationEdit   root = root </> "delegation" </> "edit"
main DelegationView   root = root </> "delegation" </> "view"
main Imprint          root = root </> "imprint"
main Terms            root = root </> "terms"
main (Login mb)       root = root </> fromString ("login" <> if fromMaybe True mb then "" else "?status=False")
main Logout           root = root </> "logout"

data Space =
    ListIdeas
  | ViewIdea (AUID Idea)
  | EditIdea (AUID Idea)
  | CreateIdea
  | ListTopics
  | ListTopicIdeas (AUID Topic)
  | ViewTopicIdea (AUID Topic) (AUID Idea)
  | EditTopicIdea (AUID Topic) (AUID Idea)
  | CreateTopicIdea (AUID Topic)
  | ViewTopicIdeasVoting (AUID Topic)
  | ViewTopicIdeasWinning (AUID Topic)
  | ViewTopicDelegations (AUID Topic)
  | CreateTopic
  | CreateTopicDelegation (AUID Topic)
  | MoveIdeasToTopic (AUID Topic)
  deriving (Generic, Show)

instance SOP.Generic Space

-- | FIXME: there are structural similarities of wild ideas and ideas in topic that should be
-- factored out.
space :: Space -> UriPath -> UriPath
space ListIdeas                   root = root </> "ideas"
space (ViewIdea iid)              root = root </> "idea" </> uriPart iid </> "view"
space (EditIdea iid)              root = root </> "idea" </> uriPart iid </> "edit"
space CreateIdea                  root = root </> "idea" </> "create"
space ListTopics                  root = root </> "topic"
space (ListTopicIdeas tid)        root = root </> "topic" </> uriPart tid </> "ideas"
space (ViewTopicIdea tid iid)     root = root </> "topic" </> uriPart tid </> "idea" </> uriPart iid </> "view"
space (EditTopicIdea tid iid)     root = root </> "topic" </> uriPart tid </> "idea" </> uriPart iid </> "edit"
space (CreateTopicIdea tid)       root = root </> "topic" </> uriPart tid </> "idea" </> "create"
space (ViewTopicIdeasVoting tid)  root = root </> "topic" </> uriPart tid </> "ideas" </> "voting"
space (ViewTopicIdeasWinning tid) root = root </> "topic" </> uriPart tid </> "ideas" </> "winning"
space (ViewTopicDelegations tid)  root = root </> "topic" </> uriPart tid </> "delegations"
-- FIXME: "ListTopicIdeas..." for the 3 lines above?
space CreateTopic                 root = root </> "topic" </> "create"
space (MoveIdeasToTopic tid)      root = root </> "topic" </> uriPart tid </> "idea" </> "move"
space (CreateTopicDelegation tid) root = root </> "topic" </> uriPart tid </> "delegation" </> "create"

data UserPs =
    UserIdeas
  | UserDelegations
  deriving (Generic, Show)

instance SOP.Generic UserPs

user :: UserPs -> UriPath -> UriPath
user UserIdeas       = (</> "ideas")
user UserDelegations = (</> "delegations")

data AdminPs =
    AdminDuration
  | AdminQuorum
  | AdminAccess PermissionContext
  | AdminEditUser (AUID User)
  | AdminEditClass SchoolClass
  | AdminEvent
  deriving (Generic, Show)

instance SOP.Generic AdminPs

admin :: AdminPs -> UriPath -> UriPath
admin AdminDuration         path = path </> "duration"
admin AdminQuorum           path = path </> "quorum"
admin (AdminAccess ctx)     path = path </> "access" </> uriPart ctx
admin (AdminEditUser uid)   path = path </> "user" </> uriPart uid </> "edit"
admin (AdminEditClass clss) path = path </> "class" </> uriPart clss </> "edit"
admin AdminEvent            path = path </> "event"

data IdeaPath = IdeaPath IdeaLocation IdeaMode
  deriving (Eq, Ord, Show, Read, Generic)

data IdeaMode =
      IdeaModeList
    | IdeaModeCreate
    | IdeaModeView (AUID Idea)
    | IdeaModeEdit (AUID Idea)
  deriving (Eq, Ord, Show, Read, Generic)

instance HasPath IdeaPath
  where
    relPath (IdeaPath loc mode) = f loc mode
      where
        f (IdeaLocationSpace isp)     = relPath . Space isp . g
        f (IdeaLocationTopic isp tid) = relPath . Space isp . h tid

        g IdeaModeList       = ListIdeas
        g IdeaModeCreate     = CreateIdea
        g (IdeaModeView iid) = ViewIdea iid
        g (IdeaModeEdit iid) = EditIdea iid

        h tid IdeaModeList       = ListTopicIdeas tid
        h tid IdeaModeCreate     = CreateTopicIdea tid
        h tid (IdeaModeView iid) = ViewTopicIdea tid iid
        h tid (IdeaModeEdit iid) = EditTopicIdea tid iid
