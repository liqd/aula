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
    , IdeaMode(..)
    , viewIdea, editIdea, commentIdea, createIdea, listIdeas, listTopicIdeas
    )
where

import GHC.Generics
import Thentos.Prelude
import Data.UriPath

import qualified Generics.SOP as SOP

import Types ( AUID, Idea, IdeaSpace, IdeaLocation(..), User, Topic, nil, PermissionContext
             , SchoolClass, _Id, ideaLocation, topicIdeaSpace)

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
  | IdeaPath IdeaLocation IdeaMode
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
main (IdeaPath l m)   root = ideaPath l m root
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
    ListTopics
  | ViewTopicIdeasVoting (AUID Topic)
  | ViewTopicIdeasWinning (AUID Topic)
  | ViewTopicDelegations (AUID Topic)
  | CreateTopic
  | CreateTopicDelegation (AUID Topic)
  | MoveIdeasToTopic (AUID Topic)
  deriving (Generic, Show)

instance SOP.Generic Space

viewIdea :: Idea -> Main
viewIdea idea = IdeaPath (idea ^. ideaLocation) $ ViewIdea (idea ^. _Id)

editIdea :: Idea -> Main
editIdea idea = IdeaPath (idea ^. ideaLocation) $ EditIdea (idea ^. _Id)

commentIdea :: Idea -> Main
commentIdea idea = IdeaPath (idea ^. ideaLocation) $ CommentIdea (idea ^. _Id)

createIdea :: IdeaLocation -> Main
createIdea loc = IdeaPath loc CreateIdea

listIdeas :: IdeaLocation -> Main
listIdeas loc = IdeaPath loc ListIdeas

listTopicIdeas :: Topic -> Main
listTopicIdeas topic = listIdeas $ IdeaLocationTopic (topic ^. topicIdeaSpace) (topic ^. _Id)

ideaMode :: IdeaMode -> UriPath -> UriPath
ideaMode ListIdeas         root = root </> "ideas"
ideaMode (ViewIdea iid)    root = root </> "idea" </> uriPart iid </> "view"
ideaMode (EditIdea iid)    root = root </> "idea" </> uriPart iid </> "edit"
ideaMode (CommentIdea iid) root = root </> "idea" </> uriPart iid </> "comment"
ideaMode CreateIdea        root = root </> "idea" </> "create"

ideaPath :: IdeaLocation -> IdeaMode -> UriPath -> UriPath
ideaPath loc mode root =
    case loc of
        IdeaLocationSpace isp     -> ideaMode mode $ rootSpace isp
        IdeaLocationTopic isp tid -> ideaMode mode $ rootSpace isp </> "topic" </> uriPart tid
  where
    rootSpace isp = root </> "space" </> uriPart isp

-- | FIXME: there are structural similarities of wild ideas and ideas in topic that should be
-- factored out.
space :: Space -> UriPath -> UriPath
space ListTopics                  root = root </> "topic"
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


data IdeaMode =
      ListIdeas
    | CreateIdea
    | ViewIdea (AUID Idea)
    | EditIdea (AUID Idea)
    | CommentIdea (AUID Idea)
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaMode
