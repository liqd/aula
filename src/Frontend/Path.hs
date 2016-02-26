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
    )
where

import Thentos.Prelude
import Data.UriPath

import Types (AUID, User, Topic, Idea, IdeaSpace, nil)

data Top =
    TopMain Main
  | TopTesting UriPath
  | TopSamples
  | TopStatic UriPath

instance HasPath Top where relPath = top

top :: Top -> UriPath
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
  | Login

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
main Login            root = root </> "login"

data Space =
    ListIdeas
  | ViewIdea (AUID Idea)
  | EditIdea (AUID Idea)
  | CreateIdea
  | ListTopics
  | ViewTopicIdeas (AUID Topic)
  | ViewTopicIdeasVoting (AUID Topic)
  | ViewTopicIdeasWinning (AUID Topic)
  | ViewTopicDelegations (AUID Topic)
  | EditTopic (AUID Topic) -- FIXME
  | CreateTopic
  | CreateIdeaInTopic (AUID Topic)
  | CreateTopicDelegation (AUID Topic)
  | MoveIdeasToTopic (AUID Topic)

space :: Space -> UriPath -> UriPath
space ListIdeas                   root = root </> "idea"
space (ViewIdea iid)              root = root </> "idea" </> uriPart iid </> "view"
space (EditIdea iid)              root = root </> "idea" </> uriPart iid </> "edit"
space CreateIdea                  root = root </> "idea" </> "create"
space ListTopics                  root = root </> "topic"
space (ViewTopicIdeas tid)        root = root </> "topic" </> uriPart tid </> "ideas"
space (ViewTopicIdeasVoting tid)  root = root </> "topic" </> uriPart tid </> "ideas" </> "voting"
space (ViewTopicIdeasWinning tid) root = root </> "topic" </> uriPart tid </> "ideas" </> "winning"
space (ViewTopicDelegations tid)  root = root </> "topic" </> uriPart tid </> "delegations"
space (EditTopic tid)             root = root </> "topic" </> uriPart tid </> "edit"
space CreateTopic                 root = root </> "topic" </> "create"
space (CreateIdeaInTopic tid)     root = root </> "topic" </> uriPart tid </> "idea" </> "create"
space (MoveIdeasToTopic tid)      root = root </> "topic" </> uriPart tid </> "idea" </> "move"
space (CreateTopicDelegation tid) root = root </> "topic" </> uriPart tid </> "delegation" </> "create"

data UserPs =
    UserIdeas
  | UserDelegations

user :: UserPs -> UriPath -> UriPath
user UserIdeas       = (</> "ideas")
user UserDelegations = (</> "delegations")

data AdminPs =
    AdminParams
  | AdminAccess
  | AdminUser
  | AdminEvent

admin :: AdminPs -> UriPath -> UriPath
admin AdminParams = (</> "params")
admin AdminAccess = (</> "access")
admin AdminUser   = (</> "user")
admin AdminEvent  = (</> "event")
