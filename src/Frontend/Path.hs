{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | rule: always add (and expect) trailing slashes.
module Frontend.Path
    ( path, href_, (</>)
    , UriPath
    , Top(..)
    , Main(..)
    , Space(..)
    , UserPs(..)
    , AdminPs(..)
    )
where

import Thentos.Prelude
import Thentos.Types ((<//>))

import qualified Lucid

import Types (AUID(AUID), User, Topic, Idea)


type UriPath = ST

(</>) :: UriPath -> UriPath -> UriPath
(</>) = (<//>)

href_ :: HasPath p => p -> Lucid.Attribute
href_ = Lucid.href_ . path


class HasPath p where
    path :: p -> UriPath
    path = ("/" </>) . relPath

    relPath :: p -> UriPath

data Top =
    TopMain Main
  | TopTesting UriPath
  | TopSamples
  | TopStatic

instance HasPath Top where relPath = top

top :: Top -> UriPath
top (TopMain p)    = path p
top (TopTesting p) = "testing/" </> p
top TopSamples     = "samples/"
top TopStatic      = "static/"

data Main =
    SpaceAll
  | SpaceOne ST Space
  | UserAll
  | UserOne (AUID User) UserPs
  | UserSettings
  | Admin AdminPs
  | DelegationEdit
  | DelegationView
  | Imprint
  | Terms
  | Login

instance HasPath Main where relPath = main

main :: Main -> UriPath
main SpaceAll               = "space/"
main (SpaceOne sid p)       = "space/" </> sid </> space p
main UserAll                = "user/"
main (UserOne (AUID uid) p) = "user/" </> cs (show uid) </> user p
main UserSettings           = "user/settins/"
main (Admin p)              = "admin/" </> admin p
main DelegationEdit         = "delegation/edit/"
main DelegationView         = "delegation/view/"
main Imprint                = "imprint/"
main Terms                  = "terms/"
main Login                  = "login/"

data Space =
    SpaceIdeaAll
  | SpaceIdeaOneView (AUID Idea)
  | SpaceIdeaOneEdit (AUID Idea)
  | SpaceIdeaCreate
  | SpaceTopicAll
  | SpaceTopicOneIdeas (AUID Topic)
  | SpaceTopicOneDelegations (AUID Topic)
  | SpaceTopicCreate
  | SpaceTopicIdeaCreate (AUID Topic)

space :: Space -> UriPath
space SpaceIdeaAll                           = "idea/"
space (SpaceIdeaOneView (AUID iid))          = "idea/" </> cs (show iid) </> "/view/"
space (SpaceIdeaOneEdit (AUID iid))          = "idea/" </> cs (show iid) </> "/edit/"
space SpaceIdeaCreate                        = "idea/create/"
space SpaceTopicAll                          = "topic/"
space (SpaceTopicOneIdeas (AUID tid))        = "topic/" </> cs (show tid) </> "/ideas/"
space (SpaceTopicOneDelegations (AUID tid))  = "topic/" </> cs (show tid) </> "/delegations/"
space SpaceTopicCreate                       = "topic/create/"
space (SpaceTopicIdeaCreate (AUID tid))      = "topic/" </> cs (show tid) </> "/idea/create/"

data UserPs =
    UserIdeas
  | UserDelegations

user :: UserPs -> UriPath
user UserIdeas       = "ideas/"
user UserDelegations = "delegations/"

data AdminPs =
    AdminParams
  | AdminAccess
  | AdminUser
  | AdminEvent

admin :: AdminPs -> UriPath
admin AdminParams = "params/"
admin AdminAccess = "access/"
admin AdminUser   = "user/"
admin AdminEvent  = "event/"
