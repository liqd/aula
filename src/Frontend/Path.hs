{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | rule: always add (and expect) trailing slashes.
module Frontend.Path
    ( pth, href_, (</>)
    , UriPath
    , Top(..)
    , Main(..)
    , Space(..)
    , Frontend.Path.User(..)
    , Admin(..)
    )
where

import Thentos.Prelude
import Thentos.Types ((<//>))

import qualified Lucid

import Types


type UriPath = ST

(</>) :: UriPath -> UriPath -> UriPath
(</>) = (<//>)

href_ :: HasPath p => p -> Lucid.Attribute
href_ = Lucid.href_ . pth


class HasPath p where
    pth :: p -> UriPath
    pth = ("/" </>) . relpth

    relpth :: p -> UriPath

data Top =
    TopMain Main  -- (we don't do proper paths for /testing/)
  | TopSamples
  | TopStatic

instance HasPath Top where relpth = top

top :: Top -> UriPath
top (TopMain p) = pth p
top TopSamples  = "samples/"
top TopStatic   = "static/"

data Main =
    MainSpaceAll
  | MainSpaceOne ST Space
  | MainUserAll
  | MainUserOne (AUID Types.User) Frontend.Path.User
  | MainAdmin Admin
  | MainDelegationEdit
  | MainDelegationView
  | MainImprint
  | MainTerms
  | MainLogin

instance HasPath Main where relpth = main

main :: Main -> UriPath
main MainSpaceAll               = "space/"
main (MainSpaceOne sid p)       = "space/" </> sid </> space p
main MainUserAll                = "user/"
main (MainUserOne (AUID uid) p) = "user/" </> cs (show uid) </> user p
main (MainAdmin p)              = "admin/" </> admin p
main MainDelegationEdit         = "deletagion/edit/"
main MainDelegationView         = "deletagion/view/"
main MainImprint                = "imprint/"
main MainTerms                  = "terms/"
main MainLogin                  = "login/"

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

data User =
    UserIdeas
  | UserDelegations
  | UserSettings

user :: Frontend.Path.User -> UriPath
user UserIdeas       = "ideas/"
user UserDelegations = "delegations/"
user UserSettings    = "settings/"

data Admin =
    AdminParams
  | AdminAccess
  | AdminUser
  | AdminEvent

admin :: Admin -> UriPath
admin AdminParams = "params/"
admin AdminAccess = "access/"
admin AdminUser   = "user/"
admin AdminEvent  = "event/"
