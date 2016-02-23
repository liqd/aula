{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | rule: always add (and expect) trailing slashes.
module Frontend.Path
where

import Thentos.Prelude
import Thentos.Types ((<//>))

import Types

type UriPath = ST

(</>) :: UriPath -> UriPath -> UriPath
(</>) = (<//>)


class HasPath p where
    pth :: p -> UriPath

-- FIXME: add `href_ . P.pth` (do export!)

data Top =
    TopMain Main  -- (we don't do proper paths for /testing/)
  | TopSamples
  | TopStatic

instance HasPath Top where
    pth (TopMain p) = pth p
    pth TopSamples  = "samples/"
    pth TopStatic   = "static/"

data Main =
    MainSpaceAll
  | MainSpaceOne ST Space
  | MainUserAll
  | MainUserOne (AUID Types.User) Frontend.Path.User
  | MainAdmin
  | MainDelegationEdit
  | MainDelegationView
  | MainImprint
  | MainTerms
  | MainLogin

instance HasPath Main where
    pth MainSpaceAll               = "space/"
    pth (MainSpaceOne sid p)       = "space/" </> sid </> pth p
    pth MainUserAll                = "user/"
    pth (MainUserOne (AUID uid) p) = "user/" </> cs (show uid) </> pth p
    pth MainAdmin                  = "admin/"
    pth MainDelegationEdit         = "deletagion/edit/"
    pth MainDelegationView         = "deletagion/view/"
    pth MainImprint                = "imprint/"
    pth MainTerms                  = "terms/"
    pth MainLogin                  = "login/"

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

instance HasPath Space where
    pth SpaceIdeaAll                           = "idea/"
    pth (SpaceIdeaOneView (AUID iid))          = "idea/" </> cs (show iid) </> "/view/"
    pth (SpaceIdeaOneEdit (AUID iid))          = "idea/" </> cs (show iid) </> "/edit/"
    pth SpaceIdeaCreate                        = "idea/create/"
    pth SpaceTopicAll                          = "topic/"
    pth (SpaceTopicOneIdeas (AUID tid))        = "topic/" </> cs (show tid) </> "/ideas/"
    pth (SpaceTopicOneDelegations (AUID tid))  = "topic/" </> cs (show tid) </> "/delegations/"
    pth SpaceTopicCreate                       = "topic/create/"
    pth (SpaceTopicIdeaCreate (AUID tid))      = "topic/" </> cs (show tid) </> "/idea/create/"

data User =
    UserIdeas
  | UserDelegations
  | UserSettings

instance HasPath Frontend.Path.User where
    pth UserIdeas       = "ideas/"
    pth UserDelegations = "delegations/"
    pth UserSettings    = "settings/"

data Admin =
    AdminParams
  | AdminAccess
  | AdminUser
  | AdminEvent

instance HasPath Admin where
    pth AdminParams = "params/"
    pth AdminAccess = "access/"
    pth AdminUser   = "user/"
    pth AdminEvent  = "event/"
