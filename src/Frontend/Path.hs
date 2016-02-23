{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}


-- | rule: always add (and expect) trailing slashes.
module Frontend.Path
where

import Thentos.Prelude

import Types

type UriPath = ST

data Top =
    TopMain Main  -- (we don't do proper paths for /testing/)
  | TopSamples
  | TopStatic

top :: Top -> UriPath
top = p
  where
    p :: Top -> UriPath
    p (TopMain pth)    = main pth
    p TopSamples       = "samples/"
    p TopStatic        = "static/"

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

main :: Main -> UriPath
main = p
  where
    p MainSpaceAll                 = "space/"
    p (MainSpaceOne sid pth)       = "space/" <> sid <> "/" <> space pth
    p MainUserAll                  = "user/"
    p (MainUserOne (AUID uid) pth) = "user/" <> cs (show uid) <> "/" <> user pth
    p MainAdmin                    = "admin/"
    p MainDelegationEdit           = "deletagion/edit/"
    p MainDelegationView           = "deletagion/view/"
    p MainImprint                  = "imprint/"
    p MainTerms                    = "terms/"
    p MainLogin                    = "login/"

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
space = p
  where
    p SpaceIdeaAll                           = "idea/"
    p (SpaceIdeaOneView (AUID iid))          = "idea/" <> cs (show iid) <> "/view/"
    p (SpaceIdeaOneEdit (AUID iid))          = "idea/" <> cs (show iid) <> "/edit/"
    p SpaceIdeaCreate                        = "idea/create/"
    p SpaceTopicAll                          = "topic/"
    p (SpaceTopicOneIdeas (AUID tid))        = "topic/" <> cs (show tid) <> "/ideas/"
    p (SpaceTopicOneDelegations (AUID tid))  = "topic/" <> cs (show tid) <> "/delegations/"
    p SpaceTopicCreate                       = "topic/create/"
    p (SpaceTopicIdeaCreate (AUID tid))      = "topic/" <> cs (show tid) <> "/idea/create/"

data User =
    UserIdeas
  | UserDelegations
  | UserSettings

user :: Frontend.Path.User -> UriPath
user = p
  where
    p UserIdeas       = "ideas/"
    p UserDelegations = "delegations/"
    p UserSettings    = "settings/"

data Admin =
    AdminParams
  | AdminAccess
  | AdminUser
  | AdminEvent

admin :: Admin -> UriPath
admin = p
  where
    p AdminParams = "params/"
    p AdminAccess = "access/"
    p AdminUser   = "user/"
    p AdminEvent  = "event/"
