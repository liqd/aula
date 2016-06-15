{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend
where

import Control.Monad (void)
import Data.String.Conversions (ST)
import Servant

import Access
import Action
import Arbitrary
import DemoData
import Frontend.Core
import Persistent.Api
import Types


-- * rest api

type Api =
       "delegations" :> DelegationsApi
  :<|> "manage-state" :> ManageStateApi

api :: (Page Api, GenArbitrary m, ActionM m) => ServerT Api m
api =  delegationsApi
  :<|> manageStateApi

type GetJ p = Get '[JSON] (GetResult p)
type PostJ p r = Post '[JSON] (PostResult p r)

-- * delegations

type DelegationsApi = GetJ DelegationNetwork

-- | FIXME: This is all a bit silly: the new end-point logs in admin implicitly; the returned
-- delegation networks are generated on top of the existing data; testing doesn't really test
-- anything.  But it is self-contained and a good basis to continue from.
delegationsApi :: (GenArbitrary m, ActionM m) => ServerT DelegationsApi m
delegationsApi = Action.loginByName "admin" >> fishDelegationNetworkAction Nothing


-- * persistent state management (for demo operation)

type ManageStateApi =
       "wipe"        :> PostJ NeedAdmin ()
  :<|> "create-init" :> PostJ NeedAdmin ()
  :<|> "create-demo" :> PostJ NeedAdmin ()
  :<|> "create-votes" :> PostJ NeedAdmin ()
  :<|> "rename-logins" :> Capture "suffix" ST :> PostJ NeedAdmin ()

manageStateApi :: (GenArbitrary m, ActionM m) => ServerT ManageStateApi m
manageStateApi =
       runPostHandler (pure NeedAdmin) (update DangerousResetAulaData)
  :<|> runPostHandler (pure NeedAdmin) genInitialTestDb
  :<|> runPostHandler (pure NeedAdmin) (void (mkUniverse defaultUniverseSize))
  :<|> runPostHandler (pure NeedAdmin) genVotingPhaseTopic
  :<|> runPostHandler (pure NeedAdmin) . update . DangerousRenameAllLogins
