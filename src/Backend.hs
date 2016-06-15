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


-- * delegations

type DelegationsApi = Get '[JSON] DelegationNetwork

-- | FIXME: This is all a bit silly: the new end-point logs in admin implicitly; the returned
-- delegation networks are generated on top of the existing data; testing doesn't really test
-- anything.  But it is self-contained and a good basis to continue from.
delegationsApi :: (GenArbitrary m, ActionM m) => ServerT DelegationsApi m
delegationsApi = Action.loginByName "admin" >> fishDelegationNetworkAction Nothing


-- * persistent state management (for demo operation)

type ManageStateApi =
       "wipe"        :> Post '[JSON] (PostResult NeedAdmin)
  :<|> "create-init" :> Post '[JSON] (PostResult NeedAdmin)
  :<|> "create-demo" :> Post '[JSON] (PostResult NeedAdmin)
  :<|> "create-votes" :> Post '[JSON] (PostResult NeedAdmin)
  :<|> "rename-logins" :> Capture "suffix" ST :> Post '[JSON] (PostResult NeedAdmin)

manageStateApi :: (GenArbitrary m, ActionM m) => ServerT ManageStateApi m
manageStateApi =
       runPostHandler (pure NeedAdmin) (update DangerousResetAulaData)
  :<|> runPostHandler (pure NeedAdmin) genInitialTestDb
  :<|> runPostHandler (pure NeedAdmin) (void (mkUniverse defaultUniverseSize))
  :<|> runPostHandler (pure NeedAdmin) genVotingPhaseTopic
  :<|> runPostHandler (pure NeedAdmin) . update . DangerousRenameAllLogins
