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
import DemoData
import Frontend.Core
import Persistent.Api
import Types


-- * rest api

type Api =
       "manage-state" :> ManageStateApi

api :: (Page Api, GenArbitrary m, ActionM m) => ServerT Api m
api = manageStateApi

type GetJ p = Get '[JSON] (GetResult p)
type PostJ p r = Post '[JSON] (PostResult p r)

data NeedEmptyUserMap = NeedEmptyUserMap

instance Page NeedEmptyUserMap where
    isAuthorized = publicPage
{-
    FIXME: since we don't have access to the db here.
    isAuthorized _ = do
        noUsers <- query $ views dbUserMap Map.null
        if noUsers
            then accessGranted
            else accessDenied $ Just "Can only be used when there are no users!"

(In thentos, we have a credential *is coming from localhost*.  I like this better because it doesn't
require any additional work to avoid having to race attackers after deployment.  So perhaps we
should write a capability 'ComeFromLocalhost' instead?)
-}

-- * persistent state management (for demo operation)

type ManageStateApi =
       "wipe"        :> PostJ NeedAdmin ()
  :<|> "create-init" :> PostJ NeedEmptyUserMap ()
  :<|> "create-demo" :> PostJ NeedAdmin ()
  :<|> "create-votes" :> PostJ NeedAdmin ()
  :<|> "create-delegations" :> PostJ NeedAdmin ()
  :<|> "rename-logins" :> Capture "suffix" ST :> PostJ NeedAdmin ()

manageStateApi :: (GenArbitrary m, ActionM m) => ServerT ManageStateApi m
manageStateApi =
       runPostHandler (pure NeedAdmin) (update DangerousResetAulaData)
  :<|> runPostHandler (pure NeedEmptyUserMap) genInitialTestDb
  :<|> runPostHandler (pure NeedAdmin) (void (mkUniverse smallUniverseSize))
  :<|> runPostHandler (pure NeedAdmin) genVotingPhaseTopic
  :<|> runPostHandler (pure NeedAdmin) randomDelegations
  :<|> runPostHandler (pure NeedAdmin) . update . DangerousRenameAllLogins
