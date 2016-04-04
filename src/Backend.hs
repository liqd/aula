{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend
where

import Control.Monad.IO.Class
import Action
import Arbitrary
import DemoData
import CreateRandom
import Persistent.Api
import Servant
import Types


-- * rest api

type Api =
       "delegations" :> DelegationsApi
  :<|> "manage-state" :> ManageStateApi

api :: (MonadIO m, GenArbitrary r, ActionM r m) => ServerT Api m
api =  delegationsApi
  :<|> manageStateApi


-- * delegations

type DelegationsApi = Get '[JSON] DelegationNetwork

-- | FIXME: This is all a bit silly: the new end-point logs in admin implicitly; the returned
-- delegation networks are generated on top of the existing data; testing doesn't really test
-- anything.  But it is self-contained and a good basis to continue from.
delegationsApi :: (GenArbitrary r, ActionM r m) => ServerT DelegationsApi m
delegationsApi = Action.loginByName "admin" >> fishDelegationNetworkAction


-- * persistent state management (for demo operation)

type ManageStateApi =
       "create-init" :> Post '[JSON] ()
  :<|> "create-demo" :> Post '[JSON] ()
  :<|> "wipe"        :> Post '[JSON] ()

manageStateApi :: (MonadIO m, GenArbitrary r, ActionM r m) => ServerT ManageStateApi m
manageStateApi =
       persistent genInitialTestDb
  :<|> (liftIO mkUniverse >>= persistent)
  :<|> persistent (modifyDb (DbId :.: id) (const emptyAulaData))
