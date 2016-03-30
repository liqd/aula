{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Backend
where

import Action
import Arbitrary
import Servant
import Types


-- * rest api

type Api = "delegations" :> DelegationsApi

api :: (GenArbitrary r, ActionM r m) => ServerT Api m
api = delegationsApi


-- * delegations

type DelegationsApi = Get '[JSON] DelegationNetwork

-- | FIXME: This is all a bit silly: the new end-point logs in admin implicitly; the returned
-- delegation networks are generated on top of the existing data; testing doesn't really test
-- anything.  But it is self-contained and a good basis to continue from.
delegationsApi :: (GenArbitrary r, ActionM r m) => ServerT Api m
delegationsApi = Action.loginByName "admin" >> fishDelegationNetworkAction
