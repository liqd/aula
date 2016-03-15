{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module FrontendSpec
where

import Data.String.Conversions (ST, cs)
import Lucid (renderText, toHtml)
import Test.Hspec (Spec, describe, context, it)
import Test.Hspec.Wai (with, get, shouldRespondWith, pending, ResponseMatcher(ResponseMatcher))
import Servant
import Network.Wai

import Action
import Config
import Frontend
import Frontend.Core
import qualified Persistent.Implementation.STM

spec :: Spec
spec = do
    describe "catchAulaExcept" . with testAppAulaExcept $ do
        let handles500 = do
                it "writes error message to stdout" $ do
                    pending
                it "sends an apologetic 500 http response" $ do
                    pending
                    get "/error" `shouldRespondWith` 500
                it "bases http response on type `Page500`" $ do
                    pending

            handles303 = do
                it "redirects to the indicated uri." $ do
                    pending

        context "on `error`"                               handles500
        context "on `throw 303`"                           handles303
        context "on all other `throw Action.ActionExcept`" handles500

    describe "catch404" . with testApp404 $ do
        context "when routing table has no matching entry" $ do
            it "writes error message to stdout" $ do
                pending
            it "sends an 404 response" $ do
                get "/nosuchpath" `shouldRespondWith` 404
            it "bases http response on type `Page404`" $ do
                get "/nosuchpath" `shouldRespondWith`
                    (ResponseMatcher 404 [] . Just . cs . renderText . toHtml $ PublicFrame Page404)


type TestApi =
       "error"     :> GetH ST
  :<|> "303"       :> GetH ST
  :<|> "any_other" :> GetH ST

testApi :: (Monad m, MonadServantErr err m) => ServerT TestApi m
testApi =
       error "unexpected"
  :<|> throwServantErr (err303 { errHeaders = ("Location", "/target") : errHeaders err303 })
  :<|> throwServantErr err500

testAppAulaExcept :: IO Application
testAppAulaExcept = do
-- FIXME
    action <- mkRunAction <$> (ActionEnv <$> Persistent.Implementation.STM.mkRunPersist <*> pure Config.test)
    let proxy :: Proxy TestApi
        proxy = Proxy
    return $ serve (Proxy :: Proxy TestApi) (enter action $ catchAulaExcept proxy testApi)

testApp404 :: IO Application
testApp404 = return . catch404 $ serve (Proxy :: Proxy TestApi) testApi
