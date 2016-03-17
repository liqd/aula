{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.LoginSpec
where

import Control.Concurrent (forkIO, killThread)
import Control.Exception
import Control.Lens
import Data.String.Conversions (LBS, cs, (<>))
import Network.Wreq hiding (get, post)
import Network.Wreq.Types (Postable, StatusChecker)
import Test.Hspec

import qualified Network.Wreq.Session as Sess

import Config
import Frontend

-- Same as Frontend.Page.FileUploadSpec.Query
data Query = Query
    { post :: forall a. Postable a => String -> a -> IO (Response LBS)
    , get  :: String -> IO (Response LBS)
    }

-- Same as Frontend.Page.FileUploadSpec.doNotThrowExceptionsOnErrorCodes
doNotThrowExceptionsOnErrorCodes :: StatusChecker
doNotThrowExceptionsOnErrorCodes _ _ _ = Nothing

-- Same as Frontend.Page.FileUploadSpec.withServer
withServer :: (Query -> IO a) -> IO a
withServer action = bracket
    (forkIO $ runFrontend cfg)
    killThread
    (const . Sess.withSession $ action . query)
  where
    cfg = Config.test
    uri path = "http://" <> cs (cfg ^. listenerInterface) <> ":" <> (cs . show $ cfg ^. listenerPort) <> path
    opts = defaults & checkStatus .~ Just doNotThrowExceptionsOnErrorCodes
                    & redirects   .~ 0
    query sess = Query (Sess.postWith opts sess . uri) (Sess.getWith opts sess . uri)

spec :: Spec
spec = describe "logging in" $ do

  let
    checkLogin query user pass code = do
        l <- post query "/login" [partString "/login.user" user, partString "/login.pass" pass]
        (l ^. responseStatus . statusCode) `shouldBe` code

    checkLoggedIn query code = do
        l <- get query "/space"
        (l ^. responseStatus . statusCode) `shouldBe` code

  describe "with standard initial DB" . around withServer $ do
    it "redirects you if not logged in" $ \query -> do
      checkLoggedIn query 303

    context "if user does not exist" $ do
      it "will not log you in and will display something" $ \query -> do
        checkLogin query "not the admin" "foo" 500
        checkLoggedIn query 303

    context "if user does exist" $ do
      context "if password is wrong" $ do
        it "will not log you in and will display something" $ \_query -> do
          pendingWith "this prototype doesn't do this yet."

      context "if password is correct" $ do
        it "will indeed log you in (yeay)" $ \query -> do
            checkLogin query "admin" "admin" 303
            checkLoggedIn query 200
