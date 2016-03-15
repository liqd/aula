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
import Network.Wreq.Types (Postable)
import Test.Hspec

import qualified Network.Wreq.Session as Sess

import Config
import Frontend

-- Same as Frontend.Page.FileUploadSpec.Query
data Query = Query
    { post :: forall a. Postable a => String -> a -> IO (Response LBS)
    , get  :: String -> IO (Response LBS)
    }

-- Same as Frontend.Page.FileUploadSpec.withServer
withServer :: (Query -> IO a) -> IO a
withServer action = bracket
    (forkIO $ runFrontend cfg)
    killThread
    (const . Sess.withSession $ action . query)
  where
    cfg = Config.test
    uri path = "http://" <> cs (cfg ^. listenerInterface) <> ":" <> (cs . show $ cfg ^. listenerPort) <> path
    query sess = Query (Sess.post sess . uri) (Sess.get sess . uri)

spec :: Spec
spec = describe "logging in" $ do

  describe "with standard initial DB" . around withServer $ do
    context "if user does not exist" $ do
      it "will not log you in and will display something" $ \query -> do
        l <- post query "/login" [partString "/login.user" "not the admin", partString "/login.pass" "foo"]
        (l ^. responseStatus . statusCode) `shouldBe` 500

    context "if user does exist" $ do
      context "if password is wrong" $ do
        it "will not log you in and will display something" $ \_query -> do
          pendingWith "this prototype doesn't do this yet."

      context "if password is correct" $ do
        it "will indeed log you in (yeay)" $ \query -> do
            l <- post query "/login" [partString "/login.user" "admin", partString "/login.pass" "admin"]
            (l ^. responseStatus . statusCode) `shouldBe` 200
            l2 <- get query "/space"
            (l2 ^. responseStatus . statusCode) `shouldBe` 200
