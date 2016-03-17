{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module AulaTests
    ( module AulaTests
    , module X
    ) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception
import Network.Wreq.Types (Postable, StatusChecker)

import qualified Network.Wreq.Session as Sess

import Config
import Frontend

import Network.Wreq     as X hiding (get, post, put, Proxy)
import Control.Lens     as X
import Test.Hspec       as X
import Thentos.Prelude  as X hiding (get, put)
import Action           as X

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
