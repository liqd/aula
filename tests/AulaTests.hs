{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

module AulaTests
    ( module AulaTests
    , module X
    ) where

import Control.Concurrent (forkIO, killThread, threadDelay, ThreadId)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Exception (bracket)
import Network.HTTP.Client (HttpException)
import Network.Wreq.Types (Postable, StatusChecker)
import System.IO.Unsafe (unsafePerformIO)

import qualified Network.Wreq
import qualified Network.Wreq.Session as Sess

import Config

import Network.Wreq     as X hiding (get, post, put, head_, Proxy, Link)
import Test.Hspec       as X
import Action           as X
import Servant          as X
import Frontend         as X
import Frontend.Prelude as X hiding (get, put)

import qualified Persistent.Implementation (mkRunPersistInMemory)

testConfig :: IO Config
testConfig = do
    cfg <- getConfig DontWarnMissing
    pop <- modifyMVar testConfigPortSource $ \(h:t) -> pure (t, h)
    cfg & listenerPort .~ pop
        & pure


-- | This is where the ports are popped from that the individual tests are run under.
testConfigPortSource :: MVar [Int]
testConfigPortSource = unsafePerformIO . newMVar . mconcat $ repeat [18081..29713]
{-# NOINLINE testConfigPortSource #-}

codeShouldBe :: Int -> Response body -> Expectation
codeShouldBe code l = l ^. responseStatus . statusCode `shouldBe` code

bodyShouldBe :: (Show body, Eq body) => body -> Response body -> Expectation
bodyShouldBe body l = l ^. responseBody `shouldBe` body

bodyShouldContain :: String -> Response LBS -> Expectation
bodyShouldContain body l = l ^. responseBody . to cs `shouldContain` body

shouldRespond :: IO (Response body) -> [Response body -> Expectation] -> IO ()
shouldRespond action matcher = action >>= \r -> mapM_ ($r) matcher

data Query = Query
    { post :: forall a. Postable a => String -> a -> IO (Response LBS)
    , get  :: String -> IO (Response LBS)
    }

doNotThrowExceptionsOnErrorCodes :: StatusChecker
doNotThrowExceptionsOnErrorCodes _ _ _ = Nothing

withServer :: (Query -> IO a) -> IO a
withServer action = do
    cfg <- testConfig

    let opts = defaults & checkStatus .~ Just doNotThrowExceptionsOnErrorCodes
                        & redirects   .~ 0
        query sess = Query (Sess.postWith opts sess . mkServerUri cfg)
                           (Sess.getWith opts sess . mkServerUri cfg)
        initialize q = do
            resp
               <- post q "/api/manage-state/create-init"
                    [partString "/login.user" "admin", partString "/login.pass" "adminPass"]
            case resp of
                (view (responseStatus . statusCode) -> 204) -> pure ()
                _ -> error $ "withServer: init failed: " <> show resp

    bracket
        (runFrontendSafeFork cfg)
        killThread
        (const . Sess.withSession $ \sess -> do
            initialize $ query sess
            action $ query sess)

mkServerUri :: Config -> String -> String
mkServerUri cfg path = "http://" <> cs (cfg ^. listenerInterface)
                          <> ":" <> show (cfg ^. listenerPort)
                          <> path

runFrontendSafeFork :: Config -> IO ThreadId
runFrontendSafeFork cfg = do
    threadId <- forkIO $ withPersist Persistent.Implementation.mkRunPersistInMemory
                                     (runFrontendGeneric cfg)
    let loop = catch
          (Network.Wreq.get $ mkServerUri cfg "/")
          (\(_ :: HttpException) -> threadDelay 4900 >> loop)
    loop >> return threadId
