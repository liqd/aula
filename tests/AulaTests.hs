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


testConfig :: IO Config
testConfig = (devel & generateDemoData .~ False &) . (listenerPort .~) <$> pop
  where
    pop :: IO Int
    pop = modifyMVar testConfigPortSource $ \(h:t) -> pure (t, h)

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

    bracket
        (runFrontendSafeFork cfg)
        killThread
        (const . Sess.withSession $ action . query)

mkServerUri :: Config -> String -> String
mkServerUri cfg path = "http://" <> cs (cfg ^. listenerInterface)
                          <> ":" <> show (cfg ^. listenerPort)
                          <> path

runFrontendSafeFork :: Config -> IO ThreadId
runFrontendSafeFork cfg = do
    threadId <- forkIO $ runFrontend cfg
    let loop = catch
          (Network.Wreq.get $ mkServerUri cfg "/")
          (\(_ :: HttpException) -> threadDelay 4900 >> loop)
    loop >> return threadId
