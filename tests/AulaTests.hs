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
import System.Process (system)
import Test.Hspec.Wai (WaiExpectation)
import Test.QuickCheck (Gen, frequency, choose)

import qualified Network.Wreq
import qualified Network.Wreq.Session as Sess

import Config

import Network.Wreq     as X hiding (get, post, put, head_, Proxy, Link)
import Test.Hspec       as X
import Action           as X hiding (setCreatorStatement)
import Servant          as X
import Frontend         as X
import Frontend.Testing as X
import Frontend.Prelude as X hiding (get, put)
import Test.Hspec.Missing as X

import Arbitrary (constantSampleTimestamp)
import Logger (LogLevel(..), nullLog)
import Persistent (mkMetaInfo)


-- Be default, test cases are part of the smoke test suite.
data TestSuite
    = Large
  deriving (Eq, Show)

instance Tag TestSuite where
    tagText = cs . show

testConfig :: IO Config
testConfig = do
    cfg <- readConfig nullLog DontWarnMissing
    pop <- modifyMVar testConfigPortSource $ \(h:t) -> pure (t, h)
    cfg & listenerPort   .~ pop
              -- (in case somebody accidentally tests on a production system: change dbPath.)
        & persistConfig . dbPath          .~ "./state/AulaData_Tests"
        & persistConfig . persistenceImpl .~ AcidStateInMem
        & logging . logLevel              .~ NOLOG
        & logging . eventLogPath          .~ "/dev/null"
        & pure


-- | This is where the ports are popped from that the individual tests are run under.
testConfigPortSource :: MVar [Int]
testConfigPortSource = unsafePerformIO . newMVar . mconcat $ repeat [18081..29713]
{-# NOINLINE testConfigPortSource #-}

-- FIXME: error location should be of the caller
-- FIXME: rename to 'statusShouldBe'
codeShouldBe :: Int -> Response body -> Expectation
codeShouldBe code l = l ^. responseStatus . statusCode `shouldBe` code

-- FIXME: error location should be of the caller
bodyShouldBe :: (Show body, Eq body) => body -> Response body -> Expectation
bodyShouldBe body l = l ^. responseBody `shouldBe` body

-- FIXME: error location should be of the caller
bodyShouldContain :: String -> Response LBS -> Expectation
bodyShouldContain body l = l ^. responseBody . csi `shouldContain` body

-- FIXME: error location should be of the caller
shouldRespond :: IO (Response body) -> [Response body -> Expectation] -> IO ()
shouldRespond action matcher = action >>= \r -> mapM_ ($r) matcher

-- FIXME: error location should be of the caller
bodyShouldSatisfy :: (Show body, Eq body) => (body -> Bool) -> Response body -> Expectation
bodyShouldSatisfy bodyP l = l ^. responseBody `shouldSatisfy` bodyP

data WreqQuery = WreqQuery
    { post :: forall a. Postable a => String -> a -> IO (Response LBS)
    , get  :: String -> IO (Response LBS)
    }

doNotThrowExceptionsOnErrorCodes :: StatusChecker
doNotThrowExceptionsOnErrorCodes _ _ _ = Nothing

withServer :: (WreqQuery -> IO a) -> IO a
withServer action = (`withServer'` action) =<< testConfig

withServerWithEventLog :: (WreqQuery -> IO a) -> IO a
withServerWithEventLog action = do
    let elpath = "/tmp/aula-test-events.json"
    _ <- system $ "rm -f " <> show elpath
    (`withServer'` action) . (logging . eventLogPath .~ elpath) =<< testConfig

withServer' :: Config -> (WreqQuery -> IO a) -> IO a
withServer' cfg action = do
    let opts = defaults & checkStatus ?~ doNotThrowExceptionsOnErrorCodes
                        & redirects   .~ 0
        wreqQuery sess = WreqQuery (Sess.postWith opts sess . mkServerUri cfg)
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
            initialize $ wreqQuery sess
            action     $ wreqQuery sess)

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

someTestUser :: User
someTestUser = user
  where
    user = User
        { _userMeta      = metainfo
        , _userLogin     = "VorNam"
        , _userFirstName = "Vorname"
        , _userLastName  = "Name"
        , _userRole      = Admin
        , _userProfile   = UserProfile
            { _profileAvatar = Nothing
            , _profileDesc   = markdown nil
            }
        , _userSettings  = UserSettings
            { _userSettingsPassword = UserPassInitial (InitialPassword "")
            , _userSettingsEmail    = Nothing
            }
        }
    uid = AUID 0
    oid = AUID 1
    cUser = user & _Id .~ uid -- the user creates himself
    metainfo :: MetaInfo User
    metainfo = mkMetaInfo cUser constantSampleTimestamp oid


-- * Expectations

passes :: Expectation
passes = return ()

wpasses :: WaiExpectation
wpasses = return ()


-- * quickcheck

-- | Make sure that the boundary values are hit.
boundary :: (Random a, Num a) => a -> a -> Gen a
boundary mn mx = frequency
    [ (1, pure mn)
    , (1, pure mx)
    , (98, choose (mn, mx))
    ]
