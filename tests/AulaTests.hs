{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module AulaTests
    ( module AulaTests
    , module X
    ) where

import Control.Concurrent (forkIO, killThread, threadDelay, ThreadId)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import Control.Monad.Trans.Reader (runReaderT)
import Data.String.Conversions
import Network.Wreq.Types (Postable, StatusChecker)
import System.Directory (getTemporaryDirectory)
import System.IO.Temp (createTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (system)
import System.Random (Random)
import System.Timeout (timeout)
import Test.Hspec.Core.Spec (SpecM(..))
import Test.Hspec.Wai (WaiExpectation)
import Test.HUnit.Lang (HUnitFailure(HUnitFailure))
import Test.QuickCheck (Gen, frequency, choose)

import qualified Data.Set as Set
import qualified Network.Wreq
import qualified Network.Wreq.Session as Sess
import qualified Test.WebDriver as WD

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
import Persistent (mkMetaInfo)


-- | 'runIO' with better errors.
runIO' :: (SomeException -> IO a) -> IO a -> SpecM () a
runIO' e a = runIO $ a `catch` e


-- | Transform an 'HtmlT' into an 'ST' in the default language.
renderHtmlDefaultT :: Monad m => HtmlT m () -> m LT
renderHtmlDefaultT = (`runReaderT` minBound) . renderTextT

-- | Transform an 'Html' into an 'ST' in the default language.
renderHtmlDefaultH :: Html () -> LT
renderHtmlDefaultH = runIdentity . renderHtmlDefaultT

-- | Transform any 'ToHtml' instance into an 'ST' in the default language.
renderHtmlDefault :: ToHtml h => h -> LT
renderHtmlDefault = renderHtmlDefaultH . toHtml


-- Be default, test cases are part of the smoke test suite.
data TestSuite
    = Large
  deriving (Eq, Show)

instance Tag TestSuite where
    tagText = cs . show

-- (In case somebody accidentally tests on a production system: change dbPath, avatars path etc.)
testConfig :: IO Config
testConfig = do
    cfg <- readConfig nullLog DontWarnMissing
    pop <- modifyMVar testConfigPortSource $ \(h:t) -> pure (t, h)
    avt <- do
        -- each test suite run accumulates a few MB of avatar images; since using
        -- 'withSystemTempDirectory' would require significant refactorings of the test suite, we
        -- simply remove all avatar test locations ever created here.  note that this is not good
        -- for concurrent testing.
        tmpPool <- (<> "aula-test-avatar-static") <$> getTemporaryDirectory
        void . system . unwords $ ["rm -rf", tmpPool, "; mkdir -p", tmpPool]
        createTempDirectory tmpPool "d"
    cfg & listenerPort                    .~ pop
        & persistConfig . dbPath          .~ "./state/AulaData_Tests"
        & persistConfig . persistenceImpl .~ AcidStateInMem
        & logging . logLevel              .~ NOLOG
        & logging . eventLogPath          .~ "/dev/null"
        & avatarPath                      .~ avt
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
shouldRespond :: forall body. (Show body, ConvertibleStrings body String) => IO (Response body) -> [Response body -> Expectation] -> IO ()
shouldRespond action matcher = action >>= \r -> mapM_ ($r) matcher `catch` appendResp r
  where
    appendResp :: Response body -> HUnitFailure -> IO ()
    appendResp r (HUnitFailure mloc msg) = throwIO $ HUnitFailure mloc (unlines $ msg:extraInfo)
      where
        extraInfo =
            [ "\n*** body:"
            , show (cs (r ^. responseBody) :: String)
            , "\n*** status:"
            , ppShow (r ^. responseStatus)
            , "\n*** headers:"
            , ppShow (r ^. responseHeaders)
            , "\n*** full request:"
            , ppShow r
            ]

-- FIXME: error location should be of the caller
bodyShouldSatisfy :: (Show body, Eq body) => (body -> Bool) -> Response body -> Expectation
bodyShouldSatisfy bodyP l = l ^. responseBody `shouldSatisfy` bodyP

data WreqQuery = WreqQuery
    { post  :: forall a. Postable a => String -> a -> IO (Response LBS)
    , get   :: String -> IO (Response LBS)
    , mkUri :: String -> String  -- (e.g., for selenium)
    }

doNotThrowExceptionsOnErrorCodes :: StatusChecker
doNotThrowExceptionsOnErrorCodes _ _ _ = Nothing

loginAsAdmin :: WreqQuery -> IO ()
loginAsAdmin wreq =
    post wreq "/login"
        [partString "/login.user" "admin", partString "/login.pass" "pssst"]
        `shouldRespond` [codeShouldBe 303]

withServer :: (WreqQuery -> IO a) -> IO a
withServer action = (`withServer'` action) =<< testConfig

withServer' :: Config -> (WreqQuery -> IO a) -> IO a
withServer' cfg action = do
    let opts = defaults & checkStatus ?~ doNotThrowExceptionsOnErrorCodes
                        & redirects   .~ 0
        wreqQuery :: Sess.Session -> Config -> WreqQuery
        wreqQuery sess cfg' = WreqQuery (Sess.postWith opts sess . mkServerUri cfg')
                                        (Sess.getWith opts sess . mkServerUri cfg')
                                        (mkServerUri cfg')
        initialize q = do
            resp <- post q "/api/manage-state/create-init" ([] :: [Part])
            case resp of
                (view (responseStatus . statusCode) -> 200) -> pure ()
                (view (responseStatus . statusCode) -> 201) -> pure ()
                (view (responseStatus . statusCode) -> 204) -> pure ()
                _ -> error $ "withServer: init failed: " <> show resp

    bracket
        (runFrontendSafeFork cfg)
        (killThread . fst)
        (\(_, cfg') -> Sess.withSession $ \sess -> do
            initialize $ wreqQuery sess cfg'
            action     $ wreqQuery sess cfg')

withServerAsAdmin :: (WreqQuery -> IO a) -> IO a
withServerAsAdmin action = withServer $ \wreq -> do
    putStrLn "logging in as admin"
    loginAsAdmin wreq
    putStrLn "logged in as admin"
    action wreq

mkServerUri :: Config -> String -> String
mkServerUri cfg path = "http://" <> cs (cfg ^. listenerInterface)
                          <> ":" <> show (cfg ^. listenerPort)
                          <> path

runFrontendSafeFork :: Config -> IO (ThreadId, Config)
runFrontendSafeFork = tryListener 37
  where
    tryListener :: Int -> Config -> IO (ThreadId, Config)
    tryListener i cfg = catch ((,cfg) <$> runFrontendSafeFork' cfg)
        (if i <= 0 then throwIO else (\(_ :: SomeException) -> threadDelay 4900 >> (tryListener (i-1) =<< testConfig)))

runFrontendSafeFork' :: Config -> IO ThreadId
runFrontendSafeFork' cfg = do
    threadId <- forkIO $ runFrontend cfg
    waitForListener 37 >> return threadId
  where
    waitForListener :: Int -> IO ()
    waitForListener i = catch (void . Network.Wreq.get $ mkServerUri cfg "/")
        (if i <= 0 then throwIO else (\(_ :: SomeException) -> threadDelay 4900 >> waitForListener (i-1)))

someTestUser :: User
someTestUser = user
  where
    user = User
        { _userMeta      = metainfo
        , _userLogin     = "VorNam"
        , _userFirstName = "Vorname"
        , _userLastName  = "Name"
        , _userRoleSet   = Set.singleton Admin
        , _userDesc      = nil
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


-- * selenium webdriver

wdConfig :: WD.WDConfig
wdConfig = useChrome WD.defaultConfig
  where
    useChrome = WD.useBrowser (WD.chrome { WD.chromeBinary = Just "/usr/bin/chromium-browser"
                                   , WD.chromeOptions = ["--no-sandbox"]
                                   })

runWDAula :: (MonadIO m) => WD.WD a -> m (Maybe a)
runWDAula = liftIO . timeout (1000 * globalTimeout) . WD.runSession wdConfig . WD.finallyClose

-- | in ms
globalTimeout :: Num n => n
globalTimeout = 10300
