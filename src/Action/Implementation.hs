{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE PackageImports             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wall -Werror #-}

-- | The 'Action.Implementation' module contains a monad stack implmententation of the 'Action'
-- interface.
module Action.Implementation
    ( Action
    , mkRunAction

    -- in case of emergency...
    , actionIO
    )
where

import Codec.Picture
import Control.Exception (throwIO, try, ErrorCall(ErrorCall), SomeException(SomeException))
import Control.Lens
import Control.Monad.Except (MonadError, catchError)
import Control.Monad.IO.Class
import Control.Monad.RWS.Lazy
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT, withExceptT, runExcept)
import "cryptonite" Crypto.Random (MonadRandom(..))
import Crypto.Scrypt (getEncryptedPass, Pass(Pass), encryptPassIO')
import Data.Elocrypt (mkPassword)
import Data.Functor.Infix ((<$$>))
import Data.String.Conversions (LBS, cs)
import Data.Time.Clock (getCurrentTime)
import Prelude
import Servant
import Servant.Missing
import System.Directory (copyFile, doesFileExist)
import System.FilePath hiding (isValid)
import System.IO (IOMode(ReadMode), openFile, hClose, hFileSize)
import Test.QuickCheck  -- FIXME: remove
import Thentos.CookieSession.Types (freshSessionToken)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS (lines)
import qualified Data.ByteString.Lazy as LBS
import qualified Thentos.CookieSession.CSRF as CSRF (checkCsrfToken)

import Action
import Config
import Frontend.Constant
import Logger.EventLog
import Persistent
import Persistent.Api
import Types


-- * concrete monad type

-- | The actions a user can perform.
newtype Action a = MkAction { unAction :: ExceptT ActionExcept (RWST ActionEnv () UserState IO) a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadError ActionExcept
             , MonadReader ActionEnv
             , MonadState UserState
             )

actionIO :: IO a -> Action a
actionIO action = do
    eval <- MkAction . liftIO $ try action
    case eval of
        Left msg  -> throwError $ ActionIOExcept msg
        Right val -> pure val

instance GenArbitrary Action where  -- FIXME: remove
    genGen = actionIO . generate

instance HasSendMail ActionExcept ActionEnv Action where
    sendMailToAddress addr msg = MkAction $ do
        logger <- view envLogger
        sendMailToAddressIO logger addr msg

instance ActionLog Action where
    log msg = do
        level <- view (Config.getConfig . Config.logging . Config.logLevel)
        case msg of
            LogEntry level' _st
                | level' < level -> pure ()
            _                    -> actionIO =<< views envLogger ($ msg)

    readEventLog = do
        cfg <- viewConfig
        now <- getCurrentTimestamp
        erows <- actionIO . try $ rd cfg
        case erows of
            Left err   -> throwError $ ActionEventLogExcept err
            Right rows -> (EventLog now (cs $ cfg ^. exposedUrl) <$> (warmUp `mapM` rows))
                `catchError` (throwError . ActionEventLogExcept . otherErr)
      where
        rd :: Config -> IO [EventLogItemCold]
        rd cfg = (LBS.lines <$> LBS.readFile (cfg ^. logging . eventLogPath))
             >>= zipWithM adecode [1..]

        adecode :: Int -> LBS -> IO EventLogItemCold
        adecode i = either (throwIO . ErrorCall . msg) pure . Aeson.eitherDecode
          where
            msg aesonSays = "readEventLog:" <> show i <> ": " <> aesonSays

        otherErr :: Show msg => msg -> SomeException
        otherErr = SomeException . ErrorCall . (<> ourMsg) . show
          where
            ourMsg = "\nthis could be caused by a stale event log file with dangling references."

throwPersistError
    :: forall m b . (ActionUserHandler m, ActionLog m)
    => PersistExcept -> m b
throwPersistError e = do
    logEvent ERROR (cshow e)
    throwError $ ActionPersistExcept e

-- | FIXME: test this (particularly strictness and exceptions)
instance ActionPersist Action where
    queryDb = actionIO =<< view (envRunPersist . rpQuery)

    equery q = do
        db <- queryDb
        either throwPersistError pure $ runExcept (runReaderT q db)

    update ev = do
        either throwPersistError pure
            =<< actionIO =<< views (envRunPersist . rpUpdate) ($ ev)

instance MonadRandom Action where
    getRandomBytes = actionIO . getRandomBytes

instance ActionRandomPassword Action where
    mkRandomPassword = actionIO $ InitialPassword . cs . unwords <$> mkPassword `mapM` [4,3,5]
    mkRandomPasswordToken = actionIO $ PasswordToken . cs <$> mkPassword 120

instance ActionEncryptPassword Action where
    encryptPassword =
        actionIO . fmap (ScryptEncryptedPassword . getEncryptedPass) . encryptPassIO' . Pass . cs

instance ActionCurrentTimestamp Action where
    getCurrentTimestamp = actionIO $ Timestamp <$> getCurrentTime

instance ActionUserHandler Action where
    login uid = do
        usUserId .= Just uid
        sessionToken <- freshSessionToken
        usSessionToken .= Just sessionToken

    addMessage msg = usMessages %= (msg:)

    flushMessages = do
        msgs <- userState usMessages
        usMessages .= []
        pure $ reverse msgs

    userState = use

    logout = put userLoggedOut >> addMessage "Danke fürs Mitmachen!"

instance ActionCsrfToken Action where
    getCsrfToken   = use usCsrfToken
    checkCsrfToken = CSRF.checkCsrfToken

instance ReadTempFile Action where
    readTempFile = actionIO . LBS.readFile

instance CleanupTempFiles Action where
    cleanupTempFiles = actionIO . releaseFormTempFiles

instance ActionAvatar Action where
    readImageFile filePath = actionIO $ do
        h <- openFile filePath ReadMode
        s <- hFileSize h
        hClose h
        if s > 0
            then Just <$> readImage filePath
            else pure Nothing

    savePngImageFile p = actionIO . savePngImage p
    addInitialAvatarImage user = do
        initialAvatars <- actionIO $ do
            (initialAvatarsPath </>) <$$> getDirectoryContentsNoDots initialAvatarsPath

        file <- genGen (Test.QuickCheck.elements initialAvatars)
        updateAvatarByCopy user file


-- | Creates a natural transformation from Action to the servant handler monad.
-- See Frontend.runFrontend for the persistency of @UserState@.
mkRunAction :: ActionEnv -> Action :~> ExceptT ServantErr IO
mkRunAction env = Nat run
  where
    run = withExceptT runActionExcept . ExceptT . fmap (view _1) . runRWSTflip env userLoggedOut
        . runExceptT . unAction . (checkCurrentUser >>)
    runRWSTflip r s comp = runRWST comp r s

    checkCurrentUser = do
        isValid <- userState $ to validUserState
        unless isValid $ do
            logout
            throwError500 "Invalid internal user session state"

runActionExcept :: ActionExcept -> ServantErr
runActionExcept (ActionExcept e) = e
runActionExcept (ActionPersistExcept pe) = runPersistExcept pe
runActionExcept (ActionSendMailExcept e) = error500 # show e
runActionExcept (ActionEventLogExcept e) = error500 # show e
runActionExcept (ActionIOExcept e) = error500 # show e


-- | Do not call 'saveAvatar', but check if target files exist, and only if not, *copy* the source.
-- NOTE: The there is a very similar function to this one in DemoData generation.
updateAvatarByCopy :: User -> FilePath -> Action ()
updateAvatarByCopy user spath = do
    apath <- view (Config.getConfig . Config.avatarPath)
    actionIO $ do
        forM_ (Nothing : (Just <$> (avatarDefaultSize : avatarExtraSizes))) $ \dim -> do
            let tpath :: FilePath = user ^. _Id . avatarFile apath dim
            yes <- doesFileExist tpath
            unless yes $ copyFile spath tpath
