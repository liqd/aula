{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -Werror -Wall -fno-warn-orphans #-}

module Config
    ( Config(Config), SmtpConfig(SmtpConfig), LogConfig(..)
    , GetConfig(..), MonadReaderConfig
    , WarnMissing(DontWarnMissing, WarnMissing, CrashMissing)
    , PersistenceImpl(..)
    , aulaRoot
    , dbPath
    , defaultRecipient
    , exposedUrl
    , getSamplesPath
    , htmlStatic
    , avatarPath
    , listenerInterface
    , listenerPort
    , persistConfig
    , persistenceImpl
    , readConfig, configFilePath
    , releaseVersion
    , senderEmail
    , senderName
    , sendmailArgs
    , sendmailPath
    , setCurrentDirectoryToAulaRoot
    , smtpConfig
    , snapshotInterval
    , delegateLikes
    , timeoutCheckInterval
    , devMode
    , logging
    , logLevel
    , eventLogPath
    , unsafeTimestampToLocalTime
    , aulaTimeLocale
    , checkAvatarPathExists
    , checkAvatarPathExistsAndIsEmpty
    , checkStaticHtmlPathExists
    , cfgCsrfSecret
    )
where

import Control.Exception (throwIO, ErrorCall(ErrorCall))
import Control.Lens
import Control.Monad (unless)
import Control.Monad.Reader (MonadReader)
import Data.Functor.Infix ((<$$>))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (SBS, cs)
import Data.Time
import Data.Version (showVersion)
import Data.Yaml
import GHC.Generics
import System.Directory
import System.Environment
import System.FilePath ((</>))
import Thentos.CookieSession.CSRF (GetCsrfSecret(..), CsrfSecret(..))

import qualified System.IO.Unsafe

import Logger
import Types


import qualified Paths_aula as Paths
-- (if you are running ghci and Paths_aula is not available, try `-idist/build/autogen`.)


-- | FIXME: move this instance upstream and remove -fno-warn-orphans for this module.
instance ToJSON CsrfSecret where
  toJSON (CsrfSecret s) = String $ cs s

-- | FIXME: move this instance upstream and remove -fno-warn-orphans for this module.
instance FromJSON CsrfSecret where
  parseJSON o = CsrfSecret . (cs :: String -> SBS) <$> parseJSON o

data PersistenceImpl = AcidStateInMem | AcidStateOnDisk
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Enum, Bounded)

data SmtpConfig = SmtpConfig
    { _senderName       :: String
    , _senderEmail      :: String
    , _defaultRecipient :: Maybe String  -- (e.g. for use in demo data.)
    , _sendmailPath     :: String
    , _sendmailArgs     :: [String]
   -- ^ Not using 'ST' here since Network.Mail.Mime wants 'String' anyway.
    }
  deriving (Show, Generic, ToJSON, FromJSON)

makeLenses ''SmtpConfig

data PersistConfig = PersistConfig
    { _dbPath           :: String
    , _persistenceImpl  :: PersistenceImpl
    , _snapshotInterval :: Timespan
    }
  deriving (Show, Generic, ToJSON, FromJSON)

makeLenses ''PersistConfig

data LogConfig = LogConfig
    { _logLevel     :: LogLevel
    , _eventLogPath :: FilePath
    }
  deriving (Show, Generic, ToJSON, FromJSON)

makeLenses ''LogConfig

data Config = Config
    { _exposedUrl           :: String  -- e.g. https://aula-stage.liqd.net
    , _listenerInterface    :: String
    , _listenerPort         :: Int
    , _htmlStatic           :: FilePath
    , _avatarPath           :: FilePath  -- avatars are stored in this directory
    , _cfgCsrfSecret        :: CsrfSecret
    , _logging              :: LogConfig
    , _persistConfig        :: PersistConfig
    , _smtpConfig           :: SmtpConfig
    , _delegateLikes        :: Bool
    , _timeoutCheckInterval :: Timespan
    -- ^ Topics which needs to change phase due to a timeout will
    -- be checked at this interval.
    -- * once per day would be the minmum
    -- * 4 times a day (every 6 hours) would ensures that
    --   all the topics are ready at least at 6am.
    , _devMode              :: Bool
    }
  deriving (Show, Generic, ToJSON, FromJSON)  -- FIXME: make nicer JSON field names.

makeLenses ''Config

class GetConfig r where
    getConfig :: Getter r Config

    viewConfig :: MonadReader r m => m Config
    viewConfig = view getConfig

type MonadReaderConfig r m = (MonadReader r m, GetConfig r)

instance GetConfig Config where
    getConfig = id

instance GetCsrfSecret Config where
    csrfSecret = pre cfgCsrfSecret

defaultSmtpConfig :: SmtpConfig
defaultSmtpConfig = SmtpConfig
    { _senderName       = "Aula Notifications"
    , _senderEmail      = "aula@example.com"
    , _defaultRecipient = Nothing
    , _sendmailPath     = "/usr/sbin/sendmail"
    , _sendmailArgs     = ["-t"]
    }

defaultPersistConfig :: PersistConfig
defaultPersistConfig = PersistConfig
    { _dbPath           = "./state/AulaData"
    , _persistenceImpl  = AcidStateInMem
    , _snapshotInterval = TimespanMins 47
    }

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
    { _logLevel     = DEBUG
    , _eventLogPath = "./aulaEventLog.json"
    }

defaultConfig :: Config
defaultConfig = Config
    { _exposedUrl           = "http://localhost:8080"
    , _listenerInterface    = "0.0.0.0"
    , _listenerPort         = 8080
    , _htmlStatic           = "./static"
    , _avatarPath           = "./avatars"
    , _cfgCsrfSecret        = CsrfSecret "please-replace-this-with-random-secret"
    , _logging              = defaultLogConfig
    , _persistConfig        = defaultPersistConfig
    , _smtpConfig           = defaultSmtpConfig
    , _delegateLikes        = True
    , _timeoutCheckInterval = TimespanHours 6
    , _devMode              = False
    }

data WarnMissing = DontWarnMissing | WarnMissing | CrashMissing
  deriving (Eq, Show)

readConfig :: SendLogMsg -> WarnMissing -> IO Config
readConfig logger warnMissing = configFilePath >>= maybe (errr msgAulaPathNotSet >> dflt) decodeFileDflt
  where
    dflt :: IO Config
    dflt = pure defaultConfig

    decodeFileDflt :: FilePath -> IO Config
    decodeFileDflt fp = decodeFileEither fp >>= either (\emsg -> errr (msgParseError emsg) >> dflt) pure

    msgAulaPathNotSet :: [String]
    msgAulaPathNotSet =
        [ "no config file found: $AULA_ROOT_PATH not set."
        , "to fix this, write the following lines to $AULA_ROOT_PATH/aula.yaml:"
        ]

    msgParseError :: Show a => a -> [String]
    msgParseError emsg =
        [ "could not read config file:"
        , show emsg
        , "to fix this, write the following lines to $AULA_ROOT_PATH/aula.yaml:"
        ]

    errr :: [String] -> IO ()
    errr msgH = case warnMissing of
        DontWarnMissing -> pure ()
        WarnMissing     -> logger . LogEntry ERROR $ cs msgs
        CrashMissing    -> throwIO . ErrorCall $ msgs
      where
        msgs = unlines $ [""] <> msgH <> ["", cs $ encode defaultConfig]

configFilePath :: IO (Maybe FilePath)
configFilePath = (</> "aula.yaml") <$$> aulaRoot

aulaRoot :: IO (Maybe FilePath)
aulaRoot = lookup "AULA_ROOT_PATH" <$> getEnvironment

setCurrentDirectoryToAulaRoot :: IO ()
setCurrentDirectoryToAulaRoot = aulaRoot >>= maybe (pure ()) setCurrentDirectory

getSamplesPath :: IO FilePath
getSamplesPath = fromMaybe (error msg) . lookup var <$> getEnvironment
  where
    var = "AULA_SAMPLES"
    msg = "please set $" <> var <> " to a path (will be created if n/a)"


-- * release version

releaseVersion :: String
releaseVersion = "[v" <> showVersion Paths.version <> "]"


-- * system time, time zones

-- | This works as long as the running system doesn't move from one time zone to the other.  It
-- would be nicer to make that an extra 'Action' class, but I argue that it's not worth the time to
-- do it (and to have to handle the slightly larger code base from now on).
unsafeTimestampToLocalTime :: Timestamp -> ZonedTime
unsafeTimestampToLocalTime (Timestamp t) = System.IO.Unsafe.unsafePerformIO $ utcToLocalZonedTime t

aulaTimeLocale :: TimeLocale
aulaTimeLocale = defaultTimeLocale
  { knownTimeZones = knownTimeZones defaultTimeLocale
                  <> [TimeZone (1 * 60) False "CET", TimeZone (2 * 60) True "CEST"] }

checkAvatarPathExists :: Config -> IO ()
checkAvatarPathExists cfg = checkPathExists (cfg ^. avatarPath)

checkAvatarPathExistsAndIsEmpty :: Config -> IO ()
checkAvatarPathExistsAndIsEmpty cfg =
    checkPathExistsAndIsEmpty (cfg ^. avatarPath)

checkStaticHtmlPathExists :: Config -> IO ()
checkStaticHtmlPathExists cfg =
    checkPathExistsAndIsEmpty (cfg ^. htmlStatic)

checkPathExists :: FilePath -> IO ()
checkPathExists path = do
    exists <- doesDirectoryExist path
    unless exists . throwIO . ErrorCall $
        show path <> " does not exist or is not a directory."

checkPathExistsAndIsEmpty :: FilePath -> IO ()
checkPathExistsAndIsEmpty path = do
    checkPathExists path
    isempty <- null <$> getDirectoryContentsNoDots path
    unless isempty . throwIO . ErrorCall $
        show path <> " does not exist, is not a directory, or is not empty."
