{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Config
    ( Config
    , WarnMissing(DontWarnMissing, WarnMissing, CrashMissing)
    , PersistenceImpl(..)
    , dbPath
    , htmlStatic
    , listenerInterface
    , listenerPort
    , persistenceImpl
    , readConfig
    , aulaRoot
    , setCurrentDirectoryToAulaRoot
    , getSamplesPath
    , logger
    )
where

import Control.Exception (throwIO, ErrorCall(ErrorCall))
import Control.Lens
import Control.Monad (when)
import Data.Functor.Infix ((<$$>))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String.Conversions (SBS, cs)
import Data.Yaml
import GHC.Generics
import System.Directory
import System.Environment
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Thentos.Frontend.CSRF (GetCsrfSecret(..), CsrfSecret(..))


-- | FIXME: move this instance upstream and remove -fno-warn-orphans for this module.
instance ToJSON CsrfSecret where
  toJSON (CsrfSecret s) = String $ cs s

-- | FIXME: move this instance upstream and remove -fno-warn-orphans for this module.
instance FromJSON CsrfSecret where
  parseJSON o = CsrfSecret . (cs :: String -> SBS) <$> parseJSON o

data PersistenceImpl = AcidStateInMem | AcidStateOnDisk
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, Enum, Bounded)

data Config = Config
    { _dbPath            :: FilePath
    , _listenerInterface :: String
    , _listenerPort      :: Int
    , _htmlStatic        :: FilePath
    , _cfgCsrfSecret     :: CsrfSecret
    , _logLevel          :: Bool  -- (see 'logger' below)
    , _persistenceImpl   :: PersistenceImpl
    }
  deriving (Show, Generic, ToJSON, FromJSON)

makeLenses ''Config

instance GetCsrfSecret Config where
    csrfSecret = pre cfgCsrfSecret

defaultConfig :: Config
defaultConfig = Config
    { _dbPath            = "./state/AulaData"
    , _listenerInterface = "0.0.0.0"
    , _listenerPort      = 8080
    , _htmlStatic        = "./static"
    -- FIXME: BEWARE, this "secret" is hardcoded and public.
    , _cfgCsrfSecret     = CsrfSecret "1daf3741e8a9ae1b39fd7e9cc7bab44ee31b6c3119ab5c3b05ac33cbb543289c"
    , _logLevel          = False
    , _persistenceImpl   = AcidStateInMem
    }

data WarnMissing = DontWarnMissing | WarnMissing | CrashMissing
  deriving (Eq, Show)

readConfig :: WarnMissing -> IO Config
readConfig warnMissing = configFilePath >>= maybe (errr msgAulaPathNotSet >> dflt) decodeFileDflt
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
        WarnMissing     -> logger (logLevel .~ True $ defaultConfig) msgs
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


-- * logging

-- | FIXME: this will become more sophisticated.  related: #65
logger :: Config -> String -> IO ()
logger cfg = when (cfg ^. logLevel) . hPutStrLn stderr
