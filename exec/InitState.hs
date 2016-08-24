{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wall -Werror #-}

module Main where

import Control.Category
import Control.Lens ((^.), (.~))
import Control.Monad (void)
import Data.List
import Data.String.Conversions
import Data.Yaml
import Prelude hiding (log, id, (.))
import Servant (unNat)
import System.Directory
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.Exit
import System.Process (system)
import Thentos.CookieSession.CSRF

import qualified Data.ByteString as BS
import qualified Data.Text as ST

import Action (ActionEnv(..), update)
import Action.Implementation
import Config
import Daemon (logDaemon, msgDaemonSend, start)
import Data.Markdown (markdown)
import DemoData (genSchoolSpace, genAdminUser)
import Logger
import Persistent.Api
import Persistent (withPersist)
import Types.Prelude (exceptToFail)
import Types.Log

import Paths_aula


-- * options

data Options = Options {
      opAdminUser :: String
    , opAdminPwd  :: String
    , opTermFile  :: FilePath
    }

options :: [String] -> Maybe Options
options os = Options <$> arg adminArg <*> arg adminPwdArg <*> arg termsFileArg
  where
    adminArg     = "admin"
    adminPwdArg  = "admin-pwd"
    termsFileArg = "terms-of-use"
    arg a = let a' = "--" <> a <> "="
            in stripPrefix a' =<< find (a' `isPrefixOf`) os

printUsage :: IO ()
printUsage = unSendLogMsg stderrLog $ LogEntry ERROR usage

usage :: ST
usage = ST.unlines
    [ "Usage: aula-init-state --admin=username --admin-pwd=password --terms-of-use=terms.md"
    , "       $AULA_ROOT_PATH must be set to target directory."
    , "       Config file $AULA_ROOT_PATH/aula.yaml must exist."
    ]


-- * initialize state

runBoostrap :: Config -> Action () -> IO ()
runBoostrap cfg action = do
    log <- logDaemon (cfg ^. logging)
    void $ log ^. start
    let logMsg = SendLogMsg $ log ^. msgDaemonSend
    withPersist cfg $ \rp -> do
        let runAction = mkRunAction (ActionEnv rp cfg logMsg Nothing)
        unNat (exceptToFail . runAction) action

createInitState :: Config -> Options -> IO ()
createInitState cfg o = do
    terms <- either (error . show) id . markdown . cs <$> readFile (opTermFile o)
    runBoostrap cfg $ do
        genSchoolSpace
        genAdminUser (cs $ opAdminUser o) (cs $ opAdminPwd o)
        update $ SetTermsOfUse terms


-- * initialize csrf token in aula.yaml

initCsrfToken :: IO ()
initCsrfToken = do
    cfg <- readConfig CrashMissing
    rnd <- genCsrfSecret
    writeConfig . (cfgCsrfSecret .~ rnd) $ cfg

-- | Overwrites all manual changes.  use with care!
writeConfig :: Config -> IO ()
writeConfig cfg = configFilePath >>= \(Just path) -> BS.writeFile path (encode cfg)


-- * main

-- FIXME: write test script that is run as part of the release process.
main :: IO ()
main = do
    setCurrentDirectoryToAulaRoot
    cfg <- readConfig CrashMissing

    opts <- maybe (printUsage >> exitFailure) pure . options =<< getArgs
    dataDir <- Paths_aula.getDataDir

    let cloneDir item = copyDir item "."
        copyDir item to = do
            ExitSuccess <- system $ unwords ["cp -a", dataDir </> item, to]
            pure ()

    cloneDir `mapM_` ["README.md", "docs", "scripts", "docker", "default-avatars", "null-terms-of-use.md"]
    ExitSuccess <- system "chmod +x ./scripts/* ./docker/*"
    copyDir "static" (cfg ^. htmlStatic)
    createDirectoryIfMissing True (cfg ^. avatarPath)
    checkAvatarPathExistsAndIsEmpty cfg

    wd <- getCurrentDirectory
    logmotd cfg wd

    createInitState cfg opts
    initCsrfToken
    unSendLogMsg (aulaLog (cfg ^. logging)) $ LogEntry INFO "done."
