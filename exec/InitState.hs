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
import System.IO
import System.Process (system)
import Text.Show.Pretty (ppShow)
import Thentos.CookieSession.CSRF

import qualified Data.ByteString as BS

import Action (ActionEnv(..), update)
import Action.Implementation
import Config
import Daemon (logDaemon, msgDaemonSend, start)
import Data.Markdown (markdown)
import DemoData (genSchoolSpace, genAdminUser)
import Persistent.Api
import Persistent (withPersist)
import Types.Prelude (exceptToFail)

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

usage :: String
usage = unlines
    [ "Usage: aula-init-state --admin=username --admin-pwd=password --terms-of-use=terms.md"
    , "       $AULA_ROOT_PATH must be set to target directory."
    , "       Config file $AULA_ROOT_PATH/aula.yaml must exist."
    ]


-- * initialize state

runBoostrap :: Config -> Action () -> IO ()
runBoostrap cfg action = do
    log <- logDaemon (cfg ^. logging)
    void $ log ^. start
    let logMsg = log ^. msgDaemonSend
    withPersist logMsg cfg $ \rp -> do
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
    cfg <- readConfig print CrashMissing
    rnd <- genCsrfSecret
    writeConfig . (cfgCsrfSecret .~ rnd) $ cfg

-- | Overwrites all manual changes.  use with care!
writeConfig :: Config -> IO ()
writeConfig cfg = configFilePath >>= \(Just path) -> BS.writeFile path (encode cfg)


-- * main

main :: IO ()
main = do
    opts <- maybe (putStrLn usage >> exitFailure) pure . options =<< getArgs
    dataDir <- Paths_aula.getDataDir

    setCurrentDirectoryToAulaRoot
    -- FIXME: Do not use print.
    cfg <- readConfig print CrashMissing

    let cloneDir item = copyDir item "."
        copyDir item to = do
            ExitSuccess <- system $ unwords ["cp -a", dataDir </> item, to]
            pure ()

    cloneDir `mapM_` ["README.md", "docs", "scripts", "docker", "default-avatars"]
    ExitSuccess <- system "chmod +x ./scripts/* ./docker/*"
    copyDir "static" (cfg ^. htmlStatic)
    createDirectoryIfMissing True (cfg ^. avatarPath)
    checkAvatarPathExistsAndIsEmpty cfg

    wd <- getCurrentDirectory
    hPutStrLn stderr $ unlines
        [ ""
        , "this is aula-init-state!"
        , "\nrelease:"
        , Config.releaseVersion
        , "\nroot path:"
        , wd
        , "\nsetup:", ppShow cfg
        , ""
        ]

    createInitState cfg opts
    initCsrfToken
    putStrLn "DONE!"
