{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wall -Werror #-}

module Main where

import Prelude hiding (log, id, (.))
import Control.Category

import Control.Lens
import Control.Monad (void)
import Data.List
import Data.String.Conversions
import Servant (unNat)
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.IO
import Text.Show.Pretty

import Config
import Daemon (logDaemon, msgDaemonSend, start)
import DemoData (genSchoolSpace, genAdminUser)
import Types.Prelude (exceptToFail)

import Action (ActionEnv(..), update)
import Action.Implementation
import Data.Markdown (markdown)
import Persistent (withPersist)
import Persistent.Api


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
    , ""
    , "       The aula.yaml should be present."
    ]


-- * initialize state

runBoostrap :: Config -> Action () -> IO ()
runBoostrap cfg action = do
    log <- logDaemon (cfg ^. logging)
    void $ log ^. start
    let logMsg = log ^. msgDaemonSend
    withPersist logMsg cfg $ \rp -> do
        let runAction = mkRunAction (ActionEnv rp cfg logMsg)
        unNat (exceptToFail . runAction) action

createInitState :: Config -> Options -> IO ()
createInitState cfg o = do
    terms <- either (error . show) id . markdown . cs <$> readFile (opTermFile o)
    runBoostrap cfg $ do
        genSchoolSpace
        genAdminUser (cs $ opAdminUser o) (cs $ opAdminPwd o)
        update $ SetTermsOfUse terms


-- * main

main :: IO ()
main = do
    setCurrentDirectoryToAulaRoot
    -- FIXME: Do not use print.
    cfg <- readConfig print CrashMissing
    wd <- getCurrentDirectory
    hPutStrLn stderr $ unlines
        [ ""
        , "this is aula!"
        , "\nrelease:"
        , Config.releaseVersion
        , "\nroot path:"
        , wd
        , "\nsetup:", ppShow cfg
        , ""
        ]
    args <- getArgs
    case options args of
        Nothing -> putStrLn usage >> exitFailure
        Just o  -> createInitState cfg o >> putStrLn "DONE!"
