{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wall -Werror #-}

module Main where

import Data.List
import Data.String.Conversions
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.IO
import Text.Show.Pretty

import Frontend
import Config
import DemoData (genSchoolSpace, genAdminUser)

import Action (update)
import Data.Markdown (markdown)
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
