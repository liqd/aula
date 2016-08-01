{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Wall -Werror #-}

module Main where

import Control.Category
import Control.Lens ((^.), (.~))
import Control.Monad (filterM, void)
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
import Text.Show.Pretty
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
import Data.Tree

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

copyDir :: FilePath -> FilePath -> IO ()
copyDir from to = unfoldTreeM dirTree (Left "") >>= copyTree
  where
    dirTree f@(Right _path) = pure (f, [])
    dirTree d@(Left path) = do
        contents <- (path </>) <$$> getDirectoryContentsNoDots (from </> path)
        dirs  <- filterM (doesDirectoryExist . (from </>)) contents
        files <- filterM (doesFileExist      . (from </>)) contents
        pure (d, map Left dirs <> map Right files)

    copyTree (Node (Right file) []) = copyFile (from </> file) (to </> file)
    copyTree (Node (Right _file) _) = error "copyDir: impossible case."
    copyTree (Node (Left dir) contents) = do
        createDirectoryIfMissing True (to </> dir)
        mapM_ copyTree contents

copyStaticDir :: Config -> IO ()
copyStaticDir cfg = do
    staticSrc <- fmap (</> "static") Paths_aula.getDataDir
    copyDir staticSrc (cfg ^. htmlStatic)

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
    setCurrentDirectoryToAulaRoot
    -- FIXME: Do not use print.
    cfg <- readConfig print CrashMissing

    createDirectoryIfMissing True (cfg ^. avatarPath)
    createDirectoryIfMissing True (cfg ^. htmlStatic)
    checkAvatarPathExistsAndIsEmpty     cfg
    checkStaticHtmlPathExistsAndIsEmpty cfg

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
    args <- getArgs
    case options args of
        Nothing -> putStrLn usage >> exitFailure
        Just o  -> do
            createInitState cfg o
            initCsrfToken
            putStrLn "DONE!"
