{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-unused-imports #-}

module Main where

import Control.Exception
import Control.Lens ((^.), has)
import Control.Monad
import Control.Monad.Trans.Except
import Data.String.Conversions
import Data.List

import Servant.Server
import System.Directory
import qualified Data.Map as Map
import qualified Data.Set as Set

import Action
import Action.Implementation (Action, mkRunAction)
import Config
import Daemon
import Persistent
import Persistent.Api
import Types


main :: IO ()
main = do
  setCurrentDirectoryToAulaRoot
  cfg <- readConfig CrashMissing
  checkSendMail cfg
  wd <- getCurrentDirectory
  logmotd cfg wd
  resetAllPasswords cfg >>= writeFile "./aula-reset-all-passwords.csv" . intercalate "\n" . fmap (intercalate ",")

resetAllPasswords :: Config -> IO [[String]]
resetAllPasswords cfg = runAction cfg $ do
  us <- query getActiveUsers
  reset `mapM` filter only us
  where
    only :: User -> Bool
    only usr = or [prop rl | rl   <- Set.toList $ usr ^. userRoleSet
                           , prop <- [has _Student, has _Moderator]]

    reset :: User -> Action [String]
    reset usr = do
      newpwd <- mkRandomPassword
      resetPassword (usr ^. userMeta . metaKey) newpwd
      pure [ usr ^. userLogin . unUserLogin . csi
           , usr ^. userFirstName . unUserFirstName . csi
           , usr ^. userLastName . unUserLastName . csi
           , intercalate " / " $ uilabel <$> Set.toList (usr ^. userRoleSet)
           , newpwd ^. unInitialPassword . csi
           ]

-- | This function is copied together from the top-level functions in module "Frontend".  Re-throws
-- exceptions as 'ErrorCall'.  FIXME: resolve code duplication.
runAction :: forall m a. (m a ~ Action a) => Config -> m a -> IO a
runAction cfg act = do
  log_ <- logDaemon (cfg ^. logging)
  void $ log_ ^. start
  withPersist cfg $ \rp -> do
    let ra :: IO (Either ServantErr a)
        ra = runExceptT (unNat (mkRunAction (ActionEnv rp cfg (SendLogMsg $ log_ ^. msgDaemonSend) Nothing)) act)
    ra >>= \case
      Right v -> pure v
      Left e -> throwIO . ErrorCall . show $ e
