{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Main where

import Control.Exception
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.Trans.Except
import Data.String.Conversions
import Servant.Server
import System.Directory
import qualified Data.Map as Map

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
    checkAvatarPathExists cfg `catch` (\(ErrorCall msg) -> throwIO . ErrorCall $
        msg <> "\n\ndid you run aula-init-state?\n\n")

    wd <- getCurrentDirectory
    logmotd cfg wd

    migrateDB cfg


-- | Perform operations on startup that fix old errors.  'migrateDB' is idempotent, i.e. it is safe
-- to run it more than once on the same DB.
migrateDB :: Config -> IO ()
migrateDB cfg = runAction cfg $ do
  fix1033 :: Action ()
  Action.log $ LogEntry INFO "migration complete."
  where
    -- https://github.com/liqd/aula/issues/1033 (v0.34.1 => v0.34.2)
    fix1033 :: ActionM m => m ()
    fix1033 = do
      ideas <- query getIdeas
      forM_ ideas $ \idea -> do
        desc' <- sanify $ idea ^. ideaDesc
        title' <- sanify' $ idea ^. ideaTitle
        Action.editIdea (idea ^. ideaMeta . metaKey) ProtoIdea
                               { _protoIdeaTitle      = title'
                               , _protoIdeaDesc       = desc'
                               , _protoIdeaCategory   = idea ^. ideaCategory
                               , _protoIdeaLocation   = idea ^. ideaLocation
                               }
        let comments :: [Comment] = Map.elems $ idea ^. ideaComments
        forM_ comments $ \comment -> do
          let k = comment ^. commentMeta . metaKey
              t = comment ^. commentText
          update . SetCommentDesc k =<< sanify t

      users <- query getAllUsers
      forM_ users $ \user -> do
        desc' <- sanify $ user ^. userDesc
        update $ SetUserDesc (user ^. userMeta . metaKey) desc'

    sanify :: ActionM m => Document -> m Document
    sanify = sanify'' . unMarkdown

    sanify' :: ActionM m => ST -> m ST
    sanify' = fmap unMarkdown . sanify''

    sanify'' :: ActionM m => ST -> m Document
    sanify'' doc = case markdown doc of
      Right doc' -> pure doc'
      Left err -> do
        now <- getCurrentTimestamp
        Action.log $ LogEntry ERROR (cs $ "sanitizing Document failed: " <> show (now, doc, err))
        case markdown $ "deleted document, see logfile at " <> cs (show now) of
          Right doc' -> pure doc'
          Left _err' -> error "impossible."

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
