{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC #-}

module Api.NoPersistent
where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Lens
import Control.Monad.IO.Class
import Data.String (fromString)
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Servant
import Servant.HTML.Blaze
import System.IO.Unsafe (unsafePerformIO)

import Thentos.Frontend.Types

import Config
import Frontend.Html
import Types


data AulaData = AulaData
    { _dbIdeas :: [Idea]
    }
  deriving (Eq, Show, Read)

makeLenses ''AulaData

aulaState :: TVar AulaData
aulaState = unsafePerformIO . newTVarIO $ AulaData []


getIdeasH :: (MonadIO m) => m [Idea]
getIdeasH = liftIO . fmap (view dbIdeas) . atomically . readTVar $ aulaState

addIdeaH :: (MonadIO m) => Idea -> m ()
addIdeaH idea = liftIO . atomically $ modifyTVar' aulaState (dbIdeas %~ (idea:))
