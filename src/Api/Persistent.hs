{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC #-}

module Api.Persistent
    ( Persist
    , runPersist
    , getIdeasH
    , addIdeaH
    )
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


-- | FIXME: call this type 'Action'?  Or 'Aula'?  Or 'AulaAction'?  As of the time of writing this
-- comment, it doesn't make sense to have separate abstractions for persistence layer (Transaction
-- in thentos) and application logic (Action in thentos).  to be discussed later?
newtype Persist a = Persist { runPersist :: IO a }
  deriving (Functor, Applicative, Monad, MonadIO)


getIdeasH :: Persist [Idea]
getIdeasH = Persist . fmap (view dbIdeas) . atomically . readTVar $ aulaState

addIdeaH :: Idea -> Persist ()
addIdeaH idea = Persist . atomically $ modifyTVar' aulaState (dbIdeas %~ (idea:))
