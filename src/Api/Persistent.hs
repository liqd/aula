{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Api.Persistent
    ( Persist
    , mkRunPersist
    , getIdeas
    , addIdea
    )
where

import Control.Concurrent.STM
import Control.Monad.Trans.Reader
import Control.Lens
import Control.Monad.IO.Class
import Servant.Server ((:~>)(Nat))

import Types


data AulaData = AulaData
    { _dbIdeas :: [Idea]
    }
  deriving (Eq, Show, Read)

makeLenses ''AulaData

emptyAulaData :: AulaData
emptyAulaData = AulaData []

-- | FIXME: call this type 'Action'?  Or 'Aula'?  Or 'AulaAction'?  As of the time of writing this
-- comment, it doesn't make sense to have separate abstractions for persistence layer (Transaction
-- in thentos) and application logic (Action in thentos).  to be discussed later?
newtype Persist a = Persist (ReaderT (TVar AulaData) IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

mkRunPersist :: IO (Persist :~> IO)
mkRunPersist = do
    tvar <- newTVarIO emptyAulaData
    let run (Persist c) = c `runReaderT` tvar
    return $ Nat run

getIdeas :: Persist [Idea]
getIdeas = Persist . ReaderT $ fmap (view dbIdeas) . atomically . readTVar

addIdea :: Idea -> Persist ()
addIdea idea = Persist . ReaderT $ \state -> atomically $ modifyTVar' state (dbIdeas %~ (idea:))
