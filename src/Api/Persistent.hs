{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Api.Persistent
    ( Persist
    , AulaData
    , mkRunPersist
    , getDb
    , addDb
    , modifyDb
    , getIdeas
    , addIdea
    , getUsers
    , addUser
    , findUserByLogin
    , loginUser
    , dbIdeas
    , dbUsers
    , dbCurrentUser
    )
where

import Data.Foldable (find)
import Data.String.Conversions
import Control.Concurrent.STM
import Control.Monad.Trans.Reader
import Control.Lens
import Control.Monad.IO.Class
import Servant.Server ((:~>)(Nat))

import Types


data AulaData = AulaData
    { _dbIdeas :: [Idea]
    , _dbUsers :: [User]
    , _dbCurrentUser :: Maybe (AUID User)
    }
  deriving (Eq, Show, Read)

makeLenses ''AulaData

emptyAulaData :: AulaData
emptyAulaData = AulaData [] [] Nothing

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

getDb :: Lens' AulaData a -> Persist a
getDb l = Persist . ReaderT $ fmap (view l) . atomically . readTVar

modifyDb :: Lens' AulaData a -> (a -> a) -> Persist ()
modifyDb l f = Persist . ReaderT $ \state -> atomically $ modifyTVar' state (l %~ f)

addDb :: Lens' AulaData [a] -> a -> Persist ()
addDb l a = modifyDb l (a:)

getIdeas :: Persist [Idea]
getIdeas = getDb dbIdeas

addIdea :: Idea -> Persist ()
addIdea = addDb dbIdeas

getUsers :: Persist [User]
getUsers = getDb dbUsers

addUser :: User -> Persist ()
addUser = addDb dbUsers

findUserByLogin :: ST -> Persist (Maybe User)
findUserByLogin login = find (\u -> u ^. userLogin == login) <$> getUsers

-- | FIXME: anyone can login
-- | FIXME: every login changes all other logins
loginUser :: ST -> Persist ()
loginUser login = modifyDb dbCurrentUser . const . fmap (view _Id) =<< findUserByLogin login
