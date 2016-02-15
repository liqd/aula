{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Api.Persistent
    ( Persist
    , AulaLens
    , AulaGetter
    , mkRunPersist

    -- * generic
    , getDb
    , addDb
    , modifyDb
    , findIn
    , findInBy
    , findInById
    , findAllIn
    , findAllInBy

    , getIdeas
    , addIdea
    , getUsers
    , addUser
    , getTopics
    , addTopic
    , findTopic
    , findUserByLogin
    , findIdeasByTopicId
    , findIdeasByTopic
    , loginUser
    , dbIdeas
    , dbUsers
    , dbTopics
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
    { _dbIdeas       :: [Idea]
    , _dbUsers       :: [User]
    , _dbTopics      :: [Topic]
    , _dbCurrentUser :: Maybe (AUID User)
    }
  deriving (Eq, Show, Read)

makeLenses ''AulaData

type AulaLens a = Lens' AulaData a
type AulaGetter a = Getter AulaData a

emptyAulaData :: AulaData
emptyAulaData = AulaData [] [] [] Nothing

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

getDb :: AulaGetter a -> Persist a
getDb l = Persist . ReaderT $ fmap (view l) . atomically . readTVar

modifyDb :: AulaLens a -> (a -> a) -> Persist ()
modifyDb l f = Persist . ReaderT $ \state -> atomically $ modifyTVar' state (l %~ f)

addDb :: AulaLens [a] -> a -> Persist ()
addDb l a = modifyDb l (a:)

findIn :: AulaGetter [a] -> (a -> Bool) -> Persist (Maybe a)
findIn l p = find p <$> getDb l

findAllIn :: AulaGetter [a] -> (a -> Bool) -> Persist [a]
findAllIn l p = filter p <$> getDb l

findInBy :: Eq b => AulaGetter [a] -> Lens' a b -> b -> Persist (Maybe a)
findInBy l f b = findIn l (\x -> x ^. f == b)

findAllInBy :: Eq b => AulaGetter [a] -> Lens' a b -> b -> Persist [a]
findAllInBy l f b = findAllIn l (\x -> x ^. f == b)

findInById :: HasMetaInfo a => AulaGetter [a] -> AUID a -> Persist (Maybe a)
findInById l = findInBy l _Id

getIdeas :: Persist [Idea]
getIdeas = getDb dbIdeas

addIdea :: Idea -> Persist ()
addIdea = addDb dbIdeas

getUsers :: Persist [User]
getUsers = getDb dbUsers

addUser :: User -> Persist ()
addUser = addDb dbUsers

getTopics :: Persist [Topic]
getTopics = getDb dbTopics

addTopic :: Topic -> Persist ()
addTopic = addDb dbTopics

findUserByLogin :: ST -> Persist (Maybe User)
findUserByLogin = findInBy dbUsers userLogin

findTopic :: AUID Topic -> Persist (Maybe Topic)
findTopic = findInById dbTopics

findIdeasByTopicId :: AUID Topic -> Persist [Idea]
findIdeasByTopicId topicId = findAllIn dbIdeas (\idea -> idea ^? ideaTopic . _Just . _Id == Just topicId)

findIdeasByTopic :: Topic -> Persist [Idea]
findIdeasByTopic = findIdeasByTopicId . view _Id

-- | FIXME: anyone can login
-- | FIXME: every login changes all other logins
loginUser :: ST -> Persist ()
loginUser login = modifyDb dbCurrentUser . const . fmap (view _Id) =<< findUserByLogin login
