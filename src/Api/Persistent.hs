{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeOperators               #-}

{-# OPTIONS_GHC -Werror #-}

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
    -- FIXME: Remove hack
    , forceLogin
    , addDbEntity
    )
where

import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.String.Conversions
import Data.Time.Clock (getCurrentTime)
import Control.Concurrent.STM
import Control.Monad.Trans.Reader
import Control.Lens
import Control.Monad.IO.Class
import Servant.Server ((:~>)(Nat))

import Types

import qualified Data.Set as Set (empty)

data AulaData = AulaData
    { _dbIdeas       :: [Idea]
    , _dbUsers       :: [User]
    , _dbTopics      :: [Topic]
    , _dbCurrentUser :: Maybe (AUID User)
    , _dbLastId :: Integer
    }
  deriving (Eq, Show, Read)

makeLenses ''AulaData

type AulaLens a = Lens' AulaData a
type AulaGetter a = Getter AulaData a

emptyAulaData :: AulaData
emptyAulaData = AulaData [] [] [] Nothing 0

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

addDb :: (HasMetaInfo a, FromPrototype a) => Lens' AulaData [a] -> Prototype a -> Persist a
addDb l pa = do
    u  <- currentUser
    i  <- nextId
    mi <- liftIO $ newMetaInfo u i
    let a = (metaInfo .~ mi) $ fromPrototype pa
    modifyDb l (a:)
    return a

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

addIdea :: Prototype Idea -> Persist Idea
addIdea = addDb dbIdeas

getUsers :: Persist [User]
getUsers = getDb dbUsers

addUser :: User -> Persist User
addUser = addDb dbUsers

getTopics :: Persist [Topic]
getTopics = getDb dbTopics

addTopic :: Prototype Topic -> Persist Topic
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

-------------------------------------------------------------------
-- HACK to make easy to emulate db savings from prototypes
-- FIXME: This is not part of the interface

-- | FIXME: Remove. Only used to add random generated entities.
addDbEntity :: Lens' AulaData [a] -> a -> Persist ()
addDbEntity l a = modifyDb l (a:)

forceLogin :: Integer -> Persist ()
forceLogin x = modifyDb dbCurrentUser (const (Just (AUID x)))

nextId :: Persist (AUID a)
nextId = do
    modifyDb dbLastId (+1)
    AUID <$> getDb dbLastId

currentUser :: Persist (AUID User)
currentUser = fromJust <$> getDb dbCurrentUser

instance FromPrototype User where
    fromPrototype = id

instance FromPrototype Idea where
    fromPrototype i =
           (ideaTitle    .~ (i ^. protoIdeaTitle))
         . (ideaDesc     .~ (i ^. protoIdeaDesc))
         . (ideaCategory .~ (i ^. protoIdeaCategory))
         $ emptyIdea

emptyIdea :: Idea
emptyIdea = Idea
    { _ideaMeta = Prelude.error "blah"
    , _ideaTitle = ""
    , _ideaDesc  = Markdown ""
    , _ideaCategory = CatRule
    , _ideaSpace    = SchoolSpace
    , _ideaTopic    = Nothing
    , _ideaComments = Set.empty
    , _ideaLikes    = Set.empty
    , _ideaQuorumOk = False
    , _ideaVotes    = Set.empty
    , _ideaFeasible = Nothing
    }

newMetaInfo :: AUID User -> AUID a -> IO (MetaInfo a)
newMetaInfo u i = do
    now <- Timestamp <$> getCurrentTime
    return $ MetaInfo
        { _metaId              = i
        , _metaCreatedBy       = u
        , _metaCreatedByLogin  = ""
        , _metaCreatedByAvatar = ""
        , _metaCreatedAt       = now
        , _metaChangedBy       = u
        , _metaChangedAt       = now
        }
