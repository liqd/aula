{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeOperators               #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Api.Persistent
    ( Persist
    , AMap
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
    , modifyIdea
    , findIdea
    , getUsers
    , addUser
    , modifyUser
    , getTopics
    , addTopic
    , modifyTopic
    , moveIdeaToTopic
    , findTopic
    , findUserByLogin
    , findIdeasByTopicId
    , findIdeasByTopic
    , loginUser
    , dbIdeas
    , dbUsers
    , dbTopics
    , dbIdeaMap
    , dbUserMap
    , dbTopicMap
    , dbCurrentUser
    -- FIXME: Remove hack
    , forceLogin
    , addDbEntity
    )
where

import Data.Foldable (find, for_)
import Data.Map (Map)
import Data.String.Conversions
import Data.Time.Clock (getCurrentTime)
import Control.Concurrent.STM
import Control.Monad (join)
import Control.Monad.Trans.Reader
import Control.Lens
import Control.Monad.IO.Class
import Servant.Server ((:~>)(Nat))

import Types

import qualified Data.Set as Set (empty)
import qualified Data.Map as Map

type AMap a = Map (AUID a) a

data AulaData = AulaData
    { _dbIdeaMap     :: AMap Idea
    , _dbUserMap     :: AMap User
    , _dbTopicMap    :: AMap Topic
    , _dbCurrentUser :: Maybe (AUID User)
    , _dbLastId      :: Integer
    }
  deriving (Eq, Show, Read)

makeLenses ''AulaData

type AulaLens a = Lens' AulaData a
type AulaGetter a = Getter AulaData a

dbIdeas :: AulaGetter [Idea]
dbIdeas = dbIdeaMap . to Map.elems

dbUsers :: AulaGetter [User]
dbUsers = dbUserMap . to Map.elems

dbTopics :: AulaGetter [Topic]
dbTopics = dbTopicMap . to Map.elems

emptyAulaData :: AulaData
emptyAulaData = AulaData Map.empty Map.empty Map.empty Nothing 0

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

addDb :: (HasMetaInfo a, FromProto a) => AulaLens (AMap a) -> Proto a -> Persist a
addDb l pa = do
    a  <- fromProto pa <$> nextMetaInfo
    modifyDb l $ at (a ^. _Id) .~ Just a
    return a

findIn :: AulaGetter [a] -> (a -> Bool) -> Persist (Maybe a)
findIn l p = find p <$> getDb l

findAllIn :: AulaGetter [a] -> (a -> Bool) -> Persist [a]
findAllIn l p = filter p <$> getDb l

findInBy :: Eq b => AulaGetter [a] -> Fold a b -> b -> Persist (Maybe a)
findInBy l f b = findIn l (\x -> x ^? f == Just b)

findAllInBy :: Eq b => AulaGetter [a] -> Fold a b -> b -> Persist [a]
findAllInBy l f b = findAllIn l (\x -> x ^? f == Just b)

findInById :: HasMetaInfo a => AulaGetter [a] -> AUID a -> Persist (Maybe a)
findInById l = findInBy l _Id

getIdeas :: Persist [Idea]
getIdeas = getDb dbIdeas

addIdea :: Proto Idea -> Persist Idea
addIdea = addDb dbIdeaMap

findIdea :: AUID Idea -> Persist (Maybe Idea)
findIdea = findInById dbIdeas

modifyAMap :: AulaLens (AMap a) -> AUID a -> (a -> a) -> Persist ()
modifyAMap l ident f = modifyDb l (at ident . _Just %~ f)

modifyIdea :: AUID Idea -> (Idea -> Idea) -> Persist ()
modifyIdea = modifyAMap dbIdeaMap

modifyUser :: AUID User -> (User -> User) -> Persist ()
modifyUser = modifyAMap dbUserMap

modifyTopic :: AUID Topic -> (Topic -> Topic) -> Persist ()
modifyTopic = modifyAMap dbTopicMap

getUsers :: Persist [User]
getUsers = getDb dbUsers

addUser :: User -> Persist User
addUser = addDb dbUserMap

getTopics :: Persist [Topic]
getTopics = getDb dbTopics

moveIdeaToTopic :: AUID Idea -> Maybe (AUID Topic) -> Persist ()
moveIdeaToTopic ideaId topicId = modifyIdea ideaId $ ideaTopic .~ topicId

addTopic :: Proto Topic -> Persist Topic
addTopic pt = do
    t <- addDb dbTopicMap pt
    -- FIXME a new topic should not be able to steal ideas from other topics of course the UI will
    -- hide this risk since only ideas without topics will be visible.
    -- Options:
    -- * Make it do nothing
    -- * Make it fail hard
    for_ (pt ^. protoTopicIdeas) $ \ideaId ->
        moveIdeaToTopic ideaId (Just $ t ^. _Id)
    return t

findUserByLogin :: ST -> Persist (Maybe User)
findUserByLogin = findInBy dbUsers userLogin

findTopic :: AUID Topic -> Persist (Maybe Topic)
findTopic = findInById dbTopics

findIdeasByTopicId :: AUID Topic -> Persist [Idea]
findIdeasByTopicId = findAllInBy dbIdeas ideaTopic . Just

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
addDbEntity :: HasMetaInfo a => AulaLens (AMap a) -> a -> Persist a
addDbEntity l pa = do
    m <- nextMetaInfo
    let a = pa & metaInfo .~ m
    modifyDb l $ at (a ^. _Id) .~ Just a
    return a

forceLogin :: Integer -> Persist ()
forceLogin x = modifyDb dbCurrentUser (const (Just (AUID x)))

nextId :: Persist (AUID a)
nextId = do
    modifyDb dbLastId (+1)
    AUID <$> getDb dbLastId

currentUser :: Persist (AUID User)
currentUser = (\(Just u) -> u) <$> getDb dbCurrentUser

instance FromProto User where
    fromProto u _ = u

instance FromProto Idea where
    fromProto i m =
           (ideaTitle    .~ (i ^. protoIdeaTitle))
         . (ideaDesc     .~ (i ^. protoIdeaDesc))
         . (ideaCategory .~ (i ^. protoIdeaCategory))
         $ emptyIdea
      where
        emptyIdea = Idea
            { _ideaMeta = m
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

instance FromProto Topic where
    fromProto t m = Topic
        { _topicMeta      = m
        , _topicTitle     = t ^. protoTopicTitle
        , _topicDesc      = t ^. protoTopicDesc
        , _topicImage     = t ^. protoTopicImage
        , _topicIdeaSpace = t ^. protoTopicIdeaSpace
        , _topicPhase     = PhaseRefinement
        }

-- | So far `newMetaInfo` is only used by `nextMetaInfo`.
newMetaInfo :: AUID User -> AUID a -> Persist (MetaInfo a)
newMetaInfo u i = liftIO $ do
    now <- Timestamp <$> getCurrentTime
    return $ MetaInfo
        { _metaId              = i
        , _metaCreatedBy       = u
        , _metaCreatedByLogin  = ""  -- FIXME: we should probably lookup user under 'u' and keep this in sync.
        , _metaCreatedByAvatar = ""  -- FIXME: dito.
        , _metaCreatedAt       = now
        , _metaChangedBy       = u
        , _metaChangedAt       = now
        }

nextMetaInfo :: Persist (MetaInfo a)
nextMetaInfo = join $ newMetaInfo <$> currentUser <*> nextId
