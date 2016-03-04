{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE ViewPatterns                #-}

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

    , getSpaces
    , getIdeas
    , addIdeaSpaceIfNotExists
    , addIdea
    , modifyIdea
    , findIdea
    , findIdeasByTopicId
    , findIdeasByTopic
    , findIdeasByUserId
    , findWildIdeasBySpace
    , findUser
    , getUsers
    , addUser
    , addFirstUser
    , mkUserLogin
    , mkRandomPassword
    , modifyUser
    , getTopics
    , addTopic
    , modifyTopic
    , moveIdeasToTopic
    , findTopic
    , findTopicsBySpace
    , findUserByLogin
    , loginUser
    , logoutUser
    , dbIdeas
    , dbUsers
    , dbTopics
    , dbSpaceSet
    , dbIdeaMap
    , dbUserMap
    , dbTopicMap
    , dbCurrentUser
    , dbElaborationDuration
    , dbVoteDuration
    , dbSchoolQuorum
    , dbClassQuorum
    , adminUsernameHack
    )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, modifyTVar')
import Control.Lens
import Control.Monad (join, unless, replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Data.Elocrypt (mkPassword)
import Data.Foldable (find, for_)
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isNothing)
import Data.String.Conversions (ST, cs, (<>))
import Data.Set (Set)
import Data.Time.Clock (getCurrentTime)
import Servant.Server ((:~>)(Nat))

import Types

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as ST

-- FIXME: Remove
import Test.QuickCheck (generate, arbitrary)

type AMap a = Map (AUID a) a

data AulaData = AulaData
    { _dbSpaceSet            :: Set IdeaSpace
    , _dbIdeaMap             :: AMap Idea
    , _dbUserMap             :: AMap User
    , _dbTopicMap            :: AMap Topic
    , _dbCurrentUser         :: Maybe (AUID User)
    , _dbElaborationDuration :: DurationDays
    , _dbVoteDuration        :: DurationDays
    , _dbSchoolQuorum        :: Int
    , _dbClassQuorum         :: Int
    , _dbLastId              :: Integer
    }
  deriving (Eq, Show, Read)

makeLenses ''AulaData

type AulaLens a = Lens' AulaData a
type AulaGetter a = Getter AulaData a

dbSpaces :: AulaGetter [IdeaSpace]
dbSpaces = dbSpaceSet . to Set.elems

dbIdeas :: AulaGetter [Idea]
dbIdeas = dbIdeaMap . to Map.elems

dbUsers :: AulaGetter [User]
dbUsers = dbUserMap . to Map.elems

dbTopics :: AulaGetter [Topic]
dbTopics = dbTopicMap . to Map.elems

emptyAulaData :: AulaData
emptyAulaData = AulaData nil nil nil nil Nothing 21 21 30 3 0

newtype Persist a = Persist (ReaderT (TVar AulaData) IO a)
  deriving (Functor, Applicative, Monad)

persistIO :: IO a -> Persist a
persistIO = Persist . liftIO

instance GenArbitrary Persist where
    genArbitrary = persistIO $ generate arbitrary

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

getSpaces :: Persist [IdeaSpace]
getSpaces = getDb dbSpaces

getIdeas :: Persist [Idea]
getIdeas = getDb dbIdeas

-- | If idea space already exists, do nothing.  Otherwise, create it.
addIdeaSpaceIfNotExists :: IdeaSpace -> Persist ()
addIdeaSpaceIfNotExists ispace = do
    exists <- (ispace `elem`) <$> getSpaces
    unless exists $ modifyDb dbSpaceSet (Set.insert ispace)

addIdea :: Proto Idea -> Persist Idea
addIdea = addDb dbIdeaMap

findIdea :: AUID Idea -> Persist (Maybe Idea)
findIdea = findInById dbIdeas

findIdeasByUserId :: AUID User -> Persist [Idea]
findIdeasByUserId user = findAllIn dbIdeas (\i -> i ^. createdBy == user)

modifyAMap :: AulaLens (AMap a) -> AUID a -> (a -> a) -> Persist ()
modifyAMap l ident f = modifyDb l (at ident . _Just %~ f)

modifyIdea :: AUID Idea -> (Idea -> Idea) -> Persist ()
modifyIdea = modifyAMap dbIdeaMap

modifyUser :: AUID User -> (User -> User) -> Persist ()
modifyUser = modifyAMap dbUserMap

modifyTopic :: AUID Topic -> (Topic -> Topic) -> Persist ()
modifyTopic = modifyAMap dbTopicMap

findUser :: AUID User -> Persist (Maybe User)
findUser = findInById dbUsers

getUsers :: Persist [User]
getUsers = getDb dbUsers

getTopics :: Persist [Topic]
getTopics = getDb dbTopics

moveIdeasToTopic :: [AUID Idea] -> Maybe (AUID Topic) -> Persist ()
moveIdeasToTopic ideaIds topicId =
    for_ ideaIds $ \ideaId ->
        modifyIdea ideaId $ ideaTopic .~ topicId

addTopic :: Proto Topic -> Persist Topic
addTopic pt = do
    t <- addDb dbTopicMap pt
    -- FIXME a new topic should not be able to steal ideas from other topics of course the UI will
    -- hide this risk since only ideas without topics will be visible.
    -- Options:
    -- - Make it do nothing
    -- - Make it fail hard
    moveIdeasToTopic (pt ^. protoTopicIdeas) (Just $ t ^. _Id)
    return t

findUserByLogin :: UserLogin -> Persist (Maybe User)
findUserByLogin = findInBy dbUsers userLogin

findTopic :: AUID Topic -> Persist (Maybe Topic)
findTopic = findInById dbTopics

findTopicsBySpace :: IdeaSpace -> Persist [Topic]
findTopicsBySpace = findAllInBy dbTopics topicIdeaSpace

findIdeasByTopicId :: AUID Topic -> Persist [Idea]
findIdeasByTopicId = findAllInBy dbIdeas ideaTopic . Just

findIdeasByTopic :: Topic -> Persist [Idea]
findIdeasByTopic = findIdeasByTopicId . view _Id

findWildIdeasBySpace :: IdeaSpace -> Persist [Idea]
findWildIdeasBySpace space = findAllIn dbIdeas (\idea -> idea ^. ideaSpace == space && isNothing (idea ^. ideaTopic))

-- | FIXME: anyone can login
-- | FIXME: every login changes all other logins
loginUser :: UserLogin -> Persist ()
loginUser login = modifyDb dbCurrentUser . const . fmap (view _Id) =<< findUserByLogin login

logoutUser :: UserLogin -> Persist ()
logoutUser _userLogin = modifyDb dbCurrentUser $ const Nothing

-------------------------------------------------------------------

nextId :: Persist (AUID a)
nextId = do
    modifyDb dbLastId (+1)
    AUID <$> getDb dbLastId

currentUser :: Persist (AUID User)
currentUser = (\(Just u) -> u) <$> getDb dbCurrentUser

addUser :: Proto User -> Persist User
addUser proto = do
    metainfo  <- nextMetaInfo
    uLogin    <- maybe (mkUserLogin proto) pure (proto ^. protoUserLogin)
    uPassword <- maybe mkRandomPassword pure (proto ^. protoUserPassword)
    let user = User
          { _userMeta      = metainfo
          , _userLogin     = uLogin
          , _userFirstName = proto ^. protoUserFirstName
          , _userLastName  = proto ^. protoUserLastName
          , _userAvatar    = "http://no.avatar/"
          , _userGroups    = proto ^. protoUserGroups
          , _userPassword  = uPassword
          , _userEmail     = proto ^. protoUserEmail
          }
    modifyDb dbUserMap $ at (user ^. _Id) .~ Just user
    return user

-- | When adding the first user, there is no creator yet, so the first user creates itself.  Login
-- name and password must be 'Just' in the proto user.
addFirstUser :: Proto User -> Persist User
addFirstUser proto = do
    now <- Timestamp <$> persistIO getCurrentTime
    let uid = AUID 0
        oid = AUID 0
        uLogin    = fromMaybe (error "addFirstUser: no login name") (proto ^. protoUserLogin)
        uPassword = fromMaybe (error "addFirstUser: no passphrase") (proto ^. protoUserPassword)
        metainfo = MetaInfo
            { _metaId              = oid
            , _metaCreatedBy       = uid
            , _metaCreatedByLogin  = uLogin
            , _metaCreatedByAvatar = "http://no.avatar/"
            , _metaCreatedAt       = now
            , _metaChangedBy       = uid
            , _metaChangedAt       = now
            }
        user = User
          { _userMeta      = metainfo
          , _userLogin     = uLogin
          , _userFirstName = proto ^. protoUserFirstName
          , _userLastName  = proto ^. protoUserLastName
          , _userAvatar    = "http://no.avatar/"
          , _userGroups    = proto ^. protoUserGroups
          , _userPassword  = uPassword
          , _userEmail     = proto ^. protoUserEmail
          }

    modifyDb dbUserMap $ at (user ^. _Id) .~ Just user
    return user

mkUserLogin :: ProtoUser -> Persist UserLogin
mkUserLogin protoUser = pick (gen firstn lastn)
  where
    firstn :: ST = protoUser ^. protoUserFirstName . fromUserFirstName
    lastn  :: ST = protoUser ^. protoUserLastName  . fromUserLastName

    pick :: [ST] -> Persist UserLogin
    pick ((UserLogin -> l):ls) = maybe (pure l) (\_ -> pick ls) =<< findUserByLogin l
    pick []                    = error "impossible.  (well, unlikely.)"

    gen :: ST -> ST -> [ST]
    gen (ST.take 3 -> fn) (ST.take 3 -> ln) = mutate (fn <> ln) <$> noise

    mutate :: ST -> ST -> ST
    mutate sig noi = ST.take (6 - ST.length noi) sig <> noi

    noise :: [ST]
    noise = nub $ cs . mconcat <$> replicateM 5 ("" : ((:[]) <$> ['a'..'z']))

mkRandomPassword :: Persist UserPass
mkRandomPassword = persistIO $ UserPassInitial . cs . unwords <$> mkPassword `mapM` [4,3,5]

adminUsernameHack :: UserLogin
adminUsernameHack = UserLogin "admin"

instance FromProto Idea where
    fromProto i m = Idea
        { _ideaMeta     = m
        , _ideaTitle    = i ^. protoIdeaTitle
        , _ideaDesc     = i ^. protoIdeaDesc
        , _ideaCategory = i ^. protoIdeaCategory
        , _ideaSpace    = SchoolSpace
        , _ideaTopic    = Nothing
        , _ideaComments = nil
        , _ideaLikes    = nil
        , _ideaQuorumOk = False
        , _ideaVotes    = nil
        , _ideaResult   = Nothing
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
newMetaInfo uid oid = do
    now <- Timestamp <$> persistIO getCurrentTime
    -- Just user <- findUser uid  -- FIXME: need exceptions; need to make test suite smarter
    return MetaInfo
        { _metaId              = oid
        , _metaCreatedBy       = uid
        , _metaCreatedByLogin  = nil  -- FIXME: user ^. userLogin
        , _metaCreatedByAvatar = nil  -- FIXME: user ^. userAvatar
        , _metaCreatedAt       = now
        , _metaChangedBy       = uid
        , _metaChangedAt       = now
        }

nextMetaInfo :: Persist (MetaInfo a)
nextMetaInfo = join $ newMetaInfo <$> currentUser <*> nextId
