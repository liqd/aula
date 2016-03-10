{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Persistent.Api
    ( PersistM
    , AMap
    , AulaLens
    , AulaGetter
    , AulaSetter

    , AulaData
    , emptyAulaData

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

import Control.Lens
import Control.Monad (join, unless, replicateM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Elocrypt (mkPassword)
import Data.Foldable (find, for_)
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isNothing)
import Data.String.Conversions (ST, cs, (<>))
import Data.Set (Set)
import Data.Time.Clock (getCurrentTime)

import Types

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as ST

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
type AulaSetter a = Setter' AulaData a

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

-- FIXME move enough specialized calls to IO in PersistM to remove MonadIO
class MonadIO m => PersistM m where
    getDb :: AulaGetter a -> m a
    modifyDb :: AulaSetter a -> (a -> a) -> m ()

addDb :: (HasMetaInfo a, FromProto a) => AulaSetter (AMap a) -> Proto a -> PersistM m => m a
addDb l pa = do
    a  <- fromProto pa <$> nextMetaInfo
    modifyDb l $ at (a ^. _Id) .~ Just a
    return a

findIn :: AulaGetter [a] -> (a -> Bool) -> PersistM m => m (Maybe a)
findIn l p = find p <$> getDb l

findAllIn :: AulaGetter [a] -> (a -> Bool) -> PersistM m => m [a]
findAllIn l p = filter p <$> getDb l

findInBy :: Eq b => AulaGetter [a] -> Fold a b -> b -> PersistM m => m (Maybe a)
findInBy l f b = findIn l (\x -> x ^? f == Just b)

findAllInBy :: Eq b => AulaGetter [a] -> Fold a b -> b -> PersistM m => m [a]
findAllInBy l f b = findAllIn l (\x -> x ^? f == Just b)

findInById :: HasMetaInfo a => AulaGetter [a] -> AUID a -> PersistM m => m (Maybe a)
findInById l = findInBy l _Id

getSpaces :: PersistM m => m [IdeaSpace]
getSpaces = getDb dbSpaces

getIdeas :: PersistM m => m [Idea]
getIdeas = getDb dbIdeas

-- | If idea space already exists, do nothing.  Otherwise, create it.
addIdeaSpaceIfNotExists :: IdeaSpace -> PersistM m => m ()
addIdeaSpaceIfNotExists ispace = do
    exists <- (ispace `elem`) <$> getSpaces
    unless exists $ modifyDb dbSpaceSet (Set.insert ispace)

addIdea :: Proto Idea -> PersistM m => m Idea
addIdea = addDb dbIdeaMap

findIdea :: AUID Idea -> PersistM m => m (Maybe Idea)
findIdea = findInById dbIdeas

findIdeasByUserId :: AUID User -> PersistM m => m [Idea]
findIdeasByUserId user = findAllIn dbIdeas (\i -> i ^. createdBy == user)

modifyAMap :: AulaLens (AMap a) -> AUID a -> (a -> a) -> PersistM m => m ()
modifyAMap l ident f = modifyDb l (at ident . _Just %~ f)
modifyIdea :: AUID Idea -> (Idea -> Idea) -> PersistM m => m ()
modifyIdea = modifyAMap dbIdeaMap

modifyUser :: AUID User -> (User -> User) -> PersistM m => m ()
modifyUser = modifyAMap dbUserMap

modifyTopic :: AUID Topic -> (Topic -> Topic) -> PersistM m => m ()
modifyTopic = modifyAMap dbTopicMap

findUser :: AUID User -> PersistM m => m (Maybe User)
findUser = findInById dbUsers

getUsers :: PersistM m => m [User]
getUsers = getDb dbUsers

--addUser :: Proto User -> PersistM m => m User
--addUser = addDb dbUserMap

getTopics :: PersistM m => m [Topic]
getTopics = getDb dbTopics

moveIdeasToTopic :: [AUID Idea] -> Maybe (AUID Topic) -> PersistM m => m ()
moveIdeasToTopic ideaIds topicId =
    for_ ideaIds $ \ideaId ->
        modifyIdea ideaId $ ideaTopic .~ topicId

addTopic :: Proto Topic -> PersistM m => m Topic
addTopic pt = do
    t <- addDb dbTopicMap pt
    -- FIXME a new topic should not be able to steal ideas from other topics of course the UI will
    -- hide this risk since only ideas without topics will be visible.
    -- Options:
    -- - Make it do nothing
    -- - Make it fail hard
    moveIdeasToTopic (pt ^. protoTopicIdeas) (Just $ t ^. _Id)
    return t

findUserByLogin :: UserLogin -> PersistM m => m (Maybe User)
findUserByLogin = findInBy dbUsers userLogin

findTopic :: AUID Topic -> PersistM m => m (Maybe Topic)
findTopic = findInById dbTopics

findTopicsBySpace :: IdeaSpace -> PersistM m => m [Topic]
findTopicsBySpace = findAllInBy dbTopics topicIdeaSpace

findIdeasByTopicId :: AUID Topic -> PersistM m => m [Idea]
findIdeasByTopicId = findAllInBy dbIdeas ideaTopic . Just

findIdeasByTopic :: Topic -> PersistM m => m [Idea]
findIdeasByTopic = findIdeasByTopicId . view _Id

findWildIdeasBySpace :: IdeaSpace -> PersistM m => m [Idea]
findWildIdeasBySpace space = findAllIn dbIdeas (\idea -> idea ^. ideaSpace == space && isNothing (idea ^. ideaTopic))

-- | FIXME: anyone can login
-- | FIXME: every login changes all other logins
loginUser :: UserLogin -> PersistM m => m ()
loginUser login = modifyDb dbCurrentUser . const . fmap (view _Id) =<< findUserByLogin login

logoutUser :: UserLogin -> PersistM m => m ()
logoutUser _userLogin = modifyDb dbCurrentUser $ const Nothing

-------------------------------------------------------------------

nextId :: PersistM m => m (AUID a)
nextId = do
    modifyDb dbLastId (+1)
    AUID <$> getDb dbLastId

currentUser :: PersistM m => m (AUID User)
currentUser = (\(Just u) -> u) <$> getDb dbCurrentUser

addUser :: Proto User -> PersistM m => m User
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
addFirstUser :: Proto User -> PersistM m => m User
addFirstUser proto = do
    now <- Timestamp <$> liftIO getCurrentTime
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

mkUserLogin :: ProtoUser -> PersistM m => m UserLogin
mkUserLogin protoUser = pick (gen firstn lastn)
  where
    firstn :: ST = protoUser ^. protoUserFirstName . fromUserFirstName
    lastn  :: ST = protoUser ^. protoUserLastName  . fromUserLastName

    pick :: [ST] -> PersistM m => m UserLogin
    pick ((UserLogin -> l):ls) = maybe (pure l) (\_ -> pick ls) =<< findUserByLogin l
    pick []                    = error "impossible.  (well, unlikely.)"

    gen :: ST -> ST -> [ST]
    gen (ST.take 3 -> fn) (ST.take 3 -> ln) = mutate (fn <> ln) <$> noise

    mutate :: ST -> ST -> ST
    mutate sig noi = ST.take (6 - ST.length noi) sig <> noi

    noise :: [ST]
    noise = nub $ cs . mconcat <$> replicateM 5 ("" : ((:[]) <$> ['a'..'z']))

mkRandomPassword :: PersistM m => m UserPass
mkRandomPassword = liftIO $ UserPassInitial . cs . unwords <$> mkPassword `mapM` [4,3,5]

adminUsernameHack :: UserLogin
adminUsernameHack = UserLogin "admin"

instance FromProto Idea where
    fromProto i m = Idea
        { _ideaMeta     = m
        , _ideaTitle    = i ^. protoIdeaTitle
        , _ideaDesc     = i ^. protoIdeaDesc
        , _ideaCategory = i ^. protoIdeaCategory
        , _ideaSpace    = i ^. protoIdeaIdeaSpace
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
newMetaInfo :: PersistM m => AUID User -> AUID a -> m (MetaInfo a)
newMetaInfo uid oid = do
    now <- Timestamp <$> liftIO getCurrentTime
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

nextMetaInfo :: PersistM m => m (MetaInfo a)
nextMetaInfo = join $ newMetaInfo <$> currentUser <*> nextId
