{-# LANGUAGE DefaultSignatures           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TupleSections               #-}
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
    , getNumVotersForIdea
    , addIdeaSpaceIfNotExists
    , addIdea
    , modifyIdea
    , findIdea
    , findIdeasByTopicId
    , findIdeasByTopic
    , findIdeasByUserId
    , findWildIdeasBySpace
    , addLikeToIdea
    , findUser
    , getUsers
    , addUser
    , addFirstUser
    , mkMetaInfo
    , mkUserLogin
    , mkRandomPassword
    , modifyUser
    , getTopics
    , addTopic
    , modifyTopic
    , moveIdeasToLocation
    , findTopic
    , findTopicsBySpace
    , findUserByLogin
    , dbIdeas
    , dbUsers
    , dbTopics
    , dbSpaceSet
    , dbIdeaMap
    , dbUserMap
    , dbTopicMap
    , dbElaborationDuration
    , dbVoteDuration
    , dbSchoolQuorum
    , dbClassQuorum
    , adminUsernameHack
    , addDelegation
    , findDelegationsByContext
    , ideaPhase
    )
where

import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (unless, replicateM)
import Data.Elocrypt (mkPassword)
import Data.Foldable (find, for_)
import Data.Functor.Infix ((<$$>))
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.String.Conversions (ST, cs, (<>))
import Data.Time.Clock (getCurrentTime)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as ST

import Types


type AMap a = Map (AUID a) a

data AulaData = AulaData
    { _dbSpaceSet            :: Set IdeaSpace
    , _dbIdeaMap             :: AMap Idea
    , _dbUserMap             :: AMap User
    , _dbTopicMap            :: AMap Topic
    , _dbDelegationMap       :: AMap Delegation
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
emptyAulaData = AulaData nil nil nil nil nil 21 21 30 3 0

class Monad m => PersistM m where
    getDb :: AulaGetter a -> m a
    modifyDb :: AulaSetter a -> (a -> a) -> m ()

    getCurrentTimestamp :: m Timestamp

    default getCurrentTimestamp :: MonadIO m => m Timestamp
    getCurrentTimestamp = Timestamp <$> liftIO getCurrentTime

    mkRandomPassword :: m UserPass

    default mkRandomPassword :: MonadIO m => m UserPass
    mkRandomPassword = liftIO $ UserPassInitial . cs . unwords <$> mkPassword `mapM` [4,3,5]

addDb :: (HasMetaInfo a, FromProto a)
      => AulaSetter (AMap a) -> UserWithProto a -> PersistM m => m a
addDb l (cUser, pa) = do
    a <- fromProto pa <$> nextMetaInfo cUser
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

-- | Users can like an idea / vote on it iff they are students with access to the idea's space.
getNumVotersForIdea :: PersistM m => Idea -> m (Idea, Int)
getNumVotersForIdea idea = (idea,) . length . filter hasAccess <$> getUsers
  where
    hasAccess u = case idea ^. ideaLocation . ideaLocationSpace of
        SchoolSpace   -> isStudent u
        ClassSpace cl -> u `isStudentInClass` cl

    isStudent (view userRole -> (Student _)) = True
    isStudent _                              = False

    isStudentInClass (view userRole -> (Student cl')) cl = cl' == cl
    isStudentInClass _ _ = False

-- | If idea space already exists, do nothing.  Otherwise, create it.
addIdeaSpaceIfNotExists :: IdeaSpace -> PersistM m => m ()
addIdeaSpaceIfNotExists ispace = do
    exists <- (ispace `elem`) <$> getSpaces
    unless exists $ modifyDb dbSpaceSet (Set.insert ispace)

addIdea :: UserWithProto Idea -> PersistM m => m Idea
addIdea = addDb dbIdeaMap

findIdea :: AUID Idea -> PersistM m => m (Maybe Idea)
findIdea = findInById dbIdeas

findIdeasByUserId :: AUID User -> PersistM m => m [Idea]
findIdeasByUserId uId = findAllIn dbIdeas (\i -> i ^. createdBy == uId)

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

getTopics :: PersistM m => m [Topic]
getTopics = getDb dbTopics

moveIdeasToLocation :: [AUID Idea] -> IdeaLocation -> PersistM m => m ()
moveIdeasToLocation ideaIds location =
    for_ ideaIds $ \ideaId ->
        modifyIdea ideaId $ ideaLocation .~ location

addTopic :: UserWithProto Topic -> PersistM m => m Topic
addTopic pt = do
    t <- addDb dbTopicMap pt
    -- FIXME a new topic should not be able to steal ideas from other topics of course the UI will
    -- hide this risk since only ideas without topics will be visible.
    -- Options:
    -- - Make it do nothing
    -- - Make it fail hard
    Just loc <- ideaLocationFromTopic (t ^. _Id)
    moveIdeasToLocation (pt ^. _2 . protoTopicIdeas) loc
    return t

addDelegation :: UserWithProto Delegation -> PersistM m => m Delegation
addDelegation = addDb dbDelegationMap

findDelegationsByContext :: DelegationContext -> PersistM m => m [Delegation]
findDelegationsByContext ctx = filter ((== ctx) . view delegationContext) . Map.elems
    <$> getDb dbDelegationMap

findUserByLogin :: UserLogin -> PersistM m => m (Maybe User)
findUserByLogin = findInBy dbUsers userLogin

findTopic :: AUID Topic -> PersistM m => m (Maybe Topic)
findTopic = findInById dbTopics

findTopicsBySpace :: IdeaSpace -> PersistM m => m [Topic]
findTopicsBySpace = findAllInBy dbTopics topicIdeaSpace

findIdeasByTopicId :: AUID Topic -> PersistM m => m [Idea]
findIdeasByTopicId tid = do
    Just loc <- ideaLocationFromTopic tid
    findAllInBy dbIdeas ideaLocation loc

findIdeasByTopic :: Topic -> PersistM m => m [Idea]
findIdeasByTopic = findIdeasByTopicId . view _Id

findWildIdeasBySpace :: IdeaSpace -> PersistM m => m [Idea]
findWildIdeasBySpace space = findAllIn dbIdeas ((== IdeaLocationSpace space) . view ideaLocation)

-- FIXME: Same user can like the same idea more than once.
addLikeToIdea :: User -> AUID Idea -> PersistM m => m ()
addLikeToIdea cUser iid = do
    metainfo <- nextMetaInfo cUser
    modifyIdea iid (ideaLikes %~ Set.insert (IdeaLike metainfo))

nextId :: PersistM m => m (AUID a)
nextId = do
    modifyDb dbLastId (+1)
    AUID <$> getDb dbLastId

-- No 'FromProto' instance, since this is more complex, due to the possible
-- auto-generating of logins and passwords.
userFromProto :: MetaInfo User -> UserLogin -> UserPass -> Proto User -> User
userFromProto metainfo uLogin uPassword proto = User
    { _userMeta      = metainfo
    , _userLogin     = uLogin
    , _userFirstName = proto ^. protoUserFirstName
    , _userLastName  = proto ^. protoUserLastName
    , _userAvatar    = Nothing
    , _userRole      = proto ^. protoUserRole
    , _userPassword  = uPassword
    , _userEmail     = proto ^. protoUserEmail
    }

addUser :: UserWithProto User -> PersistM m => m User
addUser (cUser, proto) = do
    metainfo  <- nextMetaInfo cUser
    uLogin    <- maybe (mkUserLogin proto) pure (proto ^. protoUserLogin)
    uPassword <- maybe mkRandomPassword pure (proto ^. protoUserPassword)
    let user = userFromProto metainfo uLogin uPassword proto
    modifyDb dbUserMap $ at (user ^. _Id) .~ Just user
    return user

-- | When adding the first user, there is no creator yet, so the first user creates itself.  Login
-- name and password must be 'Just' in the proto user.
addFirstUser :: Proto User -> PersistM m => m User
addFirstUser proto = do
    now <- getCurrentTimestamp
    uid <- nextId
    let uLogin    = fromMaybe (error "addFirstUser: no login name") (proto ^. protoUserLogin)
        uPassword = fromMaybe (error "addFirstUser: no passphrase") (proto ^. protoUserPassword)
        -- the user creates herself
        cUser = _Id .~ uid $ user
        metainfo = mkMetaInfo cUser now uid
        user = userFromProto metainfo uLogin uPassword proto

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

adminUsernameHack :: UserLogin
adminUsernameHack = UserLogin "admin"

instance FromProto Idea where
    fromProto i m = Idea
        { _ideaMeta     = m
        , _ideaTitle    = i ^. protoIdeaTitle
        , _ideaDesc     = i ^. protoIdeaDesc
        , _ideaCategory = i ^. protoIdeaCategory
        , _ideaLocation = i ^. protoIdeaLocation
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

instance FromProto Delegation where
    fromProto (ProtoDelegation ctx f t) m = Delegation m ctx f t

mkMetaInfo :: User -> Timestamp -> AUID a -> MetaInfo a
mkMetaInfo cUser now oid = MetaInfo
    { _metaId              = oid
    , _metaCreatedBy       = cUser ^. _Id
    , _metaCreatedByLogin  = cUser ^. userLogin
    , _metaCreatedByAvatar = cUser ^. userAvatar
    , _metaCreatedAt       = now
    , _metaChangedBy       = cUser ^. _Id
    , _metaChangedAt       = now
    }

nextMetaInfo :: PersistM m => User -> m (MetaInfo a)
nextMetaInfo cUser = mkMetaInfo cUser <$> getCurrentTimestamp <*> nextId

-- | Construct an 'IdeaLocation' from a 'Topic' id.
ideaLocationFromTopic :: AUID Topic -> PersistM m => m (Maybe IdeaLocation)
ideaLocationFromTopic tid = (`IdeaLocationTopic` tid) . view topicIdeaSpace <$$> findTopic tid

ideaPhase :: Idea -> PersistM m => m (Maybe Phase)
ideaPhase idea = case idea ^. ideaLocation of
    IdeaLocationSpace _ ->
        pure Nothing
    IdeaLocationTopic _ topicId -> do
        -- (failure to match the following can only be caused by an inconsistent state)
        Just topic <- findTopic topicId
        pure . Just $ topic ^. topicPhase
