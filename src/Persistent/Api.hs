{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE GADTs                       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TupleSections               #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -Wall -Wwarn -fno-warn-orphans #-}

module Persistent.Api
    ( PersistM
    , AMap
    , AulaLens
    , AulaGetter
    , AulaSetter
    , DbLens(..)
    , DbTraversal(..)
    , dbLens
    , dbTraversal
    , PersistExcept(PersistExcept, unPersistExcept)
    , RunPersistNat
    , RunPersistT(..)
    , RunPersist
    , withPersist'

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
    , getWildIdeas
    , getIdeasWithTopic
    , addIdeaSpaceIfNotExists
    , addIdea
    , modifyIdea
    , findIdea
    , findIdeasByTopicId
    , findIdeasByTopic
    , findIdeasByUserId
    , findWildIdeasBySpace
    , addLikeToIdea
    , addVoteToIdea
    , addCommentToIdea
    , addReplyToIdeaComment
    , addCommentVoteToIdeaComment
    , addCommentVoteToIdeaCommentReply
    , findUser
    , getUsers
    , addUser
    , addFirstUser
    , mkMetaInfo
    , mkUserLogin
    , getCurrentTimestamp
    , getCurrentTimestampIO
    , mkRandomPassword
    , mkRandomPasswordIO
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
    , dbDelegationMap
    , dbElaborationDuration
    , dbVoteDuration
    , dbSchoolQuorum
    , dbClassQuorum
    , dbLastId
    , adminUsernameHack
    , addDelegation
    , findDelegationsByContext
    , addIdeaResult
    )
where

import Control.Exception (finally)
import Control.Lens
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT)
import Control.Monad (unless, replicateM, when)
import Data.Elocrypt (mkPassword)
import Data.Foldable (find, for_)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set (Set)
import Data.String.Conversions (ST, cs, (<>))
import Data.Time.Clock (getCurrentTime)
import Data.Typeable (Typeable)
import Servant (ServantErr)
import Servant.Missing (ThrowError500(..))
import Servant.Server ((:~>))

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as ST

import Types


data AulaData = AulaData
    { _dbSpaceSet            :: Set IdeaSpace
    , _dbIdeaMap             :: Ideas
    , _dbUserMap             :: Users
    , _dbTopicMap            :: Topics
    , _dbDelegationMap       :: Delegations
    , _dbElaborationDuration :: DurationDays  -- FIXME: elaboration and refinement are the same thing.  pick one term!
    , _dbVoteDuration        :: DurationDays
    , _dbSchoolQuorum        :: Percent
    , _dbClassQuorum         :: Percent  -- (there is only one quorum for all classes, see gh#318)
    , _dbLastId              :: Integer
    }
  deriving (Eq, Show, Read, Typeable)

makeLenses ''AulaData

deriveSafeCopy 0 'base ''AulaData


type AulaLens a = Lens' AulaData a
type AulaGetter a = Getter AulaData a
type AulaSetter a = Setter' AulaData a
type AulaTraversal a = Traversal' AulaData a

data DbLens a where
    DbId    :: DbLens AulaData
    DbAt    :: DbLens (AMap a) -> AUID a -> DbLens (Maybe a)

    DbSpaceSet              :: DbLens (Set IdeaSpace)
    DbIdeas                 :: DbLens Ideas
    DbUsers                 :: DbLens Users
    DbTopics                :: DbLens Topics
    DbDelegations           :: DbLens Delegations
    DbElaborationDuration   :: DbLens DurationDays
    DbVoteDuration          :: DbLens DurationDays
    DbSchoolQuorum          :: DbLens Percent
    DbClassQuorum           :: DbLens Percent
    DbLastId                :: DbLens Integer

{-
    -- Idea specific
    DbIdeaLikes     :: DbLens Idea -> DbLens IdeaLikes
    DbIdeaVotes     :: DbLens Idea -> DbLens IdeaVotes
    DbIdeaComments  :: DbLens Idea -> DbLens Comments
    DbIdeaResult    :: DbLens Idea -> DbLens (Maybe IdeaResult)

    -- Comment specific
    DbCommentReplies    :: DbLens Comment -> DbLens Comments
    DbCommentVotes      :: DbLens Comment -> DbLens CommentVotes
-}

deriving instance Show (DbLens a)

infixr 9 :.:

data DbTraversal a where
    (:.:) :: DbLens a -> Traversal' a b -> DbTraversal b

dbLens :: DbLens a -> AulaLens a
dbLens = \case
    DbId                    -> id
    DbAt l i                -> dbLens l . at i

    DbSpaceSet              -> dbSpaceSet
    DbIdeas                 -> dbIdeaMap
    DbUsers                 -> dbUserMap
    DbTopics                -> dbTopicMap
    DbDelegations           -> dbDelegationMap
    DbElaborationDuration   -> dbElaborationDuration
    DbVoteDuration          -> dbVoteDuration
    DbSchoolQuorum          -> dbSchoolQuorum
    DbClassQuorum           -> dbClassQuorum
    DbLastId                -> dbLastId

{-
    DbIdeaLikes l           -> dbLens l . ideaLikes
    DbIdeaVotes l           -> dbLens l . ideaVotes
    DbIdeaComments l        -> dbLens l . ideaComments
    DbIdeaResult l          -> dbLens l . ideaResult

    DbCommentReplies l      -> dbLens l . commentReplies
    DbCommentVotes l        -> dbLens l . commentVotes
-}

dbTraversal :: DbTraversal a -> AulaTraversal a
dbTraversal (l :.: t) = dbLens l . t

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

-- | FIXME: this will have constructors dedicated for specific errors, and 'ServantErr' will only be
-- introduced later.
newtype PersistExcept = PersistExcept { unPersistExcept :: ServantErr }
    deriving (Eq, Show)

makePrisms ''PersistExcept

instance ThrowError500 PersistExcept where
    error500 = _PersistExcept . error500

class (MonadError PersistExcept m, Monad m) => PersistM m where
    getDb :: AulaGetter a -> m a
    modifyDb :: DbTraversal a -> (a -> a) -> m ()
    getCurrentTimestamp :: m Timestamp
    mkRandomPassword :: m UserPass


getCurrentTimestampIO :: IO Timestamp
getCurrentTimestampIO = Timestamp <$> getCurrentTime

mkRandomPasswordIO :: IO UserPass
mkRandomPasswordIO = UserPassInitial . cs . unwords <$> mkPassword `mapM` [4,3,5]

type RunPersistNat m r = r :~> ExceptT PersistExcept m

data RunPersistT m =
    forall r. (PersistM r, GenArbitrary r) =>
        RunPersist
                  { _rpDesc  :: String
                  , _rpNat   :: RunPersistNat m r
                  , _rpClose :: m ()
                  }

type RunPersist = RunPersistT IO

-- | A more low-level variant of 'Persistent.Implementation.withPersist' with the implementation
-- explicit as parameter.
withPersist' :: IO RunPersist -> (forall r. (PersistM r, GenArbitrary r) => RunPersistNat IO r -> IO a) -> IO a
withPersist' mkRunP m = do
    RunPersist desc rp close <- mkRunP -- initialization happens here
    putStrLn $ "persistence: " <> desc -- FIXME: use logger for this
    m rp `finally` close               -- closing happens here


-- | The argument is a consistency check that will throw an error if it fails.
--
-- This can be equipped with a switch for performance, but if at all possible it would be nice to
-- run the checks even in production.
assertPersistM :: PersistM m => m () -> m ()
assertPersistM check = check


type AddDb m a = UserWithProto a -> PersistM m => m a

-- | @addDb l (u, p)@ adds a record to the DB.
-- The record is added on the behalf of the user @u@.
-- The record is computed from the prototype @p@, the current time and the given user @u@.
-- The record is added at the location pointed by the traversal @l@.
--
-- It is expected that @l@ points to exactly one target (checked by 'assertPersistM').
--
-- We could make the type of @l@ be @AulaLens (AMap a)@ which would enforce the constraint
-- above at the expense of pushing the burden towards cases where the traversal is only a
-- lens when some additional assumptions are met (see addReplyToIdeaComment for instance).
--
-- It could make sense for the traversal to point to more than one target for instance
-- to index the record at different locations. For instance we could keep an additional
-- global map of the comments, votes, likes and still call @addDb@ only once.
addDb :: (HasMetaInfo a, FromProto a) => DbLens (AMap a) -> AddDb m a
addDb l (cUser, pa) = do
    a <- fromProto pa <$> nextMetaInfo cUser
    modifyDb (DbAt l (a ^. _Id) :.: id) (const $ Just a)
    return a

addDbDeep :: (HasMetaInfo b, FromProto b) => DbLens (Maybe a) -> Traversal' a (AMap b) -> AddDb m b
addDbDeep l t (cUser, pa) = do
    assertPersistM $ do
        v <- getDb (dbLens l)
        let len = lengthOf (_Just . t) v
        when (len /= 1) $ do
            fail $ "Persistent.Api.addDbDeep expects the location (lens, traversal) "
                <> "to target exactly 1 field not " <> show len
    a <- fromProto pa <$> nextMetaInfo cUser
    modifyDb (l :.: _Just . t . at (a ^. _Id)) (const $ Just a)
    return a

addInIdea :: (HasMetaInfo a, FromProto a) => AUID Idea -> Traversal' Idea (AMap a) -> AddDb m a
addInIdea = addDbDeep . DbAt DbIdeas

addDbValue :: (HasMetaInfo a, FromProto a) => DbTraversal a -> AddDb m a
addDbValue l (cUser, pa) = do
    a <- fromProto pa <$> nextMetaInfo cUser
    modifyDb l (const a)
    return a

findIn :: AulaGetter [a] -> (a -> Bool) -> PersistM m => m (Maybe a)
findIn l p = find p <$> getDb l

findAllIn :: AulaGetter [a] -> (a -> Bool) -> PersistM m => m [a]
findAllIn l p = filter p <$> getDb l

findInBy :: Eq b => AulaGetter [a] -> Fold a b -> b -> PersistM m => m (Maybe a)
findInBy l f b = findIn l (\x -> x ^? f == Just b)

findAllInBy :: Eq b => AulaGetter [a] -> Fold a b -> b -> PersistM m => m [a]
findAllInBy l f b = findAllIn l (\x -> x ^? f == Just b)

findInById :: HasMetaInfo a => AulaGetter (AMap a) -> AUID a -> PersistM m => m (Maybe a)
findInById l i = getDb (l . at i)

getSpaces :: PersistM m => m [IdeaSpace]
getSpaces = getDb dbSpaces

getIdeas :: PersistM m => m [Idea]
getIdeas = getDb dbIdeas

getWildIdeas :: PersistM m => m [Idea]
getWildIdeas = filter (isWild . view ideaLocation) <$> getIdeas

getIdeasWithTopic :: PersistM m => m [Idea]
getIdeasWithTopic = filter (not . isWild . view ideaLocation) <$> getIdeas

-- | If idea space already exists, do nothing.  Otherwise, create it.
addIdeaSpaceIfNotExists :: IdeaSpace -> PersistM m => m ()
addIdeaSpaceIfNotExists ispace = do
    exists <- (ispace `elem`) <$> getSpaces
    unless exists $ modifyDb (DbSpaceSet :.: id) (Set.insert ispace)

addIdea :: AddDb m Idea
addIdea = addDb DbIdeas

findIdea :: AUID Idea -> PersistM m => m (Maybe Idea)
findIdea = findInById dbIdeaMap

findIdeasByUserId :: AUID User -> PersistM m => m [Idea]
findIdeasByUserId uId = findAllIn dbIdeas (\i -> i ^. createdBy == uId)

-- | FIXME deal with changedBy and changedAt
modifyAMap :: DbLens (AMap a) -> AUID a -> (a -> a) -> PersistM m => m ()
modifyAMap l i = modifyDb (DbAt l i :.: _Just)

-- | FIXME: consider moving these specializations to a separate section.
modifyIdea :: AUID Idea -> (Idea -> Idea) -> PersistM m => m ()
modifyIdea = modifyAMap DbIdeas

modifyUser :: AUID User -> (User -> User) -> PersistM m => m ()
modifyUser = modifyAMap DbUsers

modifyTopic :: AUID Topic -> (Topic -> Topic) -> PersistM m => m ()
modifyTopic = modifyAMap DbTopics

findUser :: AUID User -> PersistM m => m (Maybe User)
findUser = findInById dbUserMap

getUsers :: PersistM m => m [User]
getUsers = getDb dbUsers

getTopics :: PersistM m => m [Topic]
getTopics = getDb dbTopics

moveIdeasToLocation :: [AUID Idea] -> IdeaLocation -> PersistM m => m ()
moveIdeasToLocation ideaIds location =
    for_ ideaIds $ \ideaId ->
        modifyIdea ideaId $ ideaLocation .~ location

addTopic :: AddDb m Topic
addTopic pt = do
    t <- addDb DbTopics pt
    -- FIXME a new topic should not be able to steal ideas from other topics of course the UI will
    -- hide this risk since only ideas without topics will be visible.
    -- Options:
    -- - Make it do nothing
    -- - Make it fail hard
    moveIdeasToLocation (pt ^. _2 . protoTopicIdeas) (topicIdeaLocation t)
    return t

addDelegation :: AddDb m Delegation
addDelegation = addDb DbDelegations

findDelegationsByContext :: DelegationContext -> PersistM m => m [Delegation]
findDelegationsByContext ctx = filter ((== ctx) . view delegationContext) . Map.elems
    <$> getDb dbDelegationMap

findUserByLogin :: UserLogin -> PersistM m => m (Maybe User)
findUserByLogin = findInBy dbUsers userLogin

findTopic :: AUID Topic -> PersistM m => m (Maybe Topic)
findTopic = findInById dbTopicMap

findTopicsBySpace :: IdeaSpace -> PersistM m => m [Topic]
findTopicsBySpace = findAllInBy dbTopics topicIdeaSpace

findIdeasByTopicId :: AUID Topic -> PersistM m => m [Idea]
findIdeasByTopicId tid = do
    mt <- findTopic tid
    case mt of
        Nothing -> pure []
        Just t  -> findIdeasByTopic t

findIdeasByTopic :: Topic -> PersistM m => m [Idea]
findIdeasByTopic = findAllInBy dbIdeas ideaLocation . topicIdeaLocation

findWildIdeasBySpace :: IdeaSpace -> PersistM m => m [Idea]
findWildIdeasBySpace space = findAllIn dbIdeas ((== IdeaLocationSpace space) . view ideaLocation)

instance FromProto IdeaLike where
    fromProto () = IdeaLike

-- | FIXME: Same user can like the same idea more than once (Issue #308).
-- FIXME: Assumption: the given @AUID Idea@ MUST be in the DB.
addLikeToIdea :: AUID Idea -> AddDb m IdeaLike
addLikeToIdea iid = addInIdea iid ideaLikes

instance FromProto IdeaVote where
    fromProto = flip IdeaVote

-- | FIXME: Same user can vote on the same idea more than once (Issue #308).
-- FIXME: Check also that the given idea exists and is in the right phase.
addVoteToIdea :: AUID Idea -> AddDb m IdeaVote
addVoteToIdea iid = addInIdea iid ideaVotes

instance FromProto Comment where
    fromProto d m = Comment { _commentMeta      = m
                            , _commentText      = d
                            , _commentReplies   = nil
                            , _commentVotes     = nil
                            }


-- | FIXME: Assumption: the given @AUID Idea@ MUST be in the DB.
addCommentToIdea :: AUID Idea -> AddDb m Comment
addCommentToIdea iid = addInIdea iid ideaComments

-- | FIXME: Assumptions:
-- * the given @AUID Idea@ MUST be in the DB.
-- * the given @AUID Comment@ MUST be one of the comment of the given idea.
addReplyToIdeaComment :: AUID Idea -> AUID Comment -> AddDb m Comment
addReplyToIdeaComment iid cid = addInIdea iid (ideaComments . at cid . _Just . commentReplies)

instance FromProto CommentVote where
    fromProto = flip CommentVote

-- | FIXME: Assumptions:
-- * the given @AUID Idea@ MUST be in the DB.
-- * the given @AUID Comment@ MUST be one of the comment of the given idea.
addCommentVoteToIdeaComment :: AUID Idea -> AUID Comment -> AddDb m CommentVote
addCommentVoteToIdeaComment iid cid = addInIdea iid (ideaComments . at cid . _Just . commentVotes)

-- | FIXME: Assumptions:
-- * the given @AUID Idea@ MUST be in the DB.
-- * the first given @AUID Comment@ MUST be one of the comment of the given idea.
-- * the second given @AUID Comment@ MUST be one of the comment of the first given comment.
addCommentVoteToIdeaCommentReply :: AUID Idea -> AUID Comment -> AUID Comment -> AddDb m CommentVote
addCommentVoteToIdeaCommentReply iid cid rid =
    addInIdea iid (ideaComments . at cid . _Just . commentReplies . at rid . _Just . commentVotes)

instance FromProto IdeaResult where
    fromProto = flip IdeaResult

addIdeaResult :: AUID Idea -> AddDb m IdeaResult
addIdeaResult iid = addDbValue (DbAt DbIdeas iid :.: _Just . ideaResult . _Just)

nextId :: PersistM m => m (AUID a)
nextId = do
    modifyDb (DbLastId :.: id) (+1)
    AUID <$> getDb dbLastId

-- | No 'FromProto' instance, since this is more complex, due to the possible
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

addUser :: AddDb m User
addUser (cUser, proto) = do
    metainfo  <- nextMetaInfo cUser
    uLogin    <- maybe (mkUserLogin proto) pure (proto ^. protoUserLogin)
    uPassword <- maybe mkRandomPassword pure (proto ^. protoUserPassword)
    let user = userFromProto metainfo uLogin uPassword proto
    modifyDb (DbAt DbUsers (user ^. _Id) :.: id) (const $ Just user)
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

    modifyDb (DbAt DbUsers (user ^. _Id) :.: id) (const $ Just user)
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
        , _topicPhase     = PhaseRefinement $ t ^. protoTopicRefinDays
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
