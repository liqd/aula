{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE GADTs                       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TupleSections               #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

-- | This module exports acid-state transactions.  It introduces a state type 'AulaData' and 'Query'
-- and 'Update' operations on that state.  (To be more specific, two new types for queries and
-- pudates are defined that specific to 'AulaData' and introduce exceptions.)
--
-- Serializability happens outside of this module.
module Persistent.Pure
    ( AulaData
    , AMap
    , AulaLens
    , AulaGetter
    , AulaSetter
    , emptyAulaData

    , AEvent, AQuery{-(AQuery)-}, AEQuery, AMQuery, AUpdate(AUpdate), AddDb
    , aUpdateEvent
    , WhoWhen(_whoWhenTimestamp, _whoWhenUID), whoWhenTimestamp, whoWhenUID

    , PersistExcept(PersistExcept, unPersistExcept)
    , HasAUpdate

    -- TODO: get some structure into this export list.
    -- FIXME: consider removing Purescript.Idiom and doing everything here.

    , liftAQuery

    , addDb
    , modifyDb
    , modifyDb_
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
    , modifyUser, ModifyUserOp(..)
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
    , addIdeaResult
    )
where

import Control.Lens
import Control.Monad.Except (MonadError, ExceptT(ExceptT))
import Control.Monad.Reader (MonadReader, ReaderT(ReaderT), runReader, ask, asks)
import Control.Monad.State (MonadState, state, gets, modify)
import Control.Monad (unless, replicateM, when)
import Data.Acid.Core
import Data.Acid.Memory.Pure (Event(UpdateEvent))
import Data.Acid  -- (Query, Update, liftQuery)
import Data.Foldable (find, for_)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.String.Conversions (ST, cs, (<>))
import Data.Typeable (Typeable)
import Servant.Missing (ThrowError500(..))
import Servant (ServantErr)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as ST

import Types


-- * state type

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

type AulaLens a = Lens' AulaData a
type AulaGetter a = Getter AulaData a
type AulaSetter a = Setter' AulaData a
type AulaTraversal a = Traversal' AulaData a

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


-- * transactions

data WhoWhen = WhoWhen
    { _whoWhenTimestamp :: Timestamp
    , _whoWhenUID       :: AUID User
    }

makeLenses ''WhoWhen

type AEvent = Event AulaData

-- | 'Query' for 'AulaData'.  Doesn't contain the 'WhoWhen' context,
-- because that would make the stack contain two readers.
type AQuery a = forall m. MonadReader AulaData m => m a

-- | Same as 'AQuery' but can throw 'PersistExcept'.
type AEQuery a = forall m. (MonadError PersistExcept m, MonadReader AulaData m) => m a

type AMQuery a = AQuery (Maybe a)

-- | 'Update' for 'AulaData'.  Can throw 'PersistExcept'.
newtype AUpdate a = AUpdate { _unAUpdate :: ReaderT WhoWhen
                                                (ExceptT PersistExcept
                                                    (Update AulaData)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError PersistExcept
           , MonadState AulaData
           , MonadReader WhoWhen
           )

runAUpdate :: AUpdate r -> Update AulaData r
runAUpdate = error "TODO"

aUpdateEvent :: (UpdateEvent ev, EventState ev ~ AulaData)
             => (ev -> AUpdate (EventResult ev)) -> AEvent
aUpdateEvent f = UpdateEvent $ runAUpdate . f

type HasAUpdate ev a = (UpdateEvent ev, MethodState ev ~ AulaData, MethodResult ev ~ a)

-- | FIXME: lens puzzle!  the function passed to 'state' here runs both 'f' and 'l' twice.  there
-- should be a shortcut, something like '%~', but return in a pair of new state plus new focus.
modifyDb :: AulaLens a -> (a -> a) -> AUpdate a
modifyDb l f = AUpdate . ReaderT . const . ExceptT . fmap Right
             $ state (\s -> (f $ s ^. l, l %~ f $ s))

modifyDb_ :: AulaSetter a -> (a -> a) -> AUpdate ()
modifyDb_ l f = AUpdate . ReaderT . const . ExceptT . fmap Right
              $ modify (l %~ f)

liftAQuery :: AQuery a -> AUpdate a
liftAQuery m = gets (runReader m)

-- * exceptions

-- | FIXME: this will have constructors dedicated for specific errors, and 'ServantErr' will only be
-- introduced later.
newtype PersistExcept = PersistExcept { unPersistExcept :: ServantErr }
    deriving (Eq, Show)

makePrisms ''PersistExcept

instance ThrowError500 PersistExcept where
    error500 = _PersistExcept . error500


-- * state interface

-- | The argument is a consistency check that will throw an error if it fails.
--
-- This can be equipped with a switch for performance, but if at all possible it would be nice to
-- run the checks even in production.
assertAulaDataM :: AQuery () -> AUpdate ()
assertAulaDataM = liftAQuery


type AddDb a = UserWithProto a -> AUpdate a

-- | @addDb l (u, p)@ adds a record to the DB.
-- The record is added on the behalf of the user @u@.
-- The record is computed from the prototype @p@, the current time and the given user @u@.
-- The record is added at the location pointed by the traversal @l@.
--
-- It is expected that @l@ points to exactly one target (checked by 'assertAulaDataM').
--
-- We could make the type of @l@ be @AulaLens (AMap a)@ which would enforce the constraint
-- above at the expense of pushing the burden towards cases where the traversal is only a
-- lens when some additional assumptions are met (see addReplyToIdeaComment for instance).
--
-- It could make sense for the traversal to point to more than one target for instance
-- to index the record at different locations. For instance we could keep an additional
-- global map of the comments, votes, likes and still call @addDb@ only once.
addDb :: forall a. (HasMetaInfo a, FromProto a) => AulaTraversal (AMap a) -> AddDb a
addDb l (cUser, pa) = do
    assertAulaDataM $ do
        len <- asks (lengthOf l)
        when (len /= 1) $ do
            fail $ "Persistent.Api.addDb expects the location (lens, traversal) "
                <> "to target exactly 1 field not " <> show len
    a :: a <- fromProto pa <$> nextMetaInfo cUser
    modifyDb_ l $ at (a ^. _Id) .~ Just a
    return a

addDbValue :: (HasMetaInfo a, FromProto a) => AulaTraversal a -> AddDb a
addDbValue l (cUser, pa) = do
    a <- fromProto pa <$> nextMetaInfo cUser
    modifyDb_ l (const a)
    return a

findIn :: AulaGetter [a] -> (a -> Bool) -> AQuery (Maybe a)
findIn l = views l . find

findAllIn :: AulaGetter [a] -> (a -> Bool) -> AQuery [a]
findAllIn l = views l . filter

findInBy :: Eq b => AulaGetter [a] -> Fold a b -> b -> AQuery (Maybe a)
findInBy l f b = findIn l (\x -> x ^? f == Just b)

findAllInBy :: Eq b => AulaGetter [a] -> Fold a b -> b -> AQuery [a]
findAllInBy l f b = findAllIn l (\x -> x ^? f == Just b)

findInById :: HasMetaInfo a => AulaGetter (AMap a) -> AUID a -> AQuery (Maybe a)
findInById l i = view (l . at i)

getSpaces :: AQuery [IdeaSpace]
getSpaces = view dbSpaces

getIdeas :: AQuery [Idea]
getIdeas = view dbIdeas

getWildIdeas :: AQuery [Idea]
getWildIdeas = filter (isWild . view ideaLocation) <$> getIdeas

getIdeasWithTopic :: AQuery [Idea]
getIdeasWithTopic = filter (not . isWild . view ideaLocation) <$> getIdeas

-- | If idea space already exists, do nothing.  Otherwise, create it.
addIdeaSpaceIfNotExists :: IdeaSpace -> AUpdate ()
addIdeaSpaceIfNotExists ispace = do
    exists <- (ispace `elem`) <$> liftAQuery getSpaces
    unless exists $ modifyDb_ dbSpaceSet (Set.insert ispace)

addIdea :: AddDb Idea
addIdea = addDb dbIdeaMap

findIdea :: AUID Idea -> AQuery (Maybe Idea)
findIdea = findInById dbIdeaMap

findIdeasByUserId :: AUID User -> AQuery [Idea]
findIdeasByUserId uId = findAllIn dbIdeas (\i -> i ^. createdBy == uId)

-- | FIXME deal with changedBy and changedAt
modifyAMap :: AulaLens (AMap a) -> AUID a -> (a -> a) -> AUpdate ()
modifyAMap l ident = modifyDb_ (l . at ident . _Just)

modifyIdea :: AUID Idea -> (Idea -> Idea) -> AUpdate ()
modifyIdea = modifyAMap dbIdeaMap

modifyUser :: AUID User -> ModifyUserOp -> AUpdate ()
modifyUser uid = modifyAMap dbUserMap uid . modifyUserOp

data ModifyUserOp = ModifyUserSetEmail UserEmail

modifyUserOp :: ModifyUserOp -> User -> User
modifyUserOp (ModifyUserSetEmail email) = userEmail .~ Just email

modifyTopic :: AUID Topic -> (Topic -> Topic) -> AUpdate ()
modifyTopic = modifyAMap dbTopicMap

findUser :: AUID User -> AQuery (Maybe User)
findUser = findInById dbUserMap

getUsers :: AQuery [User]
getUsers = view dbUsers

getTopics :: AQuery [Topic]
getTopics = view dbTopics

moveIdeasToLocation :: [AUID Idea] -> IdeaLocation -> AUpdate ()
moveIdeasToLocation ideaIds location =
    for_ ideaIds $ \ideaId ->
        modifyIdea ideaId $ ideaLocation .~ location

addTopic :: AddDb Topic
addTopic pt = do
    t <- addDb dbTopicMap pt
    -- FIXME a new topic should not be able to steal ideas from other topics of course the UI will
    -- hide this risk since only ideas without topics will be visible.
    -- Options:
    -- - Make it do nothing
    -- - Make it fail hard
    moveIdeasToLocation (pt ^. _2 . protoTopicIdeas) (topicIdeaLocation t)
    return t

addDelegation :: AddDb Delegation
addDelegation = addDb dbDelegationMap

findDelegationsByContext :: DelegationContext -> AQuery [Delegation]
findDelegationsByContext ctx = filter ((== ctx) . view delegationContext) . Map.elems
    <$> view dbDelegationMap

findUserByLogin :: UserLogin -> AQuery (Maybe User)
findUserByLogin = findInBy dbUsers userLogin

findTopic :: AUID Topic -> AQuery (Maybe Topic)
findTopic = findInById dbTopicMap

findTopicsBySpace :: IdeaSpace -> AQuery [Topic]
findTopicsBySpace = findAllInBy dbTopics topicIdeaSpace

findIdeasByTopicId :: AUID Topic -> AQuery [Idea]
findIdeasByTopicId tid = do
    mt <- findTopic tid
    case mt of
        Nothing -> pure []
        Just t  -> findIdeasByTopic t

findIdeasByTopic :: Topic -> AQuery [Idea]
findIdeasByTopic = findAllInBy dbIdeas ideaLocation . topicIdeaLocation

findWildIdeasBySpace :: IdeaSpace -> AQuery [Idea]
findWildIdeasBySpace space = findAllIn dbIdeas ((== IdeaLocationSpace space) . view ideaLocation)

instance FromProto IdeaLike where
    fromProto () = IdeaLike

-- | FIXME: Same user can like the same idea more than once (Issue #308).
-- FIXME: Assumption: the given @AUID Idea@ MUST be in the DB.
addLikeToIdea :: AUID Idea -> AddDb IdeaLike
addLikeToIdea iid = addDb (dbIdeaMap . at iid . _Just . ideaLikes)

instance FromProto IdeaVote where
    fromProto = flip IdeaVote

-- | FIXME: Same user can vote on the same idea more than once (Issue #308).
-- FIXME: Check also that the given idea exists and is in the right phase.
addVoteToIdea :: AUID Idea -> AddDb IdeaVote
addVoteToIdea iid = addDb (dbIdeaMap . at iid . _Just . ideaVotes)

instance FromProto Comment where
    fromProto d m = Comment { _commentMeta      = m
                            , _commentText      = d
                            , _commentReplies   = nil
                            , _commentVotes     = nil
                            }


-- | FIXME: Assumption: the given @AUID Idea@ MUST be in the DB.
addCommentToIdea :: AUID Idea -> AddDb Comment
addCommentToIdea iid = addDb (dbIdeaMap . at iid . _Just . ideaComments)

-- | FIXME: Assumptions:
-- * the given @AUID Idea@ MUST be in the DB.
-- * the given @AUID Comment@ MUST be one of the comment of the given idea.
addReplyToIdeaComment :: AUID Idea -> AUID Comment -> AddDb Comment
addReplyToIdeaComment iid cid =
    addDb (dbIdeaMap . at iid . _Just . ideaComments . at cid . _Just . commentReplies)

instance FromProto CommentVote where
    fromProto = flip CommentVote

-- | FIXME: Assumptions:
-- * the given @AUID Idea@ MUST be in the DB.
-- * the given @AUID Comment@ MUST be one of the comment of the given idea.
addCommentVoteToIdeaComment :: AUID Idea -> AUID Comment -> AddDb CommentVote
addCommentVoteToIdeaComment iid cid =
    addDb (dbIdeaMap . at iid . _Just . ideaComments . at cid . _Just . commentVotes)

-- | FIXME: Assumptions:
-- * the given @AUID Idea@ MUST be in the DB.
-- * the first given @AUID Comment@ MUST be one of the comment of the given idea.
-- * the second given @AUID Comment@ MUST be one of the comment of the first given comment.
addCommentVoteToIdeaCommentReply :: AUID Idea -> AUID Comment -> AUID Comment -> AddDb CommentVote
addCommentVoteToIdeaCommentReply iid cid rid =
    addDb (dbIdeaMap . at iid . _Just . ideaComments
                     . at cid . _Just . commentReplies
                     . at rid . _Just . commentVotes)

instance FromProto IdeaResult where
    fromProto = flip IdeaResult

addIdeaResult :: AUID Idea -> AddDb IdeaResult
addIdeaResult iid =
    addDbValue (dbIdeaMap . at iid . _Just . ideaResult . _Just)

nextId :: AUpdate (AUID a)
nextId = AUID <$> modifyDb dbLastId (+1)

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

addUser :: UserPass -> AddDb User
addUser defaultPass (cUser, proto) = do
    metainfo  <- nextMetaInfo cUser
    uLogin    <- maybe (mkUserLogin proto) pure (proto ^. protoUserLogin)
    let uPassword = fromMaybe defaultPass $ proto ^. protoUserPassword
    let user = userFromProto metainfo uLogin uPassword proto
    modifyDb_ dbUserMap $ at (user ^. _Id) .~ Just user
    return user

-- | When adding the first user, there is no creator yet, so the first user creates itself.  Login
-- name and password must be 'Just' in the proto user.
addFirstUser :: Timestamp -> Proto User -> AUpdate User
addFirstUser now proto = do
    uid <- nextId
    let uLogin    = fromMaybe (error "addFirstUser: no login name") (proto ^. protoUserLogin)
        uPassword = fromMaybe (error "addFirstUser: no passphrase") (proto ^. protoUserPassword)
        -- the user creates herself
        cUser = _Id .~ uid $ user
        metainfo = mkMetaInfo cUser now uid
        user = userFromProto metainfo uLogin uPassword proto

    modifyDb_ dbUserMap $ at (user ^. _Id) .~ Just user
    return user

mkUserLogin :: ProtoUser -> AUpdate UserLogin
mkUserLogin protoUser = pick (gen firstn lastn)
  where
    firstn :: ST = protoUser ^. protoUserFirstName . fromUserFirstName
    lastn  :: ST = protoUser ^. protoUserLastName  . fromUserLastName

    pick :: [ST] -> AUpdate UserLogin
    pick ((UserLogin -> l):ls) = maybe (pure l) (\_ -> pick ls) =<< liftAQuery (findUserByLogin l)
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

nextMetaInfo :: User -> AUpdate (MetaInfo a)
nextMetaInfo cUser = mkMetaInfo cUser <$> (view whoWhenTimestamp <$> ask) <*> nextId
