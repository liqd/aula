{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE GADTs                       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TupleSections               #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

-- | This module exports acid-state transactions.  It introduces a state type 'AulaData' and 'Query'
-- and 'Update' operations on that state.  (To be more specific, a few new types for queries and
-- updates are defined that specific to 'AulaData' and introduce exceptions.)
--
-- Serialization happens outside of this module.
--
-- FIXME: get some structure into the export list.
-- FIXME: consider removing Purescript.Idiom and doing everything here.
module Persistent.Pure
    ( AulaData
    , AMap
    , AulaLens
    , AulaGetter
    , AulaSetter
    , emptyAulaData

    , EnvWith(..), EnvWithProto, envUser, envNow, envWith
    , AEvent, Query, EQuery, MQuery, AUpdate(AUpdate), AddDb
    , runAUpdate
    , aUpdateEvent
    , WhoWhen(_whoWhenTimestamp, _whoWhenUID), whoWhenTimestamp, whoWhenUID

    , PersistExcept(..)
    , _PersistError500, _PersistError404, _PersistErrorNotImplemented
    , runPersistExcept
    , HasAUpdate
    , liftAQuery

    , addDb
    , findIn
    , findInBy
    , findInById
    , findAllIn
    , findAllInBy
    , maybe404

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
    , modifyUser
    , setUserEmail
    , setUserPass
    , setUserRole
    , setUserAvatar
    , getTopics
    , addTopic
    , editTopic
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
    , dbDurations
    , dbQuorums
    , dbSettings
    , adminUsernameHack
    , addDelegation
    , findDelegationsByContext
    , addIdeaJuryResult
    , addIdeaVoteResult
    , editIdea
    , saveDurations
    , saveQuorums
    , dangerousResetAulaData
    )
where

import Control.Lens
import Control.Monad.Except (MonadError, ExceptT(ExceptT), runExceptT, throwError)
import Control.Monad.Reader (MonadReader, runReader, asks)
import Control.Monad.State (MonadState, gets, put)
import Control.Monad (unless, replicateM, when)
import Data.Acid.Core
import Data.Acid.Memory.Pure (Event(UpdateEvent))
import Data.Acid (UpdateEvent, EventState, EventResult)
import Data.Foldable (find, for_)
import Data.List (nub)
import Data.Maybe
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set (Set)
import Data.String.Conversions (ST, cs, (<>))
import Data.Typeable (Typeable, typeRep)
import Servant
import Servant.Missing (ThrowError500(..))

import qualified Data.Acid as Acid
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import qualified Data.Text as ST

import Types


-- * state type

data AulaData = AulaData
    { _dbSpaceSet            :: Set IdeaSpace
    , _dbIdeaMap             :: Ideas
    , _dbUserMap             :: Users
    , _dbTopicMap            :: Topics
    , _dbDelegationMap       :: Delegations
    , _dbSettings            :: Settings
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
emptyAulaData = AulaData nil nil nil nil nil defaultSettings 0


-- * transactions

data WhoWhen = WhoWhen
    { _whoWhenTimestamp :: Timestamp
    , _whoWhenUID       :: AUID User
    }

makeLenses ''WhoWhen

type AEvent = Event AulaData

-- | 'Query' for 'AulaData'.  Doesn't contain the 'WhoWhen' context,
-- because that would make the stack contain two readers.
type Query a = forall m. MonadReader AulaData m => m a

-- | Same as 'Query' but can throw 'PersistExcept'.
type EQuery a = forall m. (MonadError PersistExcept m, MonadReader AulaData m) => m a

-- | This shortcut for 'EQuery' that throws the appropriate 'PersistExcept' on 'Nothing'.
type MQuery a = Query (Maybe a)

-- | 'Update' for 'AulaData'.  Can throw 'PersistExcept'.
newtype AUpdate a = AUpdate { _unAUpdate :: ExceptT PersistExcept (Acid.Update AulaData) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError PersistExcept
           , MonadState AulaData
           )

maybe404 :: forall m a. (MonadError PersistExcept m, Typeable a) => Maybe a -> m a
maybe404 = maybe (throwError . PersistError404 . show . typeRep $ (Proxy :: Proxy a)) pure

runAUpdate :: AUpdate a -> Acid.Update AulaData (Either PersistExcept a)
runAUpdate = runExceptT . _unAUpdate

aUpdateEvent :: (UpdateEvent ev, EventState ev ~ AulaData, EventResult ev ~ Either PersistExcept a)
             => (ev -> AUpdate a) -> AEvent
aUpdateEvent f = UpdateEvent $ runAUpdate . f

type HasAUpdate ev a = (UpdateEvent ev, MethodState ev ~ AulaData, MethodResult ev ~ Either PersistExcept a)

liftAQuery :: Query a -> AUpdate a
liftAQuery m = gets (runReader m)

-- * exceptions

-- | FIXME: this will have constructors dedicated for specific errors, and 'ServantErr' will only be
-- introduced later.
data PersistExcept
    = PersistError500 { persistErrorMessage :: String }
        -- FIXME: rename to PersistExceptInternal; drop ThrowError500 instance for something prismatic just for PersistExcept
    | PersistError404 { persistErrorMessage :: String }
        -- FIXME: rename to PersistExceptNotFound
    | PersistErrorNotImplemented { persistErrorMessage :: String }
    deriving (Eq, Show)

makePrisms ''PersistExcept

instance ThrowError500 PersistExcept where
    error500 = _PersistError500

deriveSafeCopy 0 'base ''PersistExcept

runPersistExcept :: PersistExcept -> ServantErr
runPersistExcept (PersistError500 msg)            = err500 { errBody = cs msg }
runPersistExcept (PersistError404 msg)            = err404 { errBody = cs msg }
runPersistExcept (PersistErrorNotImplemented msg) = err500 { errBody = cs msg }


-- * state interface

-- | The argument is a consistency check that will throw an error if it fails.
--
-- This can be equipped with a switch for performance, but if at all possible it would be nice to
-- run the checks even in production.
assertAulaDataM :: Query () -> AUpdate ()
assertAulaDataM = liftAQuery

{-
    This type carries the information needed to add (or update) something to the DB,
    namely the current user, the current time and the last parameter depends on the
    application.

    It can be thought of as the type @Meta a@ but with only the information we cannot
    compute from the current state or input data. Also unlike @Meta@ which is embedded
    in all our types (@Idea@, @Topic@), @EnvWith@ is surrunding the data.

    See also @EnvWithProto@.
-}
data EnvWith a = EnvWith
    { _envUser :: User
    , _envNow  :: Timestamp
    , _envWith :: a
    }

makeLenses ''EnvWith

deriveSafeCopy 0 'base ''EnvWith

{-
    The type @EnvWithProto a@ is a synonym for @EnvWith (Proto a)@.
    Since @Proto a@ collects all the (non-meta) information about the creation an @a@ record,
    @EnvWithProto a@ contains all the information to create an @a@.
-}
type EnvWithProto a = EnvWith (Proto a)

{-
    Functions of type @AddDb a@ are commonly partial applications of @addDb@.
    Thus such a function still lacks the @EnvWithProto a@, namely some meta-data and the @Proto a@,
    combinators such as @addWithUser@ and @currentUserAddDb@ deal with building and providing the
    meta-data. On subtelty introduced by AcidState is that instead of using directly the functions
    of type @AddDb@ one must use their event counter part.
    For instance @addIdea@ has type @AddDb Idea@, namely @EnvWithProto Idea -> AUpdate Idea@
    while @AddIdea@ has type @EnvWithProto Idea -> AddIdea@.
    Here are some examples:
    * @currentUserAddDb AddIdea someUser@
    * @addWithUser AddIdea someUser someProtoIdea@
    * @addWithUser (AddLikeToIdea someIdeaId) someUser ()@
-}
type AddDb a = EnvWithProto a -> AUpdate a

-- | @addDb l (EnvWith u now p)@ adds a record to the DB.
-- The record is added on the behalf of the user @u@.
-- The record is computed from the prototype @p@, the current time @now@ and the given user @u@.
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
addDb l (EnvWith cUser now pa) = do
    assertAulaDataM $ do
        len <- asks (lengthOf l)
        when (len /= 1) $ do
            fail $ "Persistent.Api.addDb expects the location (lens, traversal) "
                <> "to target exactly 1 field not " <> show len
    a :: a <- fromProto pa <$> nextMetaInfo cUser now
    l . at (a ^. _Id) <?= a

addDbAppValue :: (HasMetaInfo a, FromProto a, Applicative ap) => AulaTraversal (ap a) -> AddDb a
addDbAppValue l (EnvWith cUser now pa) = do
    a <- fromProto pa <$> nextMetaInfo cUser now
    l .= pure a
    pure a

findIn :: AulaGetter [a] -> (a -> Bool) -> Query (Maybe a)
findIn l = views l . find

findAllIn :: AulaGetter [a] -> (a -> Bool) -> Query [a]
findAllIn l = views l . filter

findInBy :: Eq b => AulaGetter [a] -> Fold a b -> b -> Query (Maybe a)
findInBy l f b = findIn l (\x -> x ^? f == Just b)

findAllInBy :: Eq b => AulaGetter [a] -> Fold a b -> b -> Query [a]
findAllInBy l f b = findAllIn l (\x -> x ^? f == Just b)

findInById :: HasMetaInfo a => AulaGetter (AMap a) -> AUID a -> Query (Maybe a)
findInById l i = view (l . at i)

getSpaces :: Query [IdeaSpace]
getSpaces = view dbSpaces

getIdeas :: Query [Idea]
getIdeas = view dbIdeas

getWildIdeas :: Query [Idea]
getWildIdeas = filter (isWild . view ideaLocation) <$> getIdeas

getIdeasWithTopic :: Query [Idea]
getIdeasWithTopic = filter (not . isWild . view ideaLocation) <$> getIdeas

-- | If idea space already exists, do nothing.  Otherwise, create it.
addIdeaSpaceIfNotExists :: IdeaSpace -> AUpdate ()
addIdeaSpaceIfNotExists ispace = do
    exists <- (ispace `elem`) <$> liftAQuery getSpaces
    unless exists $ dbSpaceSet %= Set.insert ispace

addIdea :: AddDb Idea
addIdea = addDb dbIdeaMap

findIdea :: AUID Idea -> Query (Maybe Idea)
findIdea = findInById dbIdeaMap

findIdeasByUserId :: AUID User -> Query [Idea]
findIdeasByUserId uId = findAllIn dbIdeas (\i -> i ^. createdBy == uId)

-- | FIXME deal with changedBy and changedAt
modifyAMap :: AulaLens (AMap a) -> AUID a -> (a -> a) -> AUpdate ()
modifyAMap l ident = (l . at ident . _Just %=)

modifyIdea :: AUID Idea -> (Idea -> Idea) -> AUpdate ()
modifyIdea = modifyAMap dbIdeaMap

modifyUser :: AUID User -> (User -> User) -> AUpdate ()
modifyUser = modifyAMap dbUserMap

setUserEmail :: AUID User -> UserEmail -> AUpdate ()
setUserEmail uid = modifyUser uid . (userEmail ?~)

setUserPass :: AUID User -> Maybe ST -> Maybe ST -> Maybe ST -> AUpdate ()
setUserPass _uid _oldPass newPass1 newPass2 = do
    when (newPass1 /= newPass2) $ throwError500 "passwords do not match!"
    -- FIXME: check _oldPass
    -- FIXME: set newPass1
    return ()

setUserRole :: AUID User -> Role -> AUpdate ()
setUserRole uid = modifyUser uid . set userRole

setUserAvatar :: AUID User -> URL -> AUpdate ()
setUserAvatar uid = modifyUser uid . set userAvatar . Just

editTopic :: AUID Topic -> EditTopicData -> AUpdate ()
editTopic topicId (EditTopicData title desc ideas) = do
    topic <- maybe404 =<< liftAQuery (findTopic topicId)
    let space = topic ^. topicIdeaSpace
    modifyTopic topicId (set topicTitle title . set topicDesc desc)
    moveIdeasToLocation ideas (IdeaLocationTopic space topicId)

modifyTopic :: AUID Topic -> (Topic -> Topic) -> AUpdate ()
modifyTopic = modifyAMap dbTopicMap

findUser :: AUID User -> Query (Maybe User)
findUser = findInById dbUserMap

getUsers :: Query [User]
getUsers = view dbUsers

getTopics :: Query [Topic]
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
    moveIdeasToLocation (pt ^. envWith . protoTopicIdeas) (topicIdeaLocation t)
    return t

addDelegation :: AddDb Delegation
addDelegation = addDb dbDelegationMap

findDelegationsByContext :: DelegationContext -> Query [Delegation]
findDelegationsByContext ctx = filter ((== ctx) . view delegationContext) . Map.elems
    <$> view dbDelegationMap

findUserByLogin :: UserLogin -> Query (Maybe User)
findUserByLogin = findInBy dbUsers userLogin

findTopic :: AUID Topic -> Query (Maybe Topic)
findTopic = findInById dbTopicMap

findTopicsBySpace :: IdeaSpace -> Query [Topic]
findTopicsBySpace = findAllInBy dbTopics topicIdeaSpace

findIdeasByTopicId :: AUID Topic -> Query [Idea]
findIdeasByTopicId tid = do
    mt <- findTopic tid
    case mt of
        Nothing -> pure []
        Just t  -> findIdeasByTopic t

findIdeasByTopic :: Topic -> Query [Idea]
findIdeasByTopic = findAllInBy dbIdeas ideaLocation . topicIdeaLocation

findWildIdeasBySpace :: IdeaSpace -> Query [Idea]
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

instance FromProto IdeaJuryResult where
    fromProto = flip IdeaJuryResult

addIdeaJuryResult :: AUID Idea -> AddDb IdeaJuryResult
addIdeaJuryResult iid =
    addDbAppValue (dbIdeaMap . at iid . _Just . ideaJuryResult)

instance FromProto IdeaVoteResult where
    fromProto = flip IdeaVoteResult

addIdeaVoteResult :: AUID Idea -> AddDb IdeaVoteResult
addIdeaVoteResult iid =
    addDbAppValue (dbIdeaMap . at iid . _Just . ideaVoteResult)


nextId :: AUpdate (AUID a)
nextId = AUID <$> (dbLastId <+= 1)

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
addUser defaultPass (EnvWith cUser now proto) = do
    metainfo  <- nextMetaInfo cUser now
    uLogin    <- maybe (mkUserLogin proto) pure (proto ^. protoUserLogin)
    let uPassword = fromMaybe defaultPass $ proto ^. protoUserPassword
    let user = userFromProto metainfo uLogin uPassword proto
    dbUserMap . at (user ^. _Id) <?= user

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

    dbUserMap . at (user ^. _Id) <?= user

mkUserLogin :: ProtoUser -> AUpdate UserLogin
mkUserLogin protoUser = pick (gen firstn lastn)
  where
    firstn :: ST = protoUser ^. protoUserFirstName . fromUserFirstName
    lastn  :: ST = protoUser ^. protoUserLastName  . fromUserLastName

    pick :: [ST] -> AUpdate UserLogin
    pick ((UserLogin -> l):ls) = maybe (pure l) (\_ -> pick ls) =<< liftAQuery (findUserByLogin l)
    pick []                    = error "impossible.  (well, unlikely.)"
                                 -- ^ FIXME: use throwError(500) here?

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
        { _ideaMeta       = m
        , _ideaTitle      = i ^. protoIdeaTitle
        , _ideaDesc       = i ^. protoIdeaDesc
        , _ideaCategory   = i ^. protoIdeaCategory
        , _ideaLocation   = i ^. protoIdeaLocation
        , _ideaComments   = nil
        , _ideaLikes      = nil
        , _ideaVotes      = nil
        , _ideaJuryResult = Nothing
        , _ideaVoteResult = Nothing
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

nextMetaInfo :: User -> Timestamp -> AUpdate (MetaInfo a)
nextMetaInfo user now = mkMetaInfo user now <$> nextId

editIdea :: AUID Idea -> ProtoIdea -> AUpdate ()
editIdea ideaId = modifyIdea ideaId . newIdea
  where
    newIdea protoIdea = (ideaTitle .~ (protoIdea ^. protoIdeaTitle))
                      . (ideaDesc .~ (protoIdea ^. protoIdeaDesc))
                      . (ideaCategory .~ (protoIdea ^. protoIdeaCategory))

dbDurations :: Lens' AulaData Durations
dbQuorums   :: Lens' AulaData Quorums

dbDurations = dbSettings . durations
dbQuorums   = dbSettings . quorums

dbElaborationDuration :: Lens' AulaData DurationDays
dbVoteDuration        :: Lens' AulaData DurationDays
dbSchoolQuorum        :: Lens' AulaData Percent
dbClassQuorum         :: Lens' AulaData Percent

dbElaborationDuration = dbDurations . elaborationPhase
dbVoteDuration        = dbDurations . votingPhase
dbSchoolQuorum        = dbQuorums   . schoolQuorumPercentage
dbClassQuorum         = dbQuorums   . classQuorumPercentage

saveDurations :: Durations -> AUpdate ()
saveDurations = (dbDurations .=)

saveQuorums :: Quorums -> AUpdate ()
saveQuorums = (dbQuorums .=)

dangerousResetAulaData :: AUpdate ()
dangerousResetAulaData = put emptyAulaData
