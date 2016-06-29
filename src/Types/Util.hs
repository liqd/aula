{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DefaultSignatures           #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE KindSignatures              #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE Rank2Types                  #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Types.Util
where

import Control.Lens hiding ((<.>))
import Data.Set.Lens (setOf)
import Crypto.Scrypt
import Data.Set as Set (Set, intersection, singleton, member)
import Data.Map as Map (fromList)
import Data.Maybe (isJust, mapMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.String
import Data.String.Conversions
import Data.UriPath (HasUriPart(uriPart))
import GHC.Generics (Generic)
import Lucid (ToHtml, toHtml, toHtmlRaw)
import Network.Mail.Mime (Address(Address))
import Servant.API (FromHttpApiData(parseUrlPiece))

import qualified Generics.SOP as SOP
import qualified Text.Email.Validate as Email

import Test.QuickCheck (Gen, Arbitrary, arbitrary)

import Types.Prelude
import Types.Core


-- * quickcheck

-- | FIXME: should either go to the test suite or go away completely.
class Monad m => GenArbitrary m where
    genGen :: Gen a -> m a

-- | FIXME: should either go to the test suite or go away completely.
genArbitrary :: (GenArbitrary m, Arbitrary a) => m a
genArbitrary = genGen arbitrary


-- * meta info class

class Ord (IdOf a) => HasMetaInfo a where
    metaInfo        :: Lens' a (MetaInfo a)
    _Key            :: Lens' a (KeyOf a)
    _Key            = metaInfo . metaKey
    _Id             :: Lens' a (IdOf a)
    _Id             = _Key . idOfKey (Proxy :: Proxy a)
    idOfKey         :: Proxy a -> Lens' (KeyOf a) (IdOf a)
    default idOfKey :: Proxy a -> Lens' (AUID a) (AUID a)
    idOfKey _       = id
    createdBy       :: Lens' a (AUID User)
    createdBy       = metaInfo . metaCreatedBy
    createdByLogin  :: Lens' a UserLogin
    createdByLogin  = metaInfo . metaCreatedByLogin
    createdByAvatar :: AvatarDimension -> Getter a URL
    createdByAvatar dim = metaInfo . metaCreatedBy . avatarUrl dim
    createdAt       :: Lens' a Timestamp
    createdAt       = metaInfo . metaCreatedAt
    changedBy       :: Lens' a (AUID User)
    changedBy       = metaInfo . metaChangedBy
    changedAt       :: Lens' a Timestamp
    changedAt       = metaInfo . metaChangedAt

instance HasMetaInfo Idea           where metaInfo = ideaMeta
instance HasMetaInfo IdeaJuryResult where metaInfo = ideaJuryResultMeta
instance HasMetaInfo IdeaVoteResult where metaInfo = ideaVoteResultMeta
instance HasMetaInfo Topic          where metaInfo = topicMeta
instance HasMetaInfo User           where metaInfo = userMeta

instance HasMetaInfo Comment where
    metaInfo  = commentMeta
    idOfKey _ = ckCommentId
instance HasMetaInfo CommentVote where
    metaInfo  = commentVoteMeta
    idOfKey _ = cvUser
instance HasMetaInfo IdeaVote where
    metaInfo  = ideaVoteMeta
    idOfKey _ = ivUser
instance HasMetaInfo IdeaLike where
    metaInfo  = ideaLikeMeta
    idOfKey _ = ivUser

aMapFromList :: HasMetaInfo a => [a] -> AMap a
aMapFromList = Map.fromList . map (\x -> (x ^. _Id, x))


-- * user

newtype PasswordToken = PasswordToken { unPasswordToken :: ST }
  deriving (Eq, Generic, Ord, Read, Show)

instance HasUriPart PasswordToken where
    uriPart = fromString . cs . unPasswordToken

-- | FIXME: Smart constructor for tokens
instance FromHttpApiData PasswordToken where
    parseUrlPiece = Right . PasswordToken . cs

data PasswordTokenState
    = Invalid
    | TimedOut
    | Valid
  deriving (Eq, Generic, Ord, Read, Show)


_GuestRole :: Prism' Role IdeaSpace
_GuestRole = prism guestRole $ \case
    SchoolGuest  -> pure SchoolSpace
    ClassGuest c -> pure $ ClassSpace c
    r            -> Left r

-- | RoleScope is a summary about roles. It summarizes the visibility of a given role
-- or set of roles. The RoleScope tells whether the user is limited to a set of classes
-- or if the user has access to all classes. Roles such as Moderator, Principal and Admin
-- have access to all classes and thus are assigned the SchoolScope.
-- RoleScope is a monoid, the unit (mempty) corresponds to having no roles whatsoever and
-- thus being limited to the empty set of classes. When a user has multiple roles, the
-- scope makes the union of the sets of classes to which the user is limited to.
-- Finally when a user has both SchoolScope role and ClassesScope then the SchoolScope
-- wins over. SchoolScope is annihilating we say.
--
-- Now consider that the class 'c' is not a member of 'user ^.. userRoles . roleSchoolClass',
-- the user might still be allowed to access the class 'c' if this included a role such as Admin.
--
-- Instead using 'user ^.. userRoles . roleScope' with a case is much more explicit, 'SchoolScope'
-- would authorize access to all the classes and 'ClassesScope' would limit it to this set.
data RoleScope
  = ClassesScope (Set SchoolClass)
  | SchoolScope
  deriving (Eq, Ord, Show, Read, Generic)

instance Monoid RoleScope where
    mempty = ClassesScope mempty
    SchoolScope `mappend` _ = SchoolScope
    _ `mappend` SchoolScope = SchoolScope
    ClassesScope xs `mappend` ClassesScope ys = ClassesScope $ xs `mappend` ys


verifyUserPass :: ST -> UserPass -> Bool
verifyUserPass pwd = \case
    UserPassInitial (InitialPassword p)           -> p == pwd
    UserPassEncrypted (ScryptEncryptedPassword p) -> verifyPass' (Pass (cs pwd)) (EncryptedPass p)
    UserPassDeactivated                           -> False


unsafeEmailAddress :: (ConvertibleStrings local SBS, ConvertibleStrings domain SBS) =>
                      local -> domain -> EmailAddress
unsafeEmailAddress local domain = InternalEmailAddress $ Email.unsafeEmailAddress (cs local) (cs domain)

{- Example:
    u :: User
    s :: Maybe ST
    s = u ^? userEmailAddress
-}
userEmailAddress :: CSI' s SBS => Fold User s
userEmailAddress = userEmail . _Just . re emailAddress

userPassword :: Lens' User UserPass
userPassword = userSettings . userSettingsPassword

userEmail :: Lens' User (Maybe EmailAddress)
userEmail = userSettings . userSettingsEmail

userRoles :: Fold User Role
userRoles = userRoleSet . folded

userSchoolClasses :: Fold User SchoolClass
userSchoolClasses = userRoles . roleSchoolClass

hasRole :: User -> Role -> Bool
hasRole user role = role `Set.member` (user ^. userRoleSet)

isAdmin :: User -> Bool
isAdmin = (`hasRole` Admin)

roleScope :: Getter Role RoleScope
roleScope = to $ \r ->
    case r ^? roleSchoolClass of
        Nothing -> SchoolScope
        Just cl -> ClassesScope $ Set.singleton cl

rolesScope :: Fold (Set Role) RoleScope
rolesScope = folded . roleScope

userRoleScope :: Fold User RoleScope
userRoleScope = userRoles . roleScope

commonSchoolClasses :: User -> User -> Set SchoolClass
commonSchoolClasses user user' =
    Set.intersection (setOf userSchoolClasses user)
                     (setOf userSchoolClasses user')

isOwnProfile :: User -> User -> Bool
isOwnProfile currentUser otherUser =
    currentUser ^. _Id == otherUser ^. _Id

onActiveUser :: a -> (User -> a) -> User -> a
onActiveUser x f u
    | isActiveUser u = f u
    | otherwise      = x

userFullName :: (ConvertibleStrings ST s) => User -> s
userFullName = cs . onActiveUser
    "[Nutzer gelöscht]"
    (\u -> u ^. userFirstName . _UserFirstName <> " " <> u ^. userLastName . _UserLastName)

-- | Show full name and email address.  Should only be displayed to admins.
dangerousUserLongName :: User -> ST
dangerousUserLongName = onActiveUser
    "[Nutzer gelöscht]"
    (\u -> userFullName u <> " [" <> u ^. userLogin . unUserLogin <> email u <> "]")
  where
    email u = maybe nil ((", " <>) . (emailAddress #)) $ u ^. userEmail

userAddress :: User -> Maybe Address
userAddress u = u ^? userEmailAddress . to (Address . Just $ userFullName u)

isDeletedUser :: User -> Bool
isDeletedUser = has $ userSettings . userSettingsPassword . _UserPassDeactivated

isActiveUser :: User -> Bool
isActiveUser = not . isDeletedUser

makeUserView :: User -> UserView
makeUserView u =
    if isDeletedUser u
        then DeletedUser u
        else ActiveUser u

activeUsers :: [UserView] -> [User]
activeUsers = mapMaybe (^? activeUser)


-- * idea

isFeasibleIdea :: Idea -> Bool
isFeasibleIdea = has $ ideaJuryResult . _Just . ideaJuryResultValue . _Feasible

ideaJuryResultValueToType :: IdeaJuryResultValue -> IdeaJuryResultType
ideaJuryResultValueToType NotFeasible{} = IdeaNotFeasible
ideaJuryResultValueToType Feasible{}    = IdeaFeasible

showJuryResultTypeUI :: IdeaJuryResultType -> ST
showJuryResultTypeUI IdeaNotFeasible = "nicht durchführbar"
showJuryResultTypeUI IdeaFeasible    = "durchführbar"

ideaHasCreatorStatement :: Idea -> Bool
ideaHasCreatorStatement = has $ ideaVoteResult . _Just . ideaVoteResultValue . _Winning . _Just

isWinning :: Idea -> Bool
isWinning = has $ ideaVoteResult . _Just . ideaVoteResultValue . _Winning

userVotedOnIdea :: User -> Idea -> Maybe IdeaVoteValue
userVotedOnIdea user idea =
    idea ^? ideaVotes . at (user ^. _Id) . _Just . ideaVoteValue

userLikesIdea :: User -> Idea -> Bool
userLikesIdea user idea =
    isJust $ idea ^? ideaLikes . at (user ^. _Id) . _Just


-- * comment

data CommentNesting
    = TopComment
    | NestedComment
  deriving (Eq, Show)

commentNestingElim :: t -> t -> CommentNesting -> t
commentNestingElim top nested = \case
    TopComment    -> top
    NestedComment -> nested

foldComment :: Fold Comment Comment
foldComment = cosmosOf (commentReplies . each)

foldComments :: Fold Comments Comment
foldComments = each . foldComment

commentsCount :: Getter Comments Int
commentsCount = to $ lengthOf foldComments

countIdeaVotes :: IdeaVoteValue -> IdeaVotes -> Int
countIdeaVotes v = countEq v ideaVoteValue

countCommentVotes :: UpDown -> CommentVotes -> Int
countCommentVotes v = countEq v commentVoteValue

commentNesting :: Comment -> CommentNesting
commentNesting c = case c ^. _Key . ckParents . to length of
    0 -> TopComment
    1 -> NestedComment
    n -> error $ "IMPOSSIBLE: Comment kind list length: " <> show n

-- Given a list of parents and a collection (AMap) of comments
-- returns the collection of comments after following the parenting list.
traverseParents :: [AUID Comment] -> Traversal' Comments Comments
traverseParents []     = id
traverseParents (p:ps) = at p . _Just . commentReplies . traverseParents ps


-- * idea space, topic, phase

-- | TODO: rename to 'showIdeaSpaceKind'
showIdeaSpaceCategory :: IsString s => IdeaSpace -> s
showIdeaSpaceCategory SchoolSpace    = "school"
showIdeaSpaceCategory (ClassSpace _) = "class"

-- | Construct an 'IdeaLocation' from a 'Topic'
topicIdeaLocation :: Topic -> IdeaLocation
topicIdeaLocation = IdeaLocationTopic <$> (^. topicIdeaSpace) <*> (^. _Id)

ideaMaybeTopicId :: Lens' Idea (Maybe (AUID Topic))
ideaMaybeTopicId = ideaLocation . ideaLocationMaybeTopicId

ideaTopicId :: Traversal' Idea (AUID Topic)
ideaTopicId = ideaLocation . ideaLocationTopicId

ideaLocationMaybeTopicId :: Lens' IdeaLocation (Maybe (AUID Topic))
ideaLocationMaybeTopicId f = \case
    IdeaLocationSpace spc     -> mk spc <$> f Nothing
    IdeaLocationTopic spc tid -> mk spc <$> f (Just tid)
  where
    mk spc = \case
        Nothing  -> IdeaLocationSpace spc
        Just tid -> IdeaLocationTopic spc tid

isWild :: IdeaLocation -> Bool
isWild (IdeaLocationSpace _)   = True
isWild (IdeaLocationTopic _ _) = False

-- | Edit topic description and add ideas to topic.
data EditTopicData = EditTopicData
    { _editTopicTitle    :: ST
    , _editTopicDesc     :: PlainDocument
    , _editTopicAddIdeas :: [AUID Idea]
    }
  deriving (Eq, Ord, Show, Read, Generic)


isPhaseFrozen :: Phase -> Bool
isPhaseFrozen = has (phaseWildFrozen . _Frozen <> phaseStatus . _FrozenPhase . like ())

phaseLeftoverFrom :: Timestamp -> Lens' PhaseStatus Timespan
phaseLeftoverFrom now f = \case
    ActivePhase end      -> ActivePhase <$> fromNow now f end
    FrozenPhase leftover -> FrozenPhase <$> f leftover

followsPhase :: Phase -> Phase -> Bool
followsPhase PhaseJury       (PhaseRefinement _) = True
followsPhase (PhaseVoting _) PhaseJury           = True
followsPhase PhaseResult     (PhaseVoting _)     = True
followsPhase _               _                   = False


-- | Elaboration and Voting phase durations
-- FIXME: 'elaboration' and 'refinement' are the same thing.  pick one term!
-- ('elaboration' is my preference ~~fisx)
data Durations = Durations
    { _elaborationPhase :: DurationDays
    , _votingPhase      :: DurationDays
    }
  deriving (Eq, Show, Read, Generic)

data Quorums = Quorums
    { _schoolQuorumPercentage :: Int
    , _classQuorumPercentage  :: Int -- (there is only one quorum for all classes, see gh#318)
    }
  deriving (Eq, Show, Read, Generic)

data Settings = Settings
    { _durations :: Durations
    , _quorums   :: Quorums
    , _freeze    :: Freeze
    }
  deriving (Eq, Show, Read, Generic)

defaultSettings :: Settings
defaultSettings = Settings
    { _durations = Durations { _elaborationPhase = 21, _votingPhase = 21 }
    , _quorums   = Quorums   { _schoolQuorumPercentage = 30, _classQuorumPercentage = 30 }
    , _freeze    = NotFrozen
    }


data PhaseChangeDir = Backward | Forward
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)

instance HasUILabel PhaseChangeDir where
    uilabel Forward  = "vorwärts"
    uilabel Backward = "zurück"

instance ToHtml PhaseChangeDir where
    toHtmlRaw = toHtml
    toHtml    = toHtml . uilabelST


data ListIdeasInTopicTab =
    ListIdeasInTopicTabAll       -- ^ feasible as well as infeasible
  | ListIdeasInTopicTabVoting    -- ^ feasible, but not accepted (yet); see 'ideaAccepted'
  | ListIdeasInTopicTabAccepted  -- ^ feaasible and accepted
  | ListIdeasInTopicTabWinning   -- ^ feasible, accepted, and marked as winning
  deriving (Eq, Ord, Show, Read, Generic)

data MoveIdea
    = MoveIdeaToWild
    | MoveIdeaToTopic (AUID Topic)
  deriving (Eq, Ord, Show, Read, Generic)

moveIdeaElim :: forall t . t -> (AUID Topic -> t) -> MoveIdea -> t
moveIdeaElim wild topic = \case
    MoveIdeaToWild    -> wild
    MoveIdeaToTopic t -> topic t


-- * delegations

fullDScopeToDScope :: DScopeFull -> DScope
fullDScopeToDScope = \case
    DScopeGlobalFull       -> DScopeGlobal
    DScopeIdeaSpaceFull is -> DScopeIdeaSpace is
    DScopeTopicFull t      -> DScopeTopicId (t ^. _Id)
    DScopeIdeaFull i       -> DScopeIdeaId (i ^. _Id)


-- * boilerplate instances

instance SOP.Generic Durations
instance SOP.Generic EditTopicData
instance SOP.Generic ListIdeasInTopicTab
instance SOP.Generic MoveIdea
instance SOP.Generic PasswordToken
instance SOP.Generic PasswordTokenState
instance SOP.Generic PhaseChangeDir
instance SOP.Generic Quorums
instance SOP.Generic RoleScope
instance SOP.Generic Settings

makePrisms ''RoleScope

makeLenses ''Durations
makeLenses ''EditTopicData
makeLenses ''Quorums
makeLenses ''RoleScope
makeLenses ''Settings

deriveSafeCopy 0 'base ''Durations
deriveSafeCopy 0 'base ''EditTopicData
deriveSafeCopy 0 'base ''MoveIdea
deriveSafeCopy 0 'base ''PasswordToken
deriveSafeCopy 0 'base ''Quorums
deriveSafeCopy 0 'base ''Settings
