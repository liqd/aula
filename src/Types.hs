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

module Types
    ( module Types
    , module Data.Markdown
    , readMaybe
    , module X
    )
where

import Types.Prelude as X

import Control.Lens hiding ((<.>))
import Data.Set.Lens (setOf)
import Control.Monad
import Crypto.Scrypt
import Data.Binary
import Data.Char
import Data.Function (on)
import Data.List as List (zipWith)
import Data.Set as Set (Set, intersection, singleton, member)
import Data.Map as Map (Map, fromList, lookup, unions, singleton)
import Data.Maybe (isJust, mapMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (base, SafeCopy(..), safeGet, safePut, contain, deriveSafeCopy)
import Data.String
import Data.String.Conversions
import Data.Time
import Data.UriPath (HasUriPart(uriPart))
import GHC.Generics (Generic)
import Lucid (ToHtml, toHtml, toHtmlRaw, div_)
import Network.HTTP.Media ((//))
import Network.Mail.Mime (Address(Address))
import Servant.API
    ( FromHttpApiData(parseUrlPiece), ToHttpApiData(toUrlPiece)
    , Accept, MimeRender, Headers(..), Header, contentType, mimeRender, addHeader
    )
import System.FilePath ((</>), (<.>))
import Text.Read (readMaybe)

import qualified Data.Aeson as Aeson
import qualified Data.Csv as CSV
import qualified Data.Text as ST
import qualified Data.Vector as Vector
import qualified Generics.Generic.Aeson as Aeson
import qualified Generics.SOP as SOP
import qualified Text.Email.Validate as Email

import Test.QuickCheck (Gen, Arbitrary, arbitrary)

import Data.Markdown
import Frontend.Constant


-- * misc

newtype DurationDays = DurationDays { unDurationDays :: Int }
  deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral, Generic)

instance SOP.Generic DurationDays

-- | Percentage values from 0 to 100, used in quorum computations.
type Percent = Int


-- * csv helpers

data CSV

instance Accept CSV where
    contentType Proxy = "text" // "csv"

type CsvHeaders a = Headers '[CsvHeadersContentDisposition] a
type CsvHeadersContentDisposition = Header "Content-Disposition" String  -- appease hlint v1.9.22

instance MimeRender CSV a => MimeRender CSV (CsvHeaders a) where
    mimeRender proxy (Headers v _) = mimeRender proxy v

csvHeaders :: String -> a -> CsvHeaders a
csvHeaders filename = addHeader $ "attachment; filename=" <> filename <> ".csv"


-- * prototypes for types

-- | Prototype for a type.
-- The prototypes contains all the information which cannot be
-- filled out of some type. Information which comes from outer
-- source and will be saved into the database.
--
-- FIXME: move this into 'FromProto'?
type family Proto type_ :: *

-- | The method how a 't' value is calculated from its prototype
-- and a metainfo to that.
class FromProto t where
    fromProto :: Proto t -> MetaInfo t -> t


-- * idea

-- | "Idee".  Ideas can be either be wild or contained in exactly one 'Topic'.
data Idea = Idea
    { _ideaMeta       :: MetaInfo Idea
    , _ideaTitle      :: ST
    , _ideaDesc       :: Document
    , _ideaCategory   :: Maybe Category
    , _ideaLocation   :: IdeaLocation
    , _ideaComments   :: Comments
    , _ideaLikes      :: IdeaLikes
    , _ideaVotes      :: IdeaVotes
    , _ideaJuryResult :: Maybe IdeaJuryResult  -- invariant: isJust => phase of containing topic > JuryPhsae
    , _ideaVoteResult :: Maybe IdeaVoteResult  -- invariant: isJust => phase of containing topic > VotingPhase
    , _ideaDeleted    :: Bool
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic Idea

-- | Invariant: for all @IdeaLocationTopic space tid@: idea space of topic with id 'tid' is 'space'.
data IdeaLocation =
      IdeaLocationSpace { _ideaLocationSpace :: IdeaSpace }
    | IdeaLocationTopic { _ideaLocationSpace :: IdeaSpace, _ideaLocationTopicId :: AUID Topic }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaLocation

instance HasUILabel IdeaLocation where
    uilabel (IdeaLocationSpace s) = uilabel s
    uilabel (IdeaLocationTopic s (AUID t)) = "Thema #" <> fromString (show t) <> " in " <> uilabel s

-- | Prototype for Idea creation.
data ProtoIdea = ProtoIdea
    { _protoIdeaTitle      :: ST
    , _protoIdeaDesc       :: Document
    , _protoIdeaCategory   :: Maybe Category
    , _protoIdeaLocation   :: IdeaLocation
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic ProtoIdea

type instance Proto Idea = ProtoIdea

data ListIdeasInTopicTab =
    ListIdeasInTopicTabAll       -- ^ feasible as well as infeasible
  | ListIdeasInTopicTabVoting    -- ^ feasible, but not accepted (yet); see 'ideaAccepted'
  | ListIdeasInTopicTabAccepted  -- ^ feaasible and accepted
  | ListIdeasInTopicTabWinning   -- ^ feasible, accepted, and marked as winning
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic ListIdeasInTopicTab

-- | "Kategorie"
data Category =
    CatRules        -- ^ "Regel"
  | CatEquipment    -- ^ "Ausstattung"
  | CatTeaching     -- ^ "Unterricht"
  | CatTime         -- ^ "Zeit"
  | CatEnvironment  -- ^ "Umgebung"
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

instance SOP.Generic Category

instance FromHttpApiData Category where
    parseUrlPiece = \case
        "rules"       -> Right CatRules
        "equipment"   -> Right CatEquipment
        "teaching"    -> Right CatTeaching
        "time"        -> Right CatTime
        "environment" -> Right CatEnvironment
        _             -> Left "no parse"

instance ToHttpApiData Category where
    toUrlPiece = \case
        CatRules       -> "rules"
        CatEquipment   -> "equipment"
        CatTeaching    -> "teaching"
        CatTime        -> "time"
        CatEnvironment -> "environment"

instance HasUILabel Category where
    uilabel = \case
        CatRules       -> "Regeln"
        CatEquipment   -> "Ausstattung"
        CatTeaching    -> "Unterricht"
        CatTime        -> "Zeit"
        CatEnvironment -> "Umgebung"


-- | FIXME: Is there a better name for 'Like'?  'Star'?  'Endorsement'?  'Interest'?
data IdeaLike = IdeaLike
    { _ideaLikeMeta     :: MetaInfo IdeaLike
    , _ideaLikeDelegate :: AUID User
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaLike

data ProtoIdeaLike = ProtoIdeaLike
    { _protoIdeaLikeDelegate :: AUID User
    }

type instance Proto IdeaLike = ProtoIdeaLike

-- | "Stimme" for "Idee".  As opposed to 'CommentVote'.
data IdeaVote = IdeaVote
    { _ideaVoteMeta     :: MetaInfo IdeaVote
    , _ideaVoteValue    :: IdeaVoteValue
    , _ideaVoteDelegate :: AUID User
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaVote

data ProtoIdeaVote = ProtoIdeaVote
    { _protoIdeaVoteValue    :: IdeaVoteValue
    , _protoIdeaVoteDelegate :: AUID User
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic ProtoIdeaVote

type instance Proto IdeaVote = ProtoIdeaVote

data IdeaVoteValue = Yes | No
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

instance SOP.Generic IdeaVoteValue

data IdeaVoteLikeKey = IdeaVoteLikeKey
    { _ivIdea :: AUID Idea
    , _ivUser :: AUID User
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaVoteLikeKey

data IdeaJuryResult = IdeaJuryResult
    { _ideaJuryResultMeta   :: MetaInfo IdeaJuryResult
    , _ideaJuryResultValue  :: IdeaJuryResultValue
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaJuryResult

data IdeaJuryResultType
    = IdeaNotFeasible
    | IdeaFeasible
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaJuryResultType

data IdeaJuryResultValue
    = NotFeasible { _ideaResultNotFeasibleReason :: Document }
    | Feasible    { _ideaResultFeasibleReason    :: Maybe Document }
  deriving (Eq, Ord, Show, Read, Generic)

type instance Proto IdeaJuryResult = IdeaJuryResultValue

ideaResultReason :: Traversal' IdeaJuryResultValue Document
ideaResultReason f = \case
    NotFeasible d -> NotFeasible <$> f d
    Feasible md   -> Feasible <$> traverse f md

ideaJuryResultValueToType :: IdeaJuryResultValue -> IdeaJuryResultType
ideaJuryResultValueToType NotFeasible{} = IdeaNotFeasible
ideaJuryResultValueToType Feasible{}    = IdeaFeasible

showJuryResultTypeUI :: IdeaJuryResultType -> ST
showJuryResultTypeUI IdeaNotFeasible = "nicht durchführbar"
showJuryResultTypeUI IdeaFeasible    = "durchführbar"

instance SOP.Generic IdeaJuryResultValue

data IdeaVoteResult = IdeaVoteResult
    { _ideaVoteResultMeta   :: MetaInfo IdeaVoteResult
    , _ideaVoteResultValue  :: IdeaVoteResultValue
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaVoteResult

data IdeaVoteResultValue
    = Winning     { _ideaResultCreatorStatement  :: Maybe Document }
    | EnoughVotes Bool
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaVoteResultValue

type instance Proto IdeaVoteResult = IdeaVoteResultValue

data MoveIdea
    = MoveIdeaToWild
    | MoveIdeaToTopic (AUID Topic)
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic MoveIdea

moveIdeaElim :: forall t . t -> (AUID Topic -> t) -> MoveIdea -> t
moveIdeaElim wild topic = \case
    MoveIdeaToWild    -> wild
    MoveIdeaToTopic t -> topic t

-- * comment

-- | "Verbesserungsvorschlag"
--
-- 'Comments' are hierarchical.  The application logic is responsible for putting some limit (if
-- any) on the recursion depth under which all children become siblings.
--
-- A comment has no implicit 'yes' vote by the author.  This gives the author the option of voting
-- for a comment, or even against it.  Even though the latter may never make sense, somebody may
-- still learn something from trying it out, and this is a teaching application.
data Comment = Comment
    { _commentMeta    :: MetaInfo Comment
    , _commentText    :: Document
    , _commentVotes   :: CommentVotes
    , _commentReplies :: Comments
    , _commentDeleted :: Bool
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic Comment

data CommentNesting
    = TopComment
    | NestedComment
  deriving (Eq, Show)

commentNestingElim :: t -> t -> CommentNesting -> t
commentNestingElim top nested = \case
    TopComment    -> top
    NestedComment -> nested

-- This is the complete information to recover a comment in AulaData
-- * ckParents: Comment identifiers from the root to the leaf. If `y`, follows `x` in ckParents,
--              then `y` is a reply to `x`. See also `traverseParents` for a use of that field.
data CommentKey = CommentKey
    { _ckIdeaLocation  :: IdeaLocation
    , _ckIdeaId        :: AUID Idea
    , _ckParents       :: [AUID Comment]
    , _ckCommentId     :: AUID Comment
    }
  deriving (Eq, Ord, Show, Read, Generic)

commentKey :: IdeaLocation -> AUID Idea -> AUID Comment -> CommentKey
commentKey loc iid = CommentKey loc iid []

replyKey :: IdeaLocation -> AUID Idea -> AUID Comment -> AUID Comment -> CommentKey
replyKey loc iid pid = CommentKey loc iid [pid]

instance SOP.Generic CommentKey

data CommentVoteKey = CommentVoteKey
    { _cvCommentKey :: CommentKey
    , _cvUser      :: AUID User
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic CommentVoteKey

newtype CommentContent = CommentContent { unCommentContent :: Document }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic CommentContent

type instance Proto Comment = CommentContent

-- | "Stimme" for "Verbesserungsvorschlag"
data CommentVote = CommentVote
    { _commentVoteMeta  :: MetaInfo CommentVote
    , _commentVoteValue :: UpDown
    }
  deriving (Eq, Ord, Show, Read, Generic)

type instance Proto CommentVote = UpDown

instance SOP.Generic CommentVote

data UpDown = Up | Down
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance SOP.Generic UpDown

data CommentContext = CommentContext
    { _parentIdea    :: Idea
    , _parentComment :: Maybe Comment
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic CommentContext


-- * idea space, topic, phase

-- | "Ideenraum" is one of "Klasse", "Schule".
data IdeaSpace =
    SchoolSpace
  | ClassSpace { _ideaSpaceSchoolClass :: SchoolClass }
  deriving (Eq, Show, Read, Generic)

-- e.g.: ["Klasse 10a", "Klasse 7b", "Klasse 7a"]
--   ==> ["Klasse 7a", "Klasse 7b", "Klasse 10a"]
instance Ord IdeaSpace where
    compare = compare `on` sortableName
      where
        sortableName :: IdeaSpace -> Maybe [Either String Int]
        sortableName SchoolSpace     = Nothing
        sortableName (ClassSpace cl) = Just . structured . cs . _className $ cl

        structured :: String -> [Either String Int]
        structured = nonDigits
          where
            digits xs = case span isDigit xs of
                            ([], []) -> []
                            ([], zs) -> nonDigits zs
                            (ys, zs) -> Right (read ys) : nonDigits zs
            nonDigits xs = case break isDigit xs of
                            ([], []) -> []
                            ([], zs) -> digits zs
                            (ys, zs) -> Left ys : digits zs

instance SOP.Generic IdeaSpace

-- | "Klasse".  (The school year is necessary as the class name is used for a fresh set of students
-- every school year.)
data SchoolClass = SchoolClass
    { _classSchoolYear :: Int -- ^ e.g. 2015
    , _className       :: ST  -- ^ e.g. "7a"
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- FIXME: SchoolClass shouldn't have an empty text, ever.  We avoid the distinction in some other
-- way, like with making 'Role' a parametric type.  (anyway, could we make this a pattern synonym?)
nilSchoolClass :: SchoolClass -> Bool
nilSchoolClass (SchoolClass _ "") = True
nilSchoolClass _                  = False

-- | FIXME: needs to be gone by the end of school year 2016!
theOnlySchoolYearHack :: Int
theOnlySchoolYearHack = 2016

schoolClass :: Int -> ST -> SchoolClass
schoolClass = SchoolClass

-- | A 'Topic' is created inside an 'IdeaSpace'.  It is used as a container for a "wild idea" that
-- has reached a quorum, plus more ideas that the moderator decides belong here.  'Topic's have
-- 'Phase's.  All 'Idea's in a 'Topic' must have the same 'IdeaSpace' as the 'Topic'.
data Topic = Topic
    { _topicMeta      :: MetaInfo Topic
    , _topicTitle     :: ST
    , _topicDesc      :: PlainDocument
    , _topicImage     :: URL
    , _topicIdeaSpace :: IdeaSpace
    , _topicPhase     :: Phase
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic Topic

data ProtoTopic = ProtoTopic
    { _protoTopicTitle       :: ST
    , _protoTopicDesc        :: PlainDocument
    , _protoTopicImage       :: URL
    , _protoTopicIdeaSpace   :: IdeaSpace
    , _protoTopicIdeas       :: [AUID Idea]
    , _protoTopicRefPhaseEnd :: Timestamp
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic ProtoTopic

type instance Proto Topic = ProtoTopic

-- Edit topic description and add ideas to topic.
data EditTopicData = EditTopicData
    { _editTopicTitle    :: ST
    , _editTopicDesc     :: PlainDocument
    , _editTopicAddIdeas :: [AUID Idea]
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic EditTopicData

data PhaseStatus
  = ActivePhase { _phaseEnd :: Timestamp }
  | FrozenPhase { _phaseLeftover :: Timespan }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic PhaseStatus

phaseLeftoverFrom :: Timestamp -> Lens' PhaseStatus Timespan
phaseLeftoverFrom now f = \case
    ActivePhase end      -> ActivePhase <$> fromNow now f end
    FrozenPhase leftover -> FrozenPhase <$> f leftover

-- | Topic phases.  (Phase 1.: "wild ideas", is where 'Topic's are born, and we don't need a
-- constructor for that here.)
data Phase =
    PhaseWildIdea   { _phaseWildFrozen :: Freeze }
  | PhaseRefinement { _phaseStatus :: PhaseStatus }
                               -- ^ 2. "Ausarbeitungsphase"
  | PhaseJury                  -- ^ 3. "Prüfungsphase"
  | PhaseVoting     { _phaseStatus :: PhaseStatus }
                               -- ^ 4. "Abstimmungsphase"
  | PhaseResult                -- ^ 5. "Ergebnisphase"
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic Phase

instance HasUILabel Phase where
    uilabel = \case
        PhaseWildIdea{}   -> "Wilde-Ideen-Phase"  -- FIXME: unreachable as of the writing of this
                                                  -- comment, but used for some tests
        PhaseRefinement{} -> "Ausarbeitungsphase"
        PhaseJury         -> "Prüfungsphase"
        PhaseVoting{}     -> "Abstimmungsphase"
        PhaseResult       -> "Ergebnisphase"

followsPhase :: Phase -> Phase -> Bool
followsPhase PhaseJury       (PhaseRefinement _) = True
followsPhase (PhaseVoting _) PhaseJury           = True
followsPhase PhaseResult     (PhaseVoting _)     = True
followsPhase _               _                   = False


-- * user

data UserSettings = UserSettings
    { _userSettingsPassword :: UserPass
    , _userSettingsEmail    :: Maybe EmailAddress
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic UserSettings

data User = User
    { _userMeta      :: MetaInfo User
    , _userLogin     :: UserLogin
    , _userFirstName :: UserFirstName
    , _userLastName  :: UserLastName
    , _userRoleSet   :: Set Role
    , _userDesc      :: Document
    , _userSettings  :: UserSettings
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic User

newtype UserLogin     = UserLogin     { _unUserLogin     :: ST }
  deriving (Eq, Ord, Show, Read, IsString, Monoid, Generic, FromHttpApiData)

newtype UserFirstName = UserFirstName { _unUserFirstName :: ST }
  deriving (Eq, Ord, Show, Read, IsString, Monoid, Generic, FromHttpApiData)

newtype UserLastName  = UserLastName  { _unUserLastName  :: ST }
  deriving (Eq, Ord, Show, Read, IsString, Monoid, Generic, FromHttpApiData)

type instance Proto User = ProtoUser

-- FIXME: Reduce the information stored in the 'DeleteUser' constructor.
data UserView
    = ActiveUser  { _activeUser  :: User }
    | DeletedUser { _deletedUser :: User }
  deriving (Eq, Ord, Show, Read, Generic)

data ProtoUser = ProtoUser
    { _protoUserLogin     :: Maybe UserLogin
    , _protoUserFirstName :: UserFirstName
    , _protoUserLastName  :: UserLastName
    , _protoUserRoleSet   :: Set Role
    , _protoUserPassword  :: InitialPassword
    , _protoUserEmail     :: Maybe EmailAddress
    , _protoUserDesc      :: Document
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic ProtoUser

-- | Note that all roles except 'Student' and 'ClassGuest' have the same access to all IdeaSpaces.
-- (Rationale: e.g. teachers have trust each other and can cover for each other.)
data Role =
    Student    { _roleSchoolClass :: SchoolClass }
  | ClassGuest { _roleSchoolClass :: SchoolClass } -- ^ e.g., parents
  | SchoolGuest  -- ^ e.g., researchers
  | Moderator
  | Principal
  | Admin
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic Role

guestRole :: IdeaSpace -> Role
guestRole = \case
    SchoolSpace  -> SchoolGuest
    ClassSpace c -> ClassGuest c

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

instance SOP.Generic RoleScope

instance Monoid RoleScope where
    mempty = ClassesScope mempty
    SchoolScope `mappend` _ = SchoolScope
    _ `mappend` SchoolScope = SchoolScope
    ClassesScope xs `mappend` ClassesScope ys = ClassesScope $ xs `mappend` ys

newtype InitialPassword = InitialPassword { _unInitialPassword :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic InitialPassword

newtype EncryptedPassword = ScryptEncryptedPassword { _unScryptEncryptedPassword :: SBS }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic EncryptedPassword

data UserPass =
    UserPassInitial   { _userPassInitial   :: InitialPassword }
  | UserPassEncrypted { _userPassEncrypted :: EncryptedPassword }
  | UserPassDeactivated
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic UserPass

verifyUserPass :: ST -> UserPass -> Bool
verifyUserPass pwd = \case
    UserPassInitial (InitialPassword p)           -> p == pwd
    UserPassEncrypted (ScryptEncryptedPassword p) -> verifyPass' (Pass (cs pwd)) (EncryptedPass p)
    UserPassDeactivated                           -> False

newtype EmailAddress = InternalEmailAddress { internalEmailAddress :: Email.EmailAddress }
    deriving (Eq, Ord, Show, Read, Generic)

instance CSV.FromField EmailAddress where
    parseField f = either fail (pure . InternalEmailAddress) . Email.validate =<< CSV.parseField f

instance Binary EmailAddress where
    put = put . Email.toByteString . internalEmailAddress
    get = maybe mzero (pure . InternalEmailAddress) . Email.emailAddress =<< get

instance SafeCopy EmailAddress where
    kind = base
    getCopy = contain $ maybe mzero (pure . InternalEmailAddress) . Email.emailAddress =<< safeGet
    putCopy = contain . safePut . Email.toByteString . internalEmailAddress

-- | "Beauftragung"
data Delegation = Delegation
    { _delegationScope :: DScope
    , _delegationFrom  :: AUID User
    , _delegationTo    :: AUID User
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic Delegation

type instance Proto Delegation = Delegation

-- | Node type for the delegation scope hierarchy DAG.  The four levels are 'Idea', 'Topic',
-- 'SchoolClass', and global.
--
-- There 'SchoolClass' level could reference an 'IdeaSpace' instead, but there is a subtle
-- difference between delegation in school space and globally that we would like to avoid having to
-- explain to our users, so we do not allow delegation in school space, and collapse 'school' and
-- 'global' liberally in the UI.  We enforce this collapse in this type.
--
-- Example to demonstrate the difference: If idea @A@ lives in class @C@, and user @X@ votes yes on
-- @A@, consider the two cases: If I delegate to user @X@ on school space level, @A@ is not covered,
-- because it lives in a different space, so user @X@ does *not* cast my vote.  If I delegate to
-- user @X@ *globally*, @A@ *is* covered, and @X@ *does* cast my vote.
--
-- The reason for this confusion is related to idea space membership, which is different for school:
-- every user is implicitly a member of the idea space "school", whereas membership in all other
-- idea spaces is explicit in the role.  However, this does not necessarily (although
-- coincidentally) constitute a subset relationship between class spaces and school space.
data DScope =
    DScopeGlobal
  | DScopeIdeaSpace { _dScopeIdeaSpace :: IdeaSpace  }
  | DScopeTopicId   { _dScopeTopicId   :: AUID Topic }
  | DScopeIdeaId    { _dScopeIdeaId    :: AUID Idea  }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic DScope

instance FromHttpApiData DScope where
    parseUrlPiece scope = case cs scope of
        "global" -> Right DScopeGlobal
        'i':'d':'e':'a':'s':'p':'a':'c':'e':'-':space -> DScopeIdeaSpace <$> parseUrlPiece (cs space)
        't':'o':'p':'i':'c':'-':topicId -> DScopeTopicId . AUID <$> readEitherCS topicId
        'i':'d':'e':'a':'-':ideaId -> DScopeIdeaId . AUID <$> readEitherCS ideaId
        _ -> Left "no parse"

instance ToHttpApiData DScope where
    toUrlPiece = (cs :: String -> ST). \case
        DScopeGlobal -> "global"
        (DScopeIdeaSpace space) -> "ideaspace-" <> cs (toUrlPiece space)
        (DScopeTopicId (AUID topicId)) -> "topic-" <> show topicId
        (DScopeIdeaId (AUID ideaId)) -> "idea-" <> show ideaId

-- | 'DScope', but with the references resolved.  (We could do a more general type @DScope a@ and
-- introduce two synonyms for @DScope AUID@ and @DScope Identity@, but it won't make things any
-- easier.)
data DScopeFull =
    DScopeGlobalFull
  | DScopeIdeaSpaceFull { _dScopeIdeaSpaceFull :: IdeaSpace }
  | DScopeTopicFull     { _dScopeTopicFull     :: Topic     }
  | DScopeIdeaFull      { _dScopeIdeaFull      :: Idea      }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic DScopeFull

data DelegationNetwork = DelegationNetwork
    { _networkUsers         :: [(User, Int)]  -- ^ 'User's and their 'votingPower's.
    , _networkDelegations   :: [Delegation]
    }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic DelegationNetwork

-- | Elaboration and Voting phase durations
-- FIXME: 'elaboration' and 'refinement' are the same thing.  pick one term!
-- ('elaboration' is my preference ~~fisx)
data Durations = Durations
    { _elaborationPhase :: DurationDays
    , _votingPhase      :: DurationDays
    }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic Durations

data Quorums = Quorums
    { _schoolQuorumPercentage :: Int
    , _classQuorumPercentage  :: Int -- (there is only one quorum for all classes, see gh#318)
    }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic Quorums

data Freeze = NotFrozen | Frozen
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance SOP.Generic Freeze

data Settings = Settings
    { _durations :: Durations
    , _quorums   :: Quorums
    , _freeze    :: Freeze
    }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic Settings

defaultSettings :: Settings
defaultSettings = Settings
    { _durations = Durations { _elaborationPhase = 21, _votingPhase = 21 }
    , _quorums   = Quorums   { _schoolQuorumPercentage = 30, _classQuorumPercentage = 30 }
    , _freeze    = NotFrozen
    }


-- * aula-specific helper types

-- | Aula Unique ID for reference in the database.  This is unique for one concrete phantom type
-- only and will probably be generated by sql `serial` type.
newtype AUID a = AUID { _unAUID :: Integer }
  deriving (Eq, Ord, Show, Read, Generic, FromHttpApiData, Enum, Real, Num, Integral)

instance SOP.Generic (AUID a)

type family   KeyOf a
type instance KeyOf User             = AUID User
type instance KeyOf Idea             = AUID Idea
type instance KeyOf Topic            = AUID Topic
type instance KeyOf Delegation       = AUID Delegation
type instance KeyOf Comment          = CommentKey
type instance KeyOf CommentVote      = CommentVoteKey
type instance KeyOf IdeaVote         = IdeaVoteLikeKey
type instance KeyOf IdeaLike         = IdeaVoteLikeKey
type instance KeyOf IdeaVoteResult   = AUID IdeaVoteResult
type instance KeyOf IdeaJuryResult   = AUID IdeaJuryResult

-- Extracts the identifier (AUID) from a key (KeyOf).
-- The identifier corresponds to the key of the last map (AMap).
--
-- For some types such as User, the key and the identifier are identical.
--
-- For a comment vote, the key is a composite of the comment key and the user id.
-- The identifier of a comment vote is only the user id part of the key.
--
-- So far all identifiers are of type AUID we shall try to keep it that way.
type family   IdOfKey a
type instance IdOfKey (AUID a)        = AUID a
type instance IdOfKey CommentKey      = AUID Comment
type instance IdOfKey CommentVoteKey  = AUID User
type instance IdOfKey IdeaVoteLikeKey = AUID User

type IdOf a = IdOfKey (KeyOf a)

type AMap a = Map (IdOf a) a

type Users        = AMap User
type Ideas        = AMap Idea
type Topics       = AMap Topic
type Comments     = AMap Comment
type CommentVotes = AMap CommentVote
type IdeaVotes    = AMap IdeaVote
type IdeaLikes    = AMap IdeaLike

instance HasUriPart (AUID a) where
    uriPart (AUID s) = fromString . show $ s

-- | General information on objects stored in the DB.
--
-- Some of these fields, like login name and avatar url of creator, are redundant.  The reason to
-- keep them here is that it makes it easy to keep large 'Page*' types containing many nested
-- objects, and still allowing all these objects to be rendered purely only based on the information
-- they contain.
--
-- If this is becoming too much in the future and we want to keep objects around without all this
-- inlined information, we should consider making objects polymorphic in the concrete meta info
-- type.  Example: 'Idea MetaInfo', but also 'Idea ShortMetaInfo'.
-- np@2016-04-18: Actually `Idea MetaInfo` does not work well. Parameters of kind `* -> *` are not
-- well supported by generics and deriving mechanisms.
data GMetaInfo a k = MetaInfo
    { _metaKey             :: k
    , _metaCreatedBy       :: AUID User
    , _metaCreatedByLogin  :: UserLogin
    , _metaCreatedAt       :: Timestamp
    , _metaChangedBy       :: AUID User
    , _metaChangedAt       :: Timestamp
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic id => SOP.Generic (GMetaInfo a id)

type MetaInfo a = GMetaInfo a (KeyOf a)

newtype PlainDocument = PlainDocument { unDescription :: ST }
  deriving (Eq, Ord, Show, Read, Generic, Monoid)

instance ToHtml PlainDocument where
    toHtmlRaw = div_ . toHtmlRaw . unDescription
    toHtml    = div_ . toHtml    . unDescription

-- | Transform values into strings suitable for presenting to the user.  These strings are not
-- machine-readable in general.  (alternative names that lost in a long bikeshedding session:
-- @HasUIString@, @HasUIText@, ...)
class HasUILabel a where
    uilabel :: a -> (Monoid s, IsString s) => s

    uilabelST :: a -> ST
    uilabelST = uilabel

    uilabeled :: (Monoid s, IsString s) => Getter a s
    uilabeled = to uilabel

    uilabeledST :: Getter a ST
    uilabeledST = to uilabel


-- * general-purpose types

-- | Use this for storing URLs in the aula state.  Unlike 'UriPath' is serializable, has equality,
-- and unlike "Frontend.Path", it is flexible enough to contain internal and external uris.
-- (FUTUREWORK: the `uri-bytestring` package could be nice here, but it may require a few orphans or
-- a newtype to prevent them; see also: #31.)
type URL = ST

newtype Timestamp = Timestamp { unTimestamp :: UTCTime }
  deriving (Eq, Ord, Generic)

instance Binary Timestamp where
    put = put . showTimestamp
    get = get >>= maybe mzero return . parseTimestamp

instance Show Timestamp where
    show = showTimestamp

instance Read Timestamp where
    readsPrec _ s = case splitAt timestampFormatLength $ dropWhile isSpace s of
        (parseTimestamp -> Just t, r) -> [(t, r)]
        _                             -> error $ "Read Timestamp: " <> show s

parseTimestamp :: String -> Maybe Timestamp
parseTimestamp = fmap Timestamp . parseTimeM True defaultTimeLocale timestampFormat

showTimestamp :: Timestamp -> String
showTimestamp = formatTime defaultTimeLocale timestampFormat . unTimestamp

timestampFormat :: String
timestampFormat = "%F_%T_%q"

timestampFormatLength :: Int
timestampFormatLength = length ("1864-04-13_13:01:33_846177415049" :: String)

data Timespan =  -- FIXME: import this from thentos?  create a package thentos-base?
    TimespanUs    Integer
  | TimespanMs    Integer
  | TimespanSecs  Integer
  | TimespanMins  Integer
  | TimespanHours Integer
  | TimespanDays  Integer
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic Timespan

showTimespan :: Timespan -> String
showTimespan (TimespanUs    i) = show i <> "us"
showTimespan (TimespanMs    i) = show i <> "ms"
showTimespan (TimespanSecs  i) = show i <> "s"
showTimespan (TimespanMins  i) = show i <> "m"
showTimespan (TimespanHours i) = show i <> "h"
showTimespan (TimespanDays  i) = show i <> "d"

timespanUs :: Timespan -> Int
timespanUs (TimespanUs    i) = fromIntegral   i
timespanUs (TimespanMs    i) = fromIntegral $ i * 1000
timespanUs (TimespanSecs  i) = fromIntegral $ i * (1000 * 1000)
timespanUs (TimespanMins  i) = fromIntegral $ i * (1000 * 1000 * 60)
timespanUs (TimespanHours i) = fromIntegral $ i * (1000 * 1000 * 3600)
timespanUs (TimespanDays  i) = fromIntegral $ i * (1000 * 1000 * 3600 * 24)

timespanDays :: Timespan -> Int
timespanDays = (`div` (1000 * 1000 * 3600 * 24)) . timespanUs

instance Aeson.FromJSON Timespan where
    parseJSON = Aeson.withText "Timespan value" $ \raw -> do
        let (digits, units) = ST.break (`notElem` ("-0123456789" :: String)) raw

            bad = fail $ "bad Timespan value: " <> cs (show raw)

            construct :: Monad m => ST -> (Integer -> Timespan) -> m Timespan
            construct i cns = pure . cns . read . cs $ i

        case (digits, units) of
            ("", _)   -> bad
            (i, "us") -> construct i TimespanUs
            (i, "ms") -> construct i TimespanMs
            (i, "s")  -> construct i TimespanSecs
            (i, "m")  -> construct i TimespanMins
            (i, "h")  -> construct i TimespanHours
            (i, "d")  -> construct i TimespanDays
            _         -> bad

instance Aeson.ToJSON Timespan where
    toJSON = \case
        (TimespanUs    i) -> render i "us"
        (TimespanMs    i) -> render i "ms"
        (TimespanSecs  i) -> render i "s"
        (TimespanMins  i) -> render i "m"
        (TimespanHours i) -> render i "h"
        (TimespanDays  i) -> render i "d"
      where
        render :: Integer -> String -> Aeson.Value
        render i unit = Aeson.String . cs $ show i <> unit

diffTimestamps :: Timestamp -> Timestamp -> Timespan
diffTimestamps (Timestamp tfrom) (Timestamp ttill) = TimespanUs .
    round $ (tfrom `diffUTCTime` ttill) * (1000 * 1000)

addTimespan :: Timespan -> Timestamp -> Timestamp
addTimespan tdiff (Timestamp tfrom) = Timestamp $
    fromRational (fromIntegral (timespanUs tdiff) / (1000 * 1000) :: Rational) `addUTCTime` tfrom

fromNow :: Timestamp -> Iso' Timestamp Timespan
fromNow now = iso (`diffTimestamps` now) (`addTimespan` now)

data PhaseChangeDir = Backward | Forward
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic)

instance SOP.Generic PhaseChangeDir

instance HasUILabel PhaseChangeDir where
    uilabel Forward  = "vorwärts"
    uilabel Backward = "zurück"

instance ToHtml PhaseChangeDir where
    toHtmlRaw = toHtml
    toHtml    = toHtml . uilabelST

-- | FIXME: should either go to the test suite or go away completely.
class Monad m => GenArbitrary m where
    genGen :: Gen a -> m a

-- | FIXME: should either go to the test suite or go away completely.
genArbitrary :: (GenArbitrary m, Arbitrary a) => m a
genArbitrary = genGen arbitrary

newtype PasswordToken = PasswordToken { unPasswordToken :: ST }
  deriving (Eq, Generic, Ord, Read, Show)

instance SOP.Generic PasswordToken

instance HasUriPart PasswordToken where
    uriPart = fromString . cs . unPasswordToken

-- FIXME: Smart constructor for tokens
instance FromHttpApiData PasswordToken where
    parseUrlPiece = Right . PasswordToken . cs

data PasswordTokenState
    = Invalid
    | TimedOut
    | Valid
  deriving (Eq, Generic, Ord, Read, Show)

instance SOP.Generic PasswordTokenState

-- * boilerplate: binary, lens (alpha order), SafeCopy

instance Binary (AUID a)
instance Binary Category
instance Binary Comment
instance Binary CommentKey
instance Binary CommentVote
instance Binary CommentVoteKey
instance Binary CommentContent
instance Binary EncryptedPassword
instance Binary PlainDocument
instance Binary Delegation
instance Binary DScope
instance Binary UserPass
instance Binary Role
instance Binary Idea
instance Binary IdeaLocation
instance Binary IdeaLike
instance Binary IdeaJuryResult
instance Binary IdeaJuryResultValue
instance Binary IdeaVoteResult
instance Binary IdeaVoteResultValue
instance Binary IdeaSpace
instance Binary IdeaVote
instance Binary IdeaVoteLikeKey
instance Binary IdeaVoteValue
instance Binary InitialPassword
instance Binary id => Binary (GMetaInfo a id)
instance Binary Phase
instance Binary PhaseStatus
instance Binary SchoolClass
instance Binary Timespan
instance Binary Topic
instance Binary UpDown
instance Binary User
instance Binary UserFirstName
instance Binary UserLastName
instance Binary UserLogin
instance Binary UserSettings
instance Binary DurationDays
instance Binary Durations
instance Binary Quorums
instance Binary Freeze
instance Binary Settings

makePrisms ''AUID
makePrisms ''Category
makePrisms ''PlainDocument
makePrisms ''DScope
makePrisms ''Document
makePrisms ''EmailAddress
makePrisms ''IdeaJuryResultValue
makePrisms ''IdeaLocation
makePrisms ''IdeaSpace
makePrisms ''IdeaVoteResultValue
makePrisms ''IdeaVoteValue
makePrisms ''Freeze
makePrisms ''PhaseStatus
makePrisms ''Phase
makePrisms ''Role
makePrisms ''RoleScope
makePrisms ''Timestamp
makePrisms ''UpDown
makePrisms ''UserFirstName
makePrisms ''UserLastName
makePrisms ''UserLogin
makePrisms ''UserPass
makePrisms ''UserView

makeLenses ''AUID
makeLenses ''Category
makeLenses ''Comment
makeLenses ''CommentContext
makeLenses ''CommentKey
makeLenses ''CommentVote
makeLenses ''CommentVoteKey
makeLenses ''EncryptedPassword
makeLenses ''PlainDocument
makeLenses ''Delegation
makeLenses ''DScope
makeLenses ''DelegationNetwork
makeLenses ''Durations
makeLenses ''EditTopicData
makeLenses ''EmailAddress
makeLenses ''Freeze
makeLenses ''GMetaInfo
makeLenses ''Idea
makeLenses ''IdeaJuryResult
makeLenses ''IdeaLike
makeLenses ''IdeaLocation
makeLenses ''IdeaSpace
makeLenses ''IdeaVote
makeLenses ''IdeaVoteLikeKey
makeLenses ''IdeaVoteResult
makeLenses ''InitialPassword
makeLenses ''Phase
makeLenses ''PhaseStatus
makeLenses ''ProtoIdea
makeLenses ''ProtoIdeaLike
makeLenses ''ProtoIdeaVote
makeLenses ''ProtoTopic
makeLenses ''ProtoUser
makeLenses ''Quorums
makeLenses ''Role
makeLenses ''RoleScope
makeLenses ''SchoolClass
makeLenses ''Settings
makeLenses ''Topic
makeLenses ''UpDown
makeLenses ''User
makeLenses ''UserFirstName
makeLenses ''UserLastName
makeLenses ''UserLogin
makeLenses ''UserPass
makeLenses ''UserSettings
makeLenses ''UserView

deriveSafeCopy 0 'base ''AUID
deriveSafeCopy 0 'base ''Category
deriveSafeCopy 0 'base ''Comment
deriveSafeCopy 0 'base ''CommentContent
deriveSafeCopy 0 'base ''CommentKey
deriveSafeCopy 0 'base ''CommentVote
deriveSafeCopy 0 'base ''CommentVoteKey
deriveSafeCopy 0 'base ''EncryptedPassword
deriveSafeCopy 0 'base ''PlainDocument
deriveSafeCopy 0 'base ''Delegation
deriveSafeCopy 0 'base ''DScope
deriveSafeCopy 0 'base ''DelegationNetwork
deriveSafeCopy 0 'base ''DurationDays
deriveSafeCopy 0 'base ''Durations
deriveSafeCopy 0 'base ''EditTopicData
deriveSafeCopy 0 'base ''Freeze
deriveSafeCopy 0 'base ''GMetaInfo
deriveSafeCopy 0 'base ''Idea
deriveSafeCopy 0 'base ''IdeaJuryResult
deriveSafeCopy 0 'base ''IdeaJuryResultValue
deriveSafeCopy 0 'base ''IdeaLike
deriveSafeCopy 0 'base ''IdeaLocation
deriveSafeCopy 0 'base ''IdeaSpace
deriveSafeCopy 0 'base ''IdeaVote
deriveSafeCopy 0 'base ''IdeaVoteLikeKey
deriveSafeCopy 0 'base ''IdeaVoteResult
deriveSafeCopy 0 'base ''IdeaVoteResultValue
deriveSafeCopy 0 'base ''IdeaVoteValue
deriveSafeCopy 0 'base ''InitialPassword
deriveSafeCopy 0 'base ''MoveIdea
deriveSafeCopy 0 'base ''PasswordToken
deriveSafeCopy 0 'base ''Phase
deriveSafeCopy 0 'base ''PhaseStatus
deriveSafeCopy 0 'base ''ProtoIdea
deriveSafeCopy 0 'base ''ProtoIdeaLike
deriveSafeCopy 0 'base ''ProtoIdeaVote
deriveSafeCopy 0 'base ''ProtoTopic
deriveSafeCopy 0 'base ''ProtoUser
deriveSafeCopy 0 'base ''Quorums
deriveSafeCopy 0 'base ''Role
deriveSafeCopy 0 'base ''SchoolClass
deriveSafeCopy 0 'base ''Settings
deriveSafeCopy 0 'base ''Timespan
deriveSafeCopy 0 'base ''Timestamp
deriveSafeCopy 0 'base ''Topic
deriveSafeCopy 0 'base ''UpDown
deriveSafeCopy 0 'base ''User
deriveSafeCopy 0 'base ''UserFirstName
deriveSafeCopy 0 'base ''UserLastName
deriveSafeCopy 0 'base ''UserLogin
deriveSafeCopy 0 'base ''UserPass
deriveSafeCopy 0 'base ''UserSettings

type AvatarDimension = Int

avatarFile :: Maybe AvatarDimension -> Getter (AUID a) FilePath
avatarFile mdim = to $ \uid -> "static" </> "avatars" </> cs (uriPart uid) <> sdim <.> "png"
  where
    sdim = mdim ^. _Just . showed . to ("-" <>)

{- See Frontend.Construct.avatarDefaultSize -}
avatarUrl :: AvatarDimension -> Getter (AUID a) URL
avatarUrl dim = to $ \uid -> "/" <> uid ^. avatarFile mdim . csi
  where
    mdim | dim == avatarDefaultSize = Nothing
         | otherwise                = Just dim

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

instance HasMetaInfo Idea where metaInfo = ideaMeta
instance HasMetaInfo IdeaJuryResult where metaInfo = ideaJuryResultMeta
instance HasMetaInfo IdeaVoteResult where metaInfo = ideaVoteResultMeta
instance HasMetaInfo Topic where metaInfo = topicMeta
instance HasMetaInfo User where metaInfo = userMeta
instance HasMetaInfo Comment where
    metaInfo = commentMeta
    idOfKey _ = ckCommentId
instance HasMetaInfo CommentVote where
    metaInfo = commentVoteMeta
    idOfKey _ = cvUser
instance HasMetaInfo IdeaVote where
    metaInfo = ideaVoteMeta
    idOfKey _ = ivUser
instance HasMetaInfo IdeaLike where
    metaInfo = ideaLikeMeta
    idOfKey _ = ivUser

{- Examples:
    e :: EmailAddress
    s :: ST
    s = emailAddress # e

    s :: ST
    s = "foo@example.com"
    e :: Maybe EmailAddress
    e = s ^? emailAddress


  These more limited type signatures are also valid:
    emailAddress :: Prism' ST  EmailAddress
    emailAddress :: Prism' LBS EmailAddress
-}
emailAddress :: (CSI s t SBS SBS) => Prism s t EmailAddress EmailAddress
emailAddress = csi . prism' Email.toByteString Email.emailAddress . from _InternalEmailAddress

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

notFeasibleIdea :: Idea -> Bool
notFeasibleIdea = has $ ideaJuryResult . _Just . ideaJuryResultValue . _NotFeasible

ideaHasCreatorStatement :: Idea -> Bool
ideaHasCreatorStatement = has $ ideaVoteResult . _Just . ideaVoteResultValue . _Winning . _Just

instance HasUriPart IdeaSpace where
    uriPart = fromString . ideaSpaceCode

instance HasUriPart SchoolClass where
    uriPart = fromString . schoolClassCode

instance HasUILabel IdeaSpace where
    uilabel = \case
        SchoolSpace    -> "Schule"
        (ClassSpace c) -> "Klasse " <> uilabel c

-- | for the first school year, we can ignore the year.  (after that, we have different options.
-- one would be to only show the year if it is not the current one, or always show it, or either
-- show "current" if applicable or the actual year if it lies in the past.)
instance HasUILabel SchoolClass where
    uilabel = fromString . cs . view className

ideaSpaceCode :: IdeaSpace -> String
ideaSpaceCode SchoolSpace    = "school"
ideaSpaceCode (ClassSpace c) = schoolClassCode c

schoolClassCode :: SchoolClass -> String
schoolClassCode c = show (c ^. classSchoolYear) <> "-" <> cs (c ^. className)

showIdeaSpaceCategory :: IsString s => IdeaSpace -> s
showIdeaSpaceCategory SchoolSpace    = "school"
showIdeaSpaceCategory (ClassSpace _) = "class"

parseIdeaSpaceCode :: (IsString err, Monoid err) => [ST] -> Either err IdeaSpace
parseIdeaSpaceCode = \case
    ["school"] -> pure SchoolSpace
    xs         -> ClassSpace <$> parseSchoolClassCode xs

parseSchoolClassCode :: (IsString err, Monoid err) => [ST] -> Either err SchoolClass
parseSchoolClassCode = \case
    [year, name] -> (`schoolClass` name) <$> readYear year
    _:_:_:_      -> err "Too many parts (two parts expected)"
    _            -> err "Too few parts (two parts expected)"
  where
    err msg = Left $ "Ill-formed school class: " <> msg
    readYear = maybe (err "Year should be only digits") Right . readMaybe . cs

parseRole :: (IsString err, Monoid err) => [ST] -> Either err Role
parseRole = \case
    ["admin"]      -> pure Admin
    ["principal"]  -> pure Principal
    ["moderator"]  -> pure Moderator
    ("student":xs) -> Student <$> parseSchoolClassCode xs
    ("guest":xs)   -> guestRole <$> parseIdeaSpaceCode xs
    _              -> Left "Ill-formed role"

instance FromHttpApiData Role where
    parseUrlPiece = parseRole . ST.splitOn "-"

instance FromHttpApiData IdeaSpace where
    parseUrlPiece = parseIdeaSpaceCode . ST.splitOn "-"

instance ToHttpApiData IdeaSpace where
    toUrlPiece = cs . ideaSpaceCode

instance FromHttpApiData SchoolClass where
    parseUrlPiece = parseSchoolClassCode . ST.splitOn "-"

instance FromHttpApiData IdeaVoteValue where
    parseUrlPiece = \case
        "yes"     -> Right Yes
        "no"      -> Right No
        _         -> Left "Ill-formed idea vote value: only `yes' or `no' are allowed"

instance HasUriPart IdeaVoteValue where
    uriPart = fromString . lowerFirst . show

instance FromHttpApiData IdeaJuryResultType where
    parseUrlPiece = \case
      "good" -> Right IdeaNotFeasible
      "bad"  -> Right IdeaFeasible
      _      -> Left "Ill-formed idea vote value: only `good' or `bad' are allowed"

instance ToHttpApiData IdeaJuryResultType where
    toUrlPiece = \case
      IdeaNotFeasible -> "good"
      IdeaFeasible    -> "bad"

instance HasUriPart IdeaJuryResultType where
    uriPart = fromString . cs . toUrlPiece

instance HasUriPart UpDown where
    uriPart = fromString . lowerFirst . show

instance FromHttpApiData UpDown where
    parseUrlPiece = \case
        "up"   -> Right Up
        "down" -> Right Down
        _      -> Left "Ill-formed comment vote value: only `up' or `down' are expected)"

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

ideaMaybeTopicId :: Lens' Idea (Maybe (AUID Topic))
ideaMaybeTopicId = ideaLocation . ideaLocationMaybeTopicId

isPhaseFrozen :: Phase -> Bool
isPhaseFrozen = has (phaseWildFrozen . _Frozen <> phaseStatus . _FrozenPhase . like ())

isFeasibleIdea :: Idea -> Bool
isFeasibleIdea = has $ ideaJuryResult . _Just . ideaJuryResultValue . _Feasible

isWinning :: Idea -> Bool
isWinning = has $ ideaVoteResult . _Just . ideaVoteResultValue . _Winning

isWild :: IdeaLocation -> Bool
isWild (IdeaLocationSpace _)   = True
isWild (IdeaLocationTopic _ _) = False

userVotedOnIdea :: User -> Idea -> Maybe IdeaVoteValue
userVotedOnIdea user idea =
    idea ^? ideaVotes . at (user ^. _Id) . _Just . ideaVoteValue

userLikesIdea :: User -> Idea -> Bool
userLikesIdea user idea =
    isJust $ idea ^? ideaLikes . at (user ^. _Id) . _Just

-- | Construct an 'IdeaLocation' from a 'Topic'
topicIdeaLocation :: Topic -> IdeaLocation
topicIdeaLocation = IdeaLocationTopic <$> (^. topicIdeaSpace) <*> (^. _Id)

instance HasUriPart Role where
    uriPart = \case
        (Student c)    -> "student-" <> uriPart c
        (ClassGuest c) -> "guest-" <> uriPart c
        SchoolGuest    -> "guest-school"
        Moderator      -> "moderator"
        Principal      -> "principal"
        Admin          -> "admin"

instance HasUILabel Role where
    uilabel = \case
        (Student c)
          | nilSchoolClass c -> "Schüler"
          | otherwise        -> "Schüler (" <> uilabel c <> ")"
        (ClassGuest c)
          | nilSchoolClass c -> "Gast"
          | otherwise        -> "Gast (" <> uilabel c <> ")"
        SchoolGuest    -> "Gast (Schule)"
        Moderator      -> "Moderator"
        Principal      -> "Direktor"
        Admin          -> "Administrator"

aMapFromList :: HasMetaInfo a => [a] -> AMap a
aMapFromList = Map.fromList . map (\x -> (x ^. _Id, x))

foldComment :: Fold Comment Comment
foldComment = cosmosOf (commentReplies . each)

foldComments :: Fold Comments Comment
foldComments = each . foldComment

commentsCount :: Getter Comments Int
commentsCount = to $ lengthOf foldComments

countEq :: (Foldable f, Eq value) => value -> Lens' vote value -> f vote -> Int
countEq v l = lengthOf $ folded . filtered ((== v) . view l)

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

userAvatar :: AvatarDimension -> Getter User URL
userAvatar dim = _Id . avatarUrl dim

userPassword :: Lens' User UserPass
userPassword = userSettings . userSettingsPassword

userEmail :: Lens' User (Maybe EmailAddress)
userEmail = userSettings . userSettingsEmail

fullDScopeToDScope :: DScopeFull -> DScope
fullDScopeToDScope = \case
    DScopeGlobalFull       -> DScopeGlobal
    DScopeIdeaSpaceFull is -> DScopeIdeaSpace is
    DScopeTopicFull t      -> DScopeTopicId (t ^. _Id)
    DScopeIdeaFull i       -> DScopeIdeaId (i ^. _Id)

instance HasUILabel DScopeFull where
    uilabel = \case
        DScopeGlobalFull       -> "Schule"
        DScopeIdeaSpaceFull is -> "Ideenraum " <> is ^. uilabeledST . csi . to fromString
        DScopeTopicFull t      -> "Thema "     <> t  ^. topicTitle  . csi . to fromString
        DScopeIdeaFull i       -> "Idee "      <> i  ^. ideaTitle   . csi . to fromString


instance Aeson.ToJSON (AUID a) where toJSON = Aeson.gtoJson
instance Aeson.ToJSON CommentKey where toJSON = Aeson.gtoJson
instance Aeson.ToJSON DScope where toJSON = Aeson.gtoJson
instance Aeson.ToJSON Delegation where toJSON = Aeson.gtoJson
instance Aeson.ToJSON EmailAddress where toJSON = Aeson.String . review emailAddress
instance Aeson.ToJSON Freeze where toJSON = Aeson.gtoJson
instance Aeson.ToJSON id => Aeson.ToJSON (GMetaInfo a id) where toJSON = Aeson.gtoJson
instance Aeson.ToJSON IdeaJuryResultType where toJSON = Aeson.gtoJson
instance Aeson.ToJSON IdeaLocation where toJSON = Aeson.gtoJson
instance Aeson.ToJSON IdeaSpace where toJSON = Aeson.gtoJson
instance Aeson.ToJSON IdeaVoteValue where toJSON = Aeson.gtoJson
instance Aeson.ToJSON Phase where toJSON = Aeson.gtoJson
instance Aeson.ToJSON PhaseStatus where toJSON = Aeson.gtoJson
instance Aeson.ToJSON Role where toJSON = Aeson.gtoJson
instance Aeson.ToJSON SchoolClass where toJSON = Aeson.gtoJson
instance Aeson.ToJSON Timestamp where toJSON = Aeson.gtoJson
instance Aeson.ToJSON UpDown where toJSON = Aeson.gtoJson
instance Aeson.ToJSON UserFirstName where toJSON = Aeson.gtoJson
instance Aeson.ToJSON UserLastName where toJSON = Aeson.gtoJson
instance Aeson.ToJSON UserLogin where toJSON = Aeson.gtoJson
instance Aeson.ToJSON UserPass where toJSON _ = Aeson.String ""  -- FIXME: where do we need this?  think of something else!
instance Aeson.ToJSON UserSettings where toJSON = Aeson.gtoJson
instance Aeson.ToJSON User where toJSON = Aeson.gtoJson


instance Aeson.ToJSON DelegationNetwork where
    toJSON (DelegationNetwork nodes links) = result
      where
        result = Aeson.object
            [ "nodes" Aeson..= array (renderNode <$> nodes)
            , "links" Aeson..= array (renderLink <$> links)
            ]

        -- FIXME: It shouldn't be rendered for deleted users.
        renderNode (u, p) = Aeson.object
            [ "name"   Aeson..= (u ^. userLogin . unUserLogin)
            , "avatar" Aeson..= (u ^. userAvatar avatarDefaultSize)
            , "power"  Aeson..= p
            ]

        renderLink (Delegation _ u1 u2) = Aeson.object
            [ "source"  Aeson..= nodeId u1
            , "target"  Aeson..= nodeId u2
            ]

        -- the d3 edges refer to nodes by list position, not name.  this function gives the list
        -- position.
        nodeId :: AUID User -> Aeson.Value
        nodeId uid = Aeson.toJSON . (\(Just pos) -> pos) $ Map.lookup uid m
          where
            m :: Map.Map (AUID User) Int
            m = Map.unions $ List.zipWith f nodes [0..]

            f :: (User, Int) -> Int -> Map.Map (AUID User) Int
            f (u, _) = Map.singleton (u ^. _Id)

        array :: Aeson.ToJSON v => [v] -> Aeson.Value
        array = Aeson.Array . Vector.fromList . fmap Aeson.toJSON


instance Aeson.FromJSON (AUID a) where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON CommentKey where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON DScope where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON Delegation where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON EmailAddress where parseJSON = Aeson.withText "email address" $ pure . (^?! emailAddress)
instance Aeson.FromJSON Freeze where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON id => Aeson.FromJSON (GMetaInfo a id) where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON IdeaJuryResultType where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON IdeaLocation where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON IdeaSpace where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON IdeaVoteValue where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON Phase where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON PhaseStatus where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON Role where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON SchoolClass where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON Timestamp where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON UpDown where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON UserFirstName where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON UserLastName where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON UserLogin where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON UserPass where parseJSON _ = pure . UserPassInitial $ InitialPassword ""  -- FIXME: where do we need this?  think of something else!
instance Aeson.FromJSON UserSettings where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON User where parseJSON = Aeson.gparseJson
