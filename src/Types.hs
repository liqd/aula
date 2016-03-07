{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE KindSignatures              #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Types
    ( module Types
    , readMaybe)
where

import Control.Lens (makeLenses, Lens', (^.), (^?), _Just)
import Control.Monad
import Data.Binary
import Data.Char
import Data.Proxy (Proxy(Proxy))
import Data.Set (Set)
import Data.String
import Data.String.Conversions
import Data.Time
import Data.UriPath
import GHC.Generics
import Lucid
import Servant.API (FromHttpApiData(parseUrlPiece))
import Text.Read (readMaybe)

import qualified Data.Text as ST
import qualified Database.PostgreSQL.Simple.ToField as PostgreSQL
import qualified Data.Csv as CSV
import qualified Generics.SOP as SOP

import Test.QuickCheck (Arbitrary)

----------------------------------------------------------------------
-- a small prelude

-- | A shorter alias for 'mempty'.
nil :: Monoid a => a
nil = mempty

isNil :: (Monoid a, Eq a) => a -> Bool
isNil = (== nil)

readWith :: Read a => Proxy a -> String -> a
readWith Proxy = read

justIf :: a -> Bool -> Maybe a
justIf x b = if b then Just x else Nothing

newtype DurationDays = DurationDays { fromDurationDays :: Int }
  deriving (Eq, Ord, Show, Read, Num, Enum, Real, Integral)


----------------------------------------------------------------------
-- prototypes for types

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

----------------------------------------------------------------------
-- idea

-- | "Idee".  Ideas can be either be wild or contained in exactly one 'Topic'.
data Idea = Idea
    { _ideaMeta       :: MetaInfo Idea
    , _ideaTitle      :: ST
    , _ideaDesc       :: Document
    , _ideaCategory   :: Category
    , _ideaLocation   :: Either IdeaSpace (AUID Topic)
    , _ideaComments   :: Set Comment
    , _ideaLikes      :: Set IdeaLike
    , _ideaQuorumOk   :: Bool  -- ^ number of likes / number of voters >= gobally configured quorum.
    , _ideaVotes      :: Set IdeaVote
    , _ideaResult     :: Maybe IdeaResult
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic Idea

-- | Prototype for Idea creation.
data ProtoIdea = ProtoIdea
    { _protoIdeaTitle      :: ST
    , _protoIdeaDesc       :: Document
    , _protoIdeaCategory   :: Category
    , _protoIdeaIdeaSpace  :: IdeaSpace
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic ProtoIdea

type instance Proto Idea = ProtoIdea

-- | "Kategorie"
data Category =
    CatRule         -- ^ "Regel"
  | CatEquipment    -- ^ "Ausstattung"
  | CatClass        -- ^ "Unterricht"
  | CatTime         -- ^ "Zeit"
  | CatEnvironment  -- ^ "Umgebung"
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

instance SOP.Generic Category

-- | FIXME: Is there a better name for 'Like'?  'Star'?  'Endorsement'?  'Interest'?
data IdeaLike = IdeaLike
    { _likeMeta  :: MetaInfo IdeaLike
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaLike

-- | "Stimme" for "Idee".  As opposed to 'CommentVote', which doesn't have neutral.
data IdeaVote = IdeaVote
    { _ideaVoteMeta  :: MetaInfo IdeaVote
    , _ideaVoteValue :: IdeaVoteValue
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaVote

data IdeaVoteValue = Yes | No | Neutral
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

instance SOP.Generic IdeaVoteValue

data IdeaResult = IdeaResult
    { _ideaResultMeta   :: MetaInfo IdeaResult
    , _ideaResultValue  :: IdeaResultValue
    , _ideaResultReason :: Document
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaResult

data IdeaResultValue = NotFeasible | Winning | NotEnoughVotes
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

instance SOP.Generic IdeaResultValue

----------------------------------------------------------------------
-- comment

-- | "Verbesserungsvorschlag"
--
-- 'Comments' are hierarchical.  The application logic is responsible for putting some limit (if
-- any) on the recursion depth under which all children become siblings.
data Comment = Comment
    { _commentMeta    :: MetaInfo Comment
    , _commentText    :: Document
    , _commentVotes   :: Set CommentVote
    , _commentReplies :: Set Comment
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic Comment

-- | "Stimme" for "Verbesserungsvorschlag"
data CommentVote = CommentVote
    { _commentVoteMeta  :: MetaInfo CommentVote
    , _commentVoteValue :: UpDown
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic CommentVote

data UpDown = Up | Down
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance SOP.Generic UpDown

----------------------------------------------------------------------
-- idea space, topic, phase

-- | "Ideenraum" is one of "Klasse", "Schule".
data IdeaSpace =
    SchoolSpace
  | ClassSpace SchoolClass
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeaSpace

-- | "Klasse".  (The school year is necessary as the class name is used for a fresh set of students
-- every school year.)
data SchoolClass = SchoolClass
    { _classSchoolYear :: Int -- ^ e.g. 2015
    , _className       :: ST  -- ^ e.g. "7a"
    }
  deriving (Eq, Ord, Show, Read, Generic)

schoolClass :: Int -> ST -> SchoolClass
schoolClass = SchoolClass

-- | A 'Topic' is created inside an 'IdeaSpace'.  It is used as a container for a "wild idea" that
-- has reached a quorum, plus more ideas that the moderator decides belong here.  'Topic's have
-- 'Phase's.  All 'Idea's in a 'Topic' must have the same 'IdeaSpace' as the 'Topic'.
data Topic = Topic
    { _topicMeta      :: MetaInfo Topic
    , _topicTitle     :: ST
    , _topicDesc      :: Document
    , _topicImage     :: URL
    , _topicIdeaSpace :: IdeaSpace
    , _topicPhase     :: Phase
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic Topic

data ProtoTopic = ProtoTopic
    { _protoTopicTitle     :: ST
    , _protoTopicDesc      :: Document
    , _protoTopicImage     :: URL
    , _protoTopicIdeaSpace :: IdeaSpace
    , _protoTopicIdeas     :: [AUID Idea]
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic ProtoTopic

type instance Proto Topic = ProtoTopic

-- | Topic phases.  (Phase 1.: "wild ideas", is where 'Topic's are born, and we don't need a
-- constructor for that here.)
data Phase =
    PhaseRefinement    -- ^ 2. "Ausarbeitungsphase"
  | PhaseJury          -- ^ 3. "Prüfungsphase"
  | PhaseVoting        -- ^ 4. "Abstimmungsphase"
  | PhaseResult        -- ^ 5. "Ergebnisphase"
  | PhaseFinished      -- ^ 6. "Beendet"
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

instance SOP.Generic Phase

phaseName :: Phase -> ST
phaseName = \case
    PhaseRefinement -> "Ausarbeitungsphase"
    PhaseJury       -> "Prüfungsphase"
    PhaseVoting     -> "Abstimmungsphase"
    PhaseResult     -> "Ergebnisphase"
    PhaseFinished   -> "Beendet"


----------------------------------------------------------------------
-- user

-- | FIXME: introduce newtypes 'UserLogin', 'UserFirstName', 'UserLastName'?
data User = User
    { _userMeta      :: MetaInfo User
    , _userLogin     :: UserLogin
    , _userFirstName :: UserFirstName
    , _userLastName  :: UserLastName
    , _userAvatar    :: URL  -- FIXME UriPath?  Maybe?
    , _userGroups    :: [Group]  -- FIXME make a set.  rename group to role.
    , _userPassword  :: UserPass
    , _userEmail     :: Maybe UserEmail
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic User

newtype UserLogin     = UserLogin     { _fromUserLogin     :: ST }
  deriving (Eq, Ord, Show, Read, IsString, Monoid, Generic)

newtype UserFirstName = UserFirstName { _fromUserFirstName :: ST }
  deriving (Eq, Ord, Show, Read, IsString, Monoid, Generic)

newtype UserLastName  = UserLastName  { _fromUserLastName  :: ST }
  deriving (Eq, Ord, Show, Read, IsString, Monoid, Generic)

type instance Proto User = ProtoUser

data ProtoUser = ProtoUser
    { _protoUserLogin     :: Maybe UserLogin
    , _protoUserFirstName :: UserFirstName
    , _protoUserLastName  :: UserLastName
    , _protoUserGroups    :: [Group]
    , _protoUserPassword  :: Maybe UserPass
    , _protoUserEmail     :: Maybe UserEmail
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic ProtoUser

-- | Note that all groups except 'Student' and 'ClassGuest' have the same access to all IdeaSpaces.
-- (Rationale: e.g. teachres have trust each other and can cover for each other.)
data Group =
    Student SchoolClass
  | ClassGuest SchoolClass  -- ^ e.g., parents
  | SchoolGuest  -- ^ e.g., researchers
  | Moderator
  | Principal
  | Admin
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic Group

data UserPass =
    UserPassInitial   ST
  | UserPassEncrypted SBS  -- FIXME: use "Crypto.Scrypt.EncryptedPass"
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic UserPass

-- | FIXME: replace with structured email type.
newtype UserEmail = UserEmail { fromUserEmail :: ST }
    deriving (Eq, Ord, Show, Read, PostgreSQL.ToField, CSV.FromField, Generic)

-- | "Beauftragung"
data Delegation = Delegation
    { _delegationMeta    :: MetaInfo Delegation
    , _delegationContext :: DelegationContext
    , _delegationFrom    :: AUID User
    , _delegationTo      :: AUID User
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic Delegation

data DelegationContext =
    DelCtxIdeaSpace IdeaSpace
  | DelCtxTopic (AUID Topic)
  | DelCtxIdea (AUID Idea)
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic DelegationContext

----------------------------------------------------------------------
-- aula-specific helper types

-- | Aula Unique ID for reference in the database.  This is unique for one concrete phantom type
-- only and will probably be generated by sql `serial` type.
newtype AUID a = AUID Integer
  deriving (Eq, Ord, Show, Read, Generic, FromHttpApiData)

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
data MetaInfo a = MetaInfo
    { _metaId              :: AUID a
    , _metaCreatedBy       :: AUID User
    , _metaCreatedByLogin  :: UserLogin
    , _metaCreatedByAvatar :: URL
    , _metaCreatedAt       :: Timestamp
    , _metaChangedBy       :: AUID User
    , _metaChangedAt       :: Timestamp
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic a => SOP.Generic (MetaInfo a)

-- | Markdown content.
newtype Document = Markdown { fromMarkdown :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

instance ToHtml Document where
    toHtml    = div_ . p_ . toHtml    . fromMarkdown
    toHtmlRaw = div_ . p_ . toHtmlRaw . fromMarkdown


----------------------------------------------------------------------
-- general-purpose types

-- | Dummy for URL.  FIXME: use uri-bytestring?
type URL = ST

newtype Timestamp = Timestamp { fromTimestamp :: UTCTime }
  deriving (Eq, Ord, Generic)

instance Binary Timestamp where
    put = put . renderTimestamp
    get = get >>= maybe mzero return . parseTimestamp

instance Show Timestamp where
    show = show . renderTimestamp

instance Read Timestamp where
    readsPrec _ s = case splitAt (timestampFormatLength + 2) $ dropWhile isSpace s of
        (parseTimestamp . read -> Just t, r) -> [(t, r)]
        _                             -> error $ "Read Timestamp: " <> show s

parseTimestamp :: String -> Maybe Timestamp
parseTimestamp = fmap Timestamp . parseTimeM True defaultTimeLocale timestampFormat

renderTimestamp :: Timestamp -> String
renderTimestamp = formatTime defaultTimeLocale timestampFormat . fromTimestamp

timestampFormat :: String
timestampFormat = "%F_%T_%q"

timestampFormatLength :: Int
timestampFormatLength = length ("1864-04-13_13:01:33_846177415049" :: String)


-- | FIXME: should either go to the test suite or go away completely.
class Monad m => GenArbitrary m where
    genArbitrary :: Arbitrary a => m a


----------------------------------------------------------------------
-- admin pages

-- FIXME: rename this.  it's not just about permissions, but also about links and menu items.
-- (also, it's weird that we are not using PermissionContext as a capture in the routing table in
-- Frontend any more, but still in Frontend.Path to construct safe uris.  Not a real problem, but
-- perhaps we can resolve this assymetry somehow if we think about it more?)
data PermissionContext
    = PermUserView
    | PermUserCreate
    | PermClassView
    | PermClassCreate
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic PermissionContext

pContextToUriStr :: PermissionContext -> ST
pContextToUriStr PermUserView    = "perm-user-view"
pContextToUriStr PermUserCreate  = "perm-user-create"
pContextToUriStr PermClassView   = "perm-class-view"
pContextToUriStr PermClassCreate = "perm-class-create"

uriStrToPContext :: ST -> Maybe PermissionContext
uriStrToPContext "perm-user-view"    = Just PermUserView
uriStrToPContext "perm-user-create"  = Just PermUserCreate
uriStrToPContext "perm-class-view"   = Just PermClassView
uriStrToPContext "perm-class-create" = Just PermClassCreate
uriStrToPContext _                   = Nothing

instance FromHttpApiData PermissionContext where
    parseUrlPiece x =
        maybe (Left "No parse")
              Right
              $ uriStrToPContext (cs x)

instance HasUriPart PermissionContext where
    uriPart = uriPart . pContextToUriStr


----------------------------------------------------------------------
-- boilerplate: binary, lens (alpha order)

instance Binary (AUID a)
instance Binary Category
instance Binary Comment
instance Binary CommentVote
instance Binary Delegation
instance Binary DelegationContext
instance Binary Document
instance Binary UserPass
instance Binary UserEmail
instance Binary Group
instance Binary Idea
instance Binary IdeaLike
instance Binary IdeaResult
instance Binary IdeaResultValue
instance Binary IdeaSpace
instance Binary IdeaVote
instance Binary IdeaVoteValue
instance Binary (MetaInfo a)
instance Binary Phase
instance Binary SchoolClass
instance Binary Topic
instance Binary UpDown
instance Binary User
instance Binary UserLogin
instance Binary UserFirstName
instance Binary UserLastName

makeLenses ''Category
makeLenses ''Comment
makeLenses ''CommentVote
makeLenses ''Delegation
makeLenses ''DelegationContext
makeLenses ''Document
makeLenses ''UserPass
makeLenses ''UserEmail
makeLenses ''Idea
makeLenses ''IdeaLike
makeLenses ''IdeaResult
makeLenses ''IdeaSpace
makeLenses ''IdeaVote
makeLenses ''MetaInfo
makeLenses ''Phase
makeLenses ''ProtoIdea
makeLenses ''ProtoTopic
makeLenses ''SchoolClass
makeLenses ''Topic
makeLenses ''UpDown
makeLenses ''User
makeLenses ''UserLogin
makeLenses ''UserFirstName
makeLenses ''UserLastName
makeLenses ''ProtoUser

class HasMetaInfo a where
    metaInfo        :: Lens' a (MetaInfo a)
    _Id             :: Lens' a (AUID a)
    _Id             = metaInfo . metaId
    createdBy       :: Lens' a (AUID User)
    createdBy       = metaInfo . metaCreatedBy
    createdByLogin  :: Lens' a UserLogin
    createdByLogin  = metaInfo . metaCreatedByLogin
    createdByAvatar :: Lens' a URL
    createdByAvatar = metaInfo . metaCreatedByAvatar
    createdAt       :: Lens' a Timestamp
    createdAt       = metaInfo . metaCreatedAt
    changedBy       :: Lens' a (AUID User)
    changedBy       = metaInfo . metaChangedBy
    changedAt       :: Lens' a Timestamp
    changedAt       = metaInfo . metaChangedAt

instance HasMetaInfo CommentVote where metaInfo = commentVoteMeta
instance HasMetaInfo Delegation where metaInfo = delegationMeta
instance HasMetaInfo Idea where metaInfo = ideaMeta
instance HasMetaInfo IdeaLike where metaInfo = likeMeta
instance HasMetaInfo IdeaResult where metaInfo = ideaResultMeta
instance HasMetaInfo IdeaVote where metaInfo = ideaVoteMeta
instance HasMetaInfo Topic where metaInfo = topicMeta
instance HasMetaInfo User where metaInfo = userMeta

notFeasibleIdea :: Idea -> Bool
notFeasibleIdea idea = idea ^? ideaResult . _Just . ideaResultValue == Just NotFeasible

winningIdea :: Idea -> Bool
winningIdea idea = idea ^? ideaResult . _Just . ideaResultValue == Just Winning

instance HasUriPart IdeaSpace where
    uriPart = fromString . showIdeaSpace

showIdeaSpace :: IdeaSpace -> String
showIdeaSpace SchoolSpace    = "school"
showIdeaSpace (ClassSpace c) = show (c ^. classSchoolYear) <> "-" <> cs (c ^. className)

showIdeaSpaceCategory :: IsString s => IdeaSpace -> s
showIdeaSpaceCategory SchoolSpace    = "school"
showIdeaSpaceCategory (ClassSpace _) = "class"

parseIdeaSpace :: (IsString err, Monoid err) => ST -> Either err IdeaSpace
parseIdeaSpace s
    | s == "school" = Right SchoolSpace
    | otherwise     =
        case ST.splitOn "-" s of
            [year, name] -> (\y -> ClassSpace $ schoolClass y name) <$> readYear year
            _:_:_:_      -> err "Too many parts (two parts expected)"
            _            -> err "Too few parts (two parts expected)"
  where
    err msg = Left $ "Ill-formed idea space: " <> msg
    readYear = maybe (err "Year should be only digits") Right . readMaybe . cs

instance FromHttpApiData IdeaSpace where
    parseUrlPiece = parseIdeaSpace
