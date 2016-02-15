{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE KindSignatures              #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Types
where

import Control.Lens (makeLenses, Lens')
import Control.Monad
import Data.Binary
import Data.Char
import Data.Set (Set)
import Data.String.Conversions
import Data.Time
import GHC.Generics
import Lucid

import qualified Database.PostgreSQL.Simple.ToField as PostgreSQL
import qualified Data.Csv as CSV

----------------------------------------------------------------------
-- prototypes for types

-- | Prototype for a type.
-- The prototypes contains all the information which cannot be
-- filled out of some type. Information which comes from outer
-- source and will be saved into the database.
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
    , _ideaSpace      :: IdeaSpace
    , _ideaTopic      :: Maybe Topic
    , _ideaComments   :: Set Comment
    , _ideaLikes      :: Set IdeaLike
    , _ideaQuorumOk   :: Bool  -- ^ number of likes / number of voters >= gobally configured quorum.
    , _ideaVotes      :: Set IdeaVote
    , _ideaFeasible   :: Maybe Feasible
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- | Prototype for Idea creation.
data ProtoIdea = ProtoIdea
    { _protoIdeaTitle      :: ST
    , _protoIdeaDesc       :: Document
    , _protoIdeaCategory   :: Category
    , _protoIdeaSpace      :: IdeaSpace
    }
  deriving (Eq, Ord, Show, Read, Generic)

type instance Proto Idea = ProtoIdea

-- | "Kategorie"
data Category =
    CatRule         -- ^ "Regel"
  | CatEquipment    -- ^ "Ausstattung"
  | CatClass        -- ^ "Unterricht"
  | CatTime         -- ^ "Zeit"
  | CatEnvironment  -- ^ "Umgebung"
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

-- | FIXME: Is there a better name for 'Like'?  'Star'?  'Endorsement'?  'Interest'?
data IdeaLike = IdeaLike
    { _likeMeta  :: MetaInfo IdeaLike
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- | "Stimme" for "Idee".  As opposed to 'CommentVote', which doesn't have neutral.
data IdeaVote = IdeaVote
    { _ideaVoteMeta  :: MetaInfo IdeaVote
    , _ideaVoteValue :: IdeaVoteValue
    }
  deriving (Eq, Ord, Show, Read, Generic)

data IdeaVoteValue = Yes | No | Neutral
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

data Feasible = Feasible
    { _feasibleMeta   :: MetaInfo Feasible
    , _feasibleValue  :: Bool
    , _feasibleReason :: Document
    }
  deriving (Eq, Ord, Show, Read, Generic)


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

-- | "Stimme" for "Verbesserungsvorschlag"
data CommentVote = CommentVote
    { _commentVoteMeta  :: MetaInfo CommentVote
    , _commentVoteValue :: UpDown
    }
  deriving (Eq, Ord, Show, Read, Generic)

data UpDown = Up | Down
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)


----------------------------------------------------------------------
-- idea space, topic, phase

-- | "Ideenraum" is one of "Klasse", "Schule".
data IdeaSpace =
    SchoolSpace
  | ClassSpace SchoolClass
  deriving (Eq, Ord, Show, Read, Generic)

-- | "Klasse".  (The school year is necessary as the class name is used for a fresh set of students
-- every school year.)
data SchoolClass = SchoolClass
    { _className       :: ST  -- ^ e.g. "7a"
    , _classSchoolYear :: ST  -- ^ e.g. "2015/16"
    }
  deriving (Eq, Ord, Show, Read, Generic)

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

-- | Topic phases.  (Phase 1.: "wild ideas", is where 'Topic's are born, and we don't need a
-- constructor for that here.)
data Phase =
    PhaseRefinement    -- ^ 2. "Ausarbeitungsphase"
  | PhaseJury          -- ^ 3. "Prüfungsphase"
  | PhaseVoting        -- ^ 4. "Abstimmungsphase"
  | PhaseResult        -- ^ 5. "Ergebnisphase"
  | PhaseFinished      -- ^ 6. "Ergebnisphase"
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)


----------------------------------------------------------------------
-- user

-- | FIXME: introduce newtypes 'UserLogin', 'UserFirstName', 'UserLastName'?
data User = User
    { _userMeta      :: MetaInfo User
    , _userLogin     :: ST
    , _userFirstName :: ST
    , _userLastName  :: ST
    , _userAvatar    :: URL
    , _userGroups    :: [Group]  -- ^ (could be a set)
    , _userPassword  :: EncryptedPass
    , _userEmail     :: Maybe Email
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- FIXME: Temporary hack to be able to save users.
type instance Proto User = User

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

-- | FIXME: import Crypto.Scrypt (EncryptedPass)
newtype EncryptedPass = EncryptedPass { fromEncryptedPass :: SBS }
  deriving (Eq, Ord, Show, Read, Generic)

-- | FIXME: replace with structured email type.
newtype Email = Email ST
    deriving (Eq, Ord, Show, Read, PostgreSQL.ToField, CSV.FromField, Generic)

-- | "Beauftragung"
data Delegation = Delegation
    { _delegationMeta    :: MetaInfo Delegation
    , _delegationContext :: DelegationContext
    , _delegationFrom    :: AUID User
    , _delegationTo      :: AUID User
    }
  deriving (Eq, Ord, Show, Read, Generic)

data DelegationContext =
    DelCtxIdeaSpace IdeaSpace
  | DelCtxTopic (AUID Topic)
  | DelCtxIdea (AUID Idea)
  deriving (Eq, Ord, Show, Read, Generic)


----------------------------------------------------------------------
-- aula-specific helper types

-- | Aula Unique ID for reference in the database.  This is unique for one concrete phantom type
-- only and will probably be generated by sql `serial` type.
newtype AUID a = AUID Integer
  deriving (Eq, Ord, Show, Read, Generic)

data MetaInfo a = MetaInfo
    { _metaId              :: AUID a
    , _metaCreatedBy       :: AUID User
    , _metaCreatedByLogin  :: ST
    , _metaCreatedByAvatar :: URL
    , _metaCreatedAt       :: Timestamp
    , _metaChangedBy       :: AUID User
    , _metaChangedAt       :: Timestamp
    }
  deriving (Eq, Ord, Show, Read, Generic)

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
        _                             -> error $ "Read Timestamp: " ++ show s

parseTimestamp :: String -> Maybe Timestamp
parseTimestamp = fmap Timestamp . parseTimeM True defaultTimeLocale timestampFormat

renderTimestamp :: Timestamp -> String
renderTimestamp = formatTime defaultTimeLocale timestampFormat . fromTimestamp

timestampFormat :: String
timestampFormat = "%F_%T_%q"

timestampFormatLength :: Int
timestampFormatLength = length ("1864-04-13_13:01:33_846177415049" :: String)


----------------------------------------------------------------------
-- boilerplate: binary, lens (alpha order)

instance Binary (AUID a)
instance Binary Category
instance Binary Comment
instance Binary CommentVote
instance Binary Delegation
instance Binary DelegationContext
instance Binary Document
instance Binary Email
instance Binary EncryptedPass
instance Binary Feasible
instance Binary Group
instance Binary Idea
instance Binary IdeaLike
instance Binary IdeaSpace
instance Binary IdeaVote
instance Binary IdeaVoteValue
instance Binary (MetaInfo a)
instance Binary Phase
instance Binary SchoolClass
instance Binary Topic
instance Binary UpDown
instance Binary User

makeLenses ''Category
makeLenses ''Comment
makeLenses ''CommentVote
makeLenses ''Delegation
makeLenses ''DelegationContext
makeLenses ''Document
makeLenses ''Email
makeLenses ''EncryptedPass
makeLenses ''Feasible
makeLenses ''Idea
makeLenses ''ProtoIdea
makeLenses ''IdeaLike
makeLenses ''IdeaSpace
makeLenses ''IdeaVote
makeLenses ''MetaInfo
makeLenses ''Phase
makeLenses ''SchoolClass
makeLenses ''Topic
makeLenses ''UpDown
makeLenses ''User

class HasMetaInfo a where
    metaInfo        :: Lens' a (MetaInfo a)
    _Id             :: Lens' a (AUID a)
    _Id             = metaInfo . metaId
    createdBy       :: Lens' a (AUID User)
    createdBy       = metaInfo . metaCreatedBy
    createdByLogin  :: Lens' a ST
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
instance HasMetaInfo Feasible where metaInfo = feasibleMeta
instance HasMetaInfo Idea where metaInfo = ideaMeta
instance HasMetaInfo IdeaLike where metaInfo = likeMeta
instance HasMetaInfo IdeaVote where metaInfo = ideaVoteMeta
instance HasMetaInfo Topic where metaInfo = topicMeta
instance HasMetaInfo User where metaInfo = userMeta
