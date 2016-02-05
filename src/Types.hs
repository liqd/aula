{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE KindSignatures              #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-orphans -Wall -Werror #-}

module Types
where

import Control.Lens (makeLenses)
import Control.Monad
-- import Crypto.Scrypt (EncryptedPass)
import Data.Binary
import Data.Char
import Data.Set (Set)
import Data.String.Conversions
import Data.Time
import GHC.Generics

import Database.PostgreSQL.Simple.ToField (ToField)

import qualified Data.Csv as CSV


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

data IdeaVoteValue = IdeaVoteYes | IdeaVoteNo | IdeaVoteNeutral
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
-- idea spaces, topics, phases.

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
    , _topicIdeaSpace :: IdeaSpace
    , _topicPhase     :: Phase
    , _topicDesc      :: Document
    , _topicImage     :: PNG
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- | Topic phases.
data Phase =
    PhaseEditTopics      -- ^ "Ausarbeitungsphase"
  | PhaseFixFeasibility  -- ^ "Prüfungsphase"
  | PhaseVote            -- ^ "Abstimmungsphase"
  | PhaseFinished        -- ^ "Ergebnisphase"
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)


----------------------------------------------------------------------
-- user

data User = User
    { _userMeta      :: MetaInfo User
    , _userLogin     :: ST
    , _userFirstName :: ST
    , _userLastName  :: ST
    , _userAvatar    :: PNG
    , _userGroups    :: [Group]  -- ^ (could be a set)
    , _userPassword  :: EncryptedPass
    , _userEmail     :: Maybe Email
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- | Dummy for PNG images.  FIXME: use type from juicypixels?
type PNG = ()

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

newtype EncryptedPass = EncryptedPass { fromEncryptedPass :: SBS }
  deriving (Eq, Ord, Show, Read, Generic)

-- | FIXME: replace with structured email type.
newtype Email = Email ST
    deriving (Eq, Ord, Show, Read, ToField, CSV.FromField, Generic)

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
    { _metaId        :: AUID a
    , _metaCreatedBy :: AUID User
    , _metaCreatedAt :: Timestamp
    , _metaChangedBy :: AUID User
    , _metaChangedAt :: Timestamp
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- | Markdown content.
newtype Document = Markdown { fromMarkdown :: ST }
  deriving (Eq, Ord, Show, Read, Generic)


----------------------------------------------------------------------
-- general-purpose types

newtype Timestamp = Timestamp { fromTimestamp :: UTCTime }
  deriving (Eq, Ord, Generic)

instance Binary Timestamp where
    put (Timestamp t) = put $ show t
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
makeLenses ''IdeaLike
makeLenses ''IdeaSpace
makeLenses ''IdeaVote
makeLenses ''MetaInfo
makeLenses ''Phase
makeLenses ''SchoolClass
makeLenses ''Topic
makeLenses ''UpDown
makeLenses ''User
