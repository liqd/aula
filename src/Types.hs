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

-- | "Klasse".  (The school year is necessary as the class name is used for a fresh set of students
-- every school year.)
data SchoolClass = SchoolClass
    { _className       :: ST  -- ^ e.g. "7a"
    , _classSchoolYear :: ST  -- ^ e.g. "2015/16"
    }
  deriving (Eq, Ord, Show, Read, Generic)

data IdeaSpaceType =
    ISSchool
  | ISClass SchoolClass
  deriving (Eq, Ord, Show, Read, Generic)

data Topic = Topic
    { _topicMeta  :: MetaInfo Topic
    , _topicName  :: ST
    , _topicDesc  :: Document
    , _topicImage :: PNG
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- | "Ideenraum" is one of "Thema", "Klasse", "Schule".
data IdeaSpace = IdeaSpace
    { _ideaSpaceMeta      :: MetaInfo IdeaSpace
    , _ideaSpaceTitle     :: ST
    , _ideaSpaceType      :: IdeaSpaceType  -- ^ (FIXME: Can we do this on the type level?
                                            -- 'SchoolClass' can't be lifted, though.)
    , _ideaSpaceDesc      :: Document
    , _ideaSpaceWildIdeas :: Set Idea
    , _ideaSpaceTopics    :: Set Topic
    }
  deriving (Eq, Ord, Show, Read, Generic)


-- | Process phases are a property of 'Idea'.
--
-- FIXME: How do we express that FixFeasibility is over for a Topic, not just a few of its 'Ideas'?
-- The rule is that once all ideas have left FixFeasibility phase, the Topic leaves the phsae.  This
-- rule can be expressed in terms of the current data model ("only ideas have phases"), but there
-- may be a better way.
data Phase =
    PhaseWildIdeas       -- ^ "Wilde-Ideen-Sammlung"
  | PhaseEditTopics      -- ^ "Ausarbeitungsphase"
  | PhaseFixFeasibility  -- ^ "Prüfungsphase"
  | PhaseVote            -- ^ "Abstimmungsphase"
  | PhaseFinished        -- ^ "Ergebnisphase"
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

-- | "Idee"
data Idea = Idea
    { _ideaMeta       :: MetaInfo Idea
    , _ideaTitle      :: ST
    , _ideaDesc       :: Document
    , _ideaCategory   :: Category
    , _ideaPhase      :: Phase
    , _ideaTopic      :: Maybe Topic
    , _ideaComments   :: Set Comment
    , _ideaLikes      :: Set IdeaLike
    , _ideaVotes      :: Set IdeaVote
    , _ideaFeasible   :: Maybe Feasible
    }
  deriving (Eq, Ord, Show, Read, Generic)

data Feasible = Feasible
    { _feasibleMeta   :: MetaInfo Feasible
    , _feasibleValue  :: Bool
    , _feasibleReason :: Document
    }
  deriving (Eq, Ord, Show, Read, Generic)

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

-- | "Kategorie"
data Category =
    CatRule         -- ^ "Regel"
  | CatEquipment    -- ^ "Ausstattung"
  | CatClass        -- ^ "Unterricht"
  | CatTime         -- ^ "Zeit"
  | CatEnvironment  -- ^ "Umgebung"
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

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

data User = User
    { _userMeta      :: MetaInfo User
    , _userLogin     :: ST
    , _userFisstName :: ST
    , _userLastName  :: ST
    , _userAvatar    :: PNG
    , _userGroups    :: [Group]  -- ^ (could be a set)
    , _userPassword  :: EncryptedPass
    , _userEmail     :: Maybe Email
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- | Dummy for PNG images.  FIXME: use type from juicypixels?
type PNG = ()

data Group =
    Admin
  | Moderator
  | Principal
  | Student
  | Guest
  | InClass SchoolClass
  deriving (Eq, Ord, Show, Read, Generic)

newtype EncryptedPass = EncryptedPass { fromEncryptedPass :: SBS }
  deriving (Eq, Ord, Show, Read, Generic)

-- | FIXME: replace with structured email type.
newtype Email = Email ST
    deriving (Eq, Ord, Show, Read, ToField, CSV.FromField, Generic)

-- | "Beauftragung"
--
-- TODO: do we nee delegation by 'Idea'?  by 'ISTopic'?
data Delegation = Delegation
    { _delegationMeta      :: MetaInfo Delegation
--    , _delegationIdeaSpace :: AUID (IdeaSpace a)  -- FIXME
    , _delegationFrom      :: AUID User
    , _delegationTo        :: AUID User
    }
  deriving (Eq, Ord, Show, Read, Generic)

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

instance Binary Category
instance Binary Comment
instance Binary CommentVote
instance Binary Delegation
instance Binary Document
instance Binary Email
instance Binary EncryptedPass
instance Binary Feasible
instance Binary Group
instance Binary (AUID a)
instance Binary Idea
instance Binary IdeaSpace
instance Binary IdeaSpaceType
instance Binary IdeaVote
instance Binary IdeaVoteValue
instance Binary IdeaLike
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
makeLenses ''Document
makeLenses ''Email
makeLenses ''EncryptedPass
makeLenses ''Feasible
makeLenses ''Idea
makeLenses ''IdeaSpace
makeLenses ''IdeaSpaceType
makeLenses ''IdeaVote
makeLenses ''IdeaLike
makeLenses ''MetaInfo
makeLenses ''Phase
makeLenses ''SchoolClass
makeLenses ''Topic
makeLenses ''UpDown
makeLenses ''User
