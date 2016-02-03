{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE KindSignatures              #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -fno-warn-orphans -Wall -Werror #-}

module Types
where

{-

>>> NOTES BEFORE THE PAD:


"Article" -> "Document".  make it markdownable.

"ideaSpaceArticle" => "ideaSpacePhase".

should "IdeaSpacePhase" be a field in "IdeaSpace"?  call it "Phase"!

should "IdeaSpaceType" be a type parameter?

ideaVotes is there, but ideaLikes missing.  type Like = (Bool, UserId).  think of a better name for Like.  Star?  Endorsement?  Interest?

ideaInfeasible :: Maybe (Bool, Document, MetaInfo?, ...?)

data Comment = Comment { ..., _commentReplies :: Set Comment, ... }

_voteValue: make ternary ADT, not 'Maybe Bool'

votes in comments can't be neutral.  that's yet another type.  IdeaVote, CommentVote.





questions for product owner:
 - ideas have a single author.  is that correct.
 - abuse button for ideas?
 - full names or nicks.


>>> NOTES FROM THE PAD:


data User = User
    { _userMeta           :: MetaInfo
    , _userLogin                   :: ST
    , _userFullName           :: (ST, ST)
    , _userAvatar  :: Image  -- dummy type.
    , _userGroups      :: [Group]
    , _userPassword       :: EncryptedPass
    , _userEmail          :: Maybe Email
    }
  deriving (Eq, Ord, Show, Read, Generic)

newtype EncryptedPass = EncryptedPass { fromEncryptedPass :: SBS }
  deriving (Eq, Ord, Show, Read, Generic)

newtype Email = Email ST -- TODO: replace by structured email type
    deriving (Eq, Ord, Show, Read, ToField, CSV.FromField, Generic- | Globally Unique ID (for reference in the database).  (FIXME: should we have different id types
-- for different object types?)
newtype GUID a = GUID Integer
data Group = Admin | Moderator | Principal | Student | Guest | InClass Class

data Class =
  { _className :: ST
   , _classYear :: Year
   }

[Student, InClass "7a"]


data MetaInfo = MetaInfo
                  { _metaId        :: GUID
                  , _metaCreatedBy :: GUID ////use  USER
                  , _metaCreatedAt :: Timestamp
                  , _metaChangedBy :: GUID
                  , _metaChangedAt :: Timestamp
                  }                              deriving (Eq, Ord, Show, Read, Generic)


-- | "Beauftragung"
data Delegation = Delegation
                  { _delegationMeta :: MetaInfo
                  , _delegationIdeaSpace :: GUID  -- FIXME: can you delegate for particular Idea?  how do we express that?  do we need delegation on topic?
                  , _delegationFrom :: GUID
                  , _delegationTo   :: GUID
                  }                              deriving (Eq, Ord, Show, Read, Generic)



topics are just tags.  does that work?  either way make topics separate data type and an idea should have a relation with a topic.

IdeaSpaceType lacks info about school class.


data IdeaSpaceType = Class Class | School

idea has a `Maybe Topic`.  Topic has name, description, image, meta.



data IdeaSpacePhase =
    PhaseWildIdeas       -- ^ "Wilde-Ideen-Sammlung"
  | PhaseEditTopics      -- ^ "Ausarbeitungsphase"
  | PhaseFixFeasibility  -- ^ "Prüfungsphase"
  | PhaseVote            -- ^ "Abstimmungsphase"
  | PhaseFinished        -- ^ "Ergebnisphase"
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)


- treat this per idea, not IdeaSpace.
- all more rigid phase transition rules can be implemented with that.
- example: FixFeasibility is over for topic once all ideas have left FixFeasibility phase.









-}

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


-- | Globally Unique ID (for reference in the database).  (FIXME: should we have different id types
-- for different object types?)
type GUID = Integer

data MetaInfo = MetaInfo
    { _metaId        :: GUID
    , _metaCreatedBy :: GUID
    , _metaCreatedAt :: Timestamp
    , _metaChangedBy :: GUID
    , _metaChangedAt :: Timestamp
    }
  deriving (Eq, Ord, Show, Read, Generic)

newtype Article = Article { fromArticle :: [ST] }
  deriving (Eq, Ord, Show, Read, Generic)

-- | "Ideenraum" is one of "Thema", "Klasse", "Schule".
data IdeaSpace (a :: IdeaSpaceType) = IdeaSpace
    { _ideaSpaceMeta      :: MetaInfo
    , _ideaSpaceTitle     :: ST
    , _ideaSpaceArticle   :: Article
    , _ideaSpacePhase     :: IdeaSpacePhase
    , _ideaSpaceWildIdeas :: Set Idea
    , _ideaSpaceTopics    :: Maybe (Set (IdeaSpace 'Topic))
    }
  deriving (Eq, Ord, Show, Read, Generic)

data IdeaSpaceType =
    Topic
  | Class
  | School
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

data IdeaSpacePhase =
    PhaseWildIdeas       -- ^ "Wilde-Ideen-Sammlung"
  | PhaseEditTopics      -- ^ "Ausarbeitungsphase"
  | PhaseFixFeasibility  -- ^ "Prüfungsphase"
  | PhaseVote            -- ^ "Abstimmungsphase"
  | PhaseFinished        -- ^ "Ergebnisphase"
  deriving (Eq, Ord, Bounded, Enum, Show, Read, Generic)

-- | "Idee"
data Idea = Idea
    { _ideaMeta       :: MetaInfo
    , _ideaTitle      :: ST
    , _ideaArticle    :: Article
    , _ideaCategory   :: Category
    , _ideaComments   :: Set Comment
    , _ideaVotes      :: Set Vote
    , _ideaInfeasible :: Maybe ST  -- ^ Reason for infisibility, if any.
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

-- | "Verbesserungsvorschlag"
data Comment = Comment
    { _commentMeta    :: MetaInfo
    , _commentArticle :: Article
    , _commentVotes   :: Set Vote
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- | "Stimme"
data Vote = Vote
    { _voteMeta  :: MetaInfo
    , _voteValue :: Maybe Bool
    }
  deriving (Eq, Ord, Show, Read, Generic)

data User = User
    { _userMeta           :: MetaInfo
    , _userName           :: ST
    , _userPassword       :: EncryptedPass
    , _userEmail          :: Maybe Email
    }
  deriving (Eq, Ord, Show, Read, Generic)

newtype EncryptedPass = EncryptedPass { fromEncryptedPass :: SBS }
  deriving (Eq, Ord, Show, Read, Generic)

newtype Email = Email ST -- TODO: replace by structured email type
    deriving (Eq, Ord, Show, Read, ToField, CSV.FromField, Generic)

-- | "Beauftragung"
data Delegation = Delegation
    { _delegationFrom :: GUID
    , _delegationTo   :: GUID
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

instance Binary MetaInfo
instance Binary Article
instance Binary (IdeaSpace a)
instance Binary IdeaSpaceType
instance Binary IdeaSpacePhase
instance Binary Idea
instance Binary Category
instance Binary Comment
instance Binary Vote
instance Binary User
instance Binary EncryptedPass
instance Binary Email
instance Binary Delegation

makeLenses ''MetaInfo
makeLenses ''Article
makeLenses ''IdeaSpace
makeLenses ''IdeaSpaceType
makeLenses ''IdeaSpacePhase
makeLenses ''Idea
makeLenses ''Category
makeLenses ''Comment
makeLenses ''Vote
makeLenses ''User
makeLenses ''EncryptedPass
makeLenses ''Email
makeLenses ''Delegation
