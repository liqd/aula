{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

module Types
where

import Control.Lens (makeLenses)
-- import Crypto.Scrypt (EncryptedPass)
import Data.SafeCopy
import Data.Set (Set)
import Data.String.Conversions
import Data.Time (UTCTime)
import GHC.Generics

import Database.PostgreSQL.Simple.ToField (ToField)

import qualified Data.Csv as CSV
import qualified Data.Text as ST


data MetaInfo = MetaInfo
    { _metaCreatedBy :: UserId
    , _metaCreatedAt :: UTCTime
    , _metaChangedBy :: UserId
    , _metaChangedAt :: UTCTime
    }
  deriving (Show, Eq, Ord, Generic)

-- | "Ideenraum" is one of "Thema", "Klasse", "Schule".
data IdeaSpace = IdeaSpace
    { _ideaSpaceType      :: IdeaSpaceType
    , _ideaSpaceMeta      :: MetaInfo
    , _ideaSpaceTitle     :: ST
    , _ideaSpaceText      :: ST
    , _ideaSpacePhase     :: IdeaSpacePhase
    , _ideaSpaceWildIdeas :: Set Idea
    -- , _ideaSpaceTopics    :: Set (IdeaSpace Topic)
    }
  deriving (Show, Eq, Ord, Generic)

data IdeaSpaceType =
    Topic
  | Class
  | School
  deriving (Show, Eq, Ord, Generic)

data IdeaSpacePhase =
    PhaseWildIdeas       -- ^ "Wilde-Ideen-Sammlung"
  | PhaseEditTopics      -- ^ "Ausarbeitungsphase"
  | PhaseFixFeasibility  -- ^ "Prüfungsphase"
  | PhaseVote            -- ^ "Abstimmungsphase"
  | PhaseFinished        -- ^ "Ergebnisphase"
  deriving (Show, Eq, Ord, Generic)

-- | "Idee".
data Idea = Idea
    { _ideaMeta       :: MetaInfo
    , _ideaSpace      :: IdeaSpace
    , _ideaTitle      :: ST
    , _ideaText       :: ST
    , _ideaCategory   :: Category
    , _ideaComments   :: Set Comment
    , _ideaVotes      :: Set Vote
    , _ideaInfeasible :: Maybe ST  -- ^ Reason for infisibility, if any.
    }
  deriving (Show, Eq, Ord, Generic)

-- | "Kategorie"
data Category =
    CatRule         -- ^ "Regel"
  | CatEquipment    -- ^ "Ausstattung"
  | CatClass        -- ^ "Unterricht"
  | CatTime         -- ^ "Zeit"
  | CatEnvironment  -- ^ "Umgebung"
  deriving (Show, Eq, Ord, Generic)

-- | "Verbesserungsvorschlag"
data Comment = Comment
    { _commentMeta  :: MetaInfo
    , _commentText  :: ST
    , _commentVotes :: Set Vote
    }
  deriving (Show, Eq, Ord, Generic)

-- | "Stimme"
data Vote = Vote
    { _voteMeta  :: MetaInfo
    , _voteValue :: Maybe Bool
    }
  deriving (Show, Eq, Ord, Generic)

data User = User
    { _userMeta           :: MetaInfo
    , _userId             :: UserId
    , _userName           :: ST
    , _userPassword       :: EncryptedPass
    , _userEmail          :: Maybe Email
    -- should probably not be included in user type but looked up as needed
    -- , _userInDelegations  :: Set UserId
    -- , _userOutDelegations :: Set UserId
    }
  deriving (Generic)

data UserId = UserId Int
  deriving (Show, Eq, Ord, Generic)

newtype EncryptedPass = EncrypedPass SBS

newtype Email = Email ST -- TODO: replace by structured email type
    deriving (Eq, Ord, Show, ToField, CSV.FromField)

makeLenses ''MetaInfo
makeLenses ''IdeaSpace
makeLenses ''IdeaSpaceType
makeLenses ''Idea
makeLenses ''Category
makeLenses ''Comment
makeLenses ''Vote
makeLenses ''User
makeLenses ''UserId
