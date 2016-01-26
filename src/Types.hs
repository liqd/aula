{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TemplateHaskell  #-}

module Types
where

import Control.Lens (makeLenses)
import Data.SafeCopy
import Data.Set (Set)
import Data.String.Conversions
import Data.Time (UTCTime)
import GHC.Generics

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
    , _ideaSpacePhase     :: Phase
    , _ideaSpaceWildIdeas :: Set Idea
    , _ideaSpaceTopics    :: Set (IdeaSpace Topic)
    }
  deriving (Show, Eq, Ord, Generic)

data IdeaSpaceType =
    Topic
  | Class
  | School
  deriving (Show, Eq, Ord, Generic)

data IdeaSpacePhase =
    WildIdeas       -- ^ "Wilde-Ideen-Sammlung"
  | EditTopics      -- ^ "Ausarbeitungsphase"
  | FixFeasibility  -- ^ "Prüfungsphase"
  | Vote            -- ^ "Abstimmungsphase"
  | Finished        -- ^ "Ergebnisphase"
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
    , _userPassword       :: ...
    , _userEmail          :: ...
    , _userInDeletations  :: Set UserId
    , _userOutDeletations :: Set UserId
    }
  deriving (Show, Eq, Ord, Generic)

data UserId = UserId Int
  deriving (Show, Eq, Ord, Generic)


makeLenses ''MetaInfo
makeLenses ''IdeaSpace
makeLenses ''IdeaSpaceType
makeLenses ''Idea
makeLenses ''Category
makeLenses ''Comment
makeLenses ''Vote
makeLenses ''User
makeLenses ''UserId

$(deriveSafeCopy 0 'base ''MetaInfo)
$(deriveSafeCopy 0 'base ''IdeaSpace)
$(deriveSafeCopy 0 'base ''IdeaSpaceType)
$(deriveSafeCopy 0 'base ''Idea)
$(deriveSafeCopy 0 'base ''Category)
$(deriveSafeCopy 0 'base ''Comment)
$(deriveSafeCopy 0 'base ''Vote)
$(deriveSafeCopy 0 'base ''User)
$(deriveSafeCopy 0 'base ''UserId)
