{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Transaction where

import Control.Lens
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put, modify)
import Data.Acid
import Data.Monoid
import Data.SafeCopy
import Data.Set (Set)

import qualified Data.Set as Set

import Types

import qualified Config


data Db = Db
    { _dbIdeas :: Set Idea
    }
  deriving (Show)

makeLenses ''Db

instance Monoid Db where
  mempty = Db Set.empty
  Db s1 `mappend` Db s2 = Db $ s1 <> s2

$(deriveSafeCopy 0 'base ''Db)

addIdea :: Idea -> Update Db ()
addIdea idea = modify $ dbIdeas %~ (Set.insert idea)

viewIdeas :: Query Db (Set Idea)
viewIdeas = (^. dbIdeas) <$> ask

$(makeAcidic ''Db ['addIdea, 'viewIdeas])
