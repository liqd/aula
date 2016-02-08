{-# LANGUAGE RankNTypes #-}

{-# OPTIONS -Wall -Werror #-}

module Api
where

import Control.Lens (Lens', view)
import Data.Set (Set)

import qualified Data.Set as Set


countVotes :: (Eq value) => value -> Lens' vote value -> Set vote -> Int
countVotes v l = Set.size . Set.filter ((== v) . view l)
