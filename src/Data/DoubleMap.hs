{-# OPTIONS_GHC -Wall -Werror    #-}

module Data.DoubleMap
where

import Control.Lens
import Data.Map as Map

lookup :: (Ord k1, Ord k2) => k1 -> k2 -> Map k1 (Map k2 v) -> Maybe v
lookup k1 k2 m = Map.lookup k1 m >>= Map.lookup k2

insert :: (Ord k1, Ord k2) => k1 -> k2 -> v -> Map k1 (Map k2 v) -> Map k1 (Map k2 v)
insert k1 k2 v m = case Map.lookup k1 m of
    Nothing -> Map.insert k1 (Map.singleton k2 v) m
    Just m' -> Map.insert k1 (Map.insert k2 v m') m

remove :: (Ord k1, Ord k2) => k1 -> k2 -> Map k1 (Map k2 v) -> Map k1 (Map k2 v)
remove k1 k2 m = m & at k1 . _Just . at k2 .~ Nothing
                   & at k1 %~ deleteEmpty


deleteEmpty :: Foldable t => Maybe (t a) -> Maybe (t a)
deleteEmpty (Just s) | Prelude.null s = Nothing
deleteEmpty x                         = x
