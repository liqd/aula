{-# OPTIONS_GHC -Werror -Wall #-}

module Data.Graph.Missing
where

import Data.Graph (SCC(..))
import Data.Monoid ((<>))
import qualified Data.Set as Set


showSCC :: Show a => SCC a -> String
showSCC (AcyclicSCC vertex)   = show vertex
showSCC (CyclicSCC  vertices) = show vertices

-- | Add the nodes to the node list which only appear as leaf of some
-- other node.
fixLeaves :: Ord k => [(k,k,[k])] -> [(k,k,[k])]
fixLeaves nodes = nodes <> leaves
  where
    nubSet   = Set.toList . Set.fromList
    nodeKeys = nubSet $ _2 <$> nodes
    leaves   = makeLeaf <$> filter (`notElem` nodeKeys) (nubSet $ _3 =<< nodes)
    makeLeaf k = (k,k,[])
    _2 (_,s,_) = s
    _3 (_,_,t) = t
