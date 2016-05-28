{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module Data.Voting
where

import Prelude       hiding ((.))
import Data.Function hiding ((.))
import Control.Applicative hiding (empty)
import Control.Category

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

data VoterKind = VoterKind

data Voter = Voter Int
  deriving (Eq, Ord, Show)

data Vote = Yes | No

data Delegation v (a :: VoterKind) (b :: VoterKind)
    = Delegation v v
    | IdentityDelegation
    | NoDelegation

identity :: Voter -> Delegation Voter 'VoterKind 'VoterKind
identity v = Delegation v v

delegate :: Voter -> Voter -> Delegation Voter 'VoterKind 'VoterKind
delegate from to = Delegation from to

instance Eq v => Category (Delegation v) where
    id = IdentityDelegation
    NoDelegation . x = NoDelegation
    x . NoDelegation = NoDelegation
    IdentityDelegation . (Delegation a b) = Delegation a b
    (Delegation a b) . IdentityDelegation = Delegation a b
    (Delegation b1 c) . (Delegation a b0)
      | b0 == b1  = Delegation a c
      | otherwise = NoDelegation

data DelegationMap = DelegationMap {
        delegationMap
            :: Map
                Voter -- who delegates
                Voter -- to whom delegates
    }

data CoDelegationMap = CoDelegationMap {
        coDelegationMap
            :: Map
                Voter   -- who is delegated
                (Set Voter) -- by whom
    }

data Delegations = Delegations {
      delegations   :: DelegationMap
    , coDelegations :: CoDelegationMap
    }

emptyDelegations = Delegations (DelegationMap Map.empty) (CoDelegationMap Map.empty)

setDelegation :: Voter -> Voter -> Delegations -> Delegations
setDelegation from to (Delegations (DelegationMap dmap) (CoDelegationMap coDmap))
    = Delegations dmap' coDmap'
  where
    dmap' = DelegationMap $ Map.insert from to dmap
    coDmap' = CoDelegationMap $ case Map.lookup from dmap of
        Nothing  -> Map.insert to (Set.singleton from) coDmap
        Just to' -> let coDmap0 = Map.update (deleteValue from) to coDmap
                        coDmap1 = case Map.lookup to' coDmap0 of
                                    Nothing -> Map.insert to' (Set.singleton from) coDmap0
                                    Just ds -> Map.insert to' (Set.insert from ds) coDmap0
                    in coDmap1
    deleteValue d ds = let ds' = Set.delete d ds in if Set.null ds' then Nothing else Just ds'

canVote :: Voter -> DelegationMap -> Bool
canVote v (DelegationMap dmap) = isNothing $ Map.lookup v dmap

getDelegators :: Voter -> CoDelegationMap -> [Voter]
getDelegators v (CoDelegationMap dmap) = fix voters v
  where
    voters rec v = v : maybe [] (mconcat . map rec) (Set.toList <$> Map.lookup v dmap)

data Votings = Votings {
        votings :: Map Voter (Voter, Vote)
    }

setVote :: Voter -> Vote -> [Voter] -> Votings -> Votings
setVote voter vote delegators (Votings vmap) = Votings $ foldl setAVote vmap delegators
  where
    setAVote m d = Map.insert d (voter, vote) m

vote :: Voter -> Vote -> Delegations -> Votings -> Votings
vote voter vote (Delegations dmap coDmap) votings
  | canVote voter dmap = setVote voter vote (getDelegators voter coDmap) votings
  | otherwise          = votings
