{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TemplateHaskell            #-}

module Data.Voting
where

import Prelude       hiding ((.))
import Data.Function hiding ((.))
import Control.Applicative hiding (empty)
import Control.Category
import Control.Lens
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

data VoterKind = VoterKind

data Voter = Voter Int
  deriving (Eq, Ord, Show)

data Vote = Yes | No


-- * category

data DCat v (a :: VoterKind) (b :: VoterKind)
    = DelegationCat v v
    | IdentityDelegation
    | NoDelegation

identity :: Voter -> DCat Voter 'VoterKind 'VoterKind
identity v = DelegationCat v v

delegate :: Voter -> Voter -> DCat Voter 'VoterKind 'VoterKind
delegate from to = DelegationCat from to

instance Eq v => Category (DCat v) where
    id = IdentityDelegation
    NoDelegation . x = NoDelegation
    x . NoDelegation = NoDelegation
    IdentityDelegation . (DelegationCat a b) = DelegationCat a b
    (DelegationCat a b) . IdentityDelegation = DelegationCat a b
    (DelegationCat b1 c) . (DelegationCat a b0)
      | b0 == b1  = DelegationCat a c
      | otherwise = NoDelegation

-- * pure model

data DelegationMap = DelegationMap {
        _delegationMap
            :: Map
                Voter -- who delegates
                Voter -- to whom delegates
    }

data CoDelegationMap = CoDelegationMap {
        _coDelegationMap
            :: Map
                Voter   -- who is delegated
                (Set Voter) -- by whom
    }

data Delegations = Delegations {
      _delegations   :: DelegationMap
    , _coDelegations :: CoDelegationMap
    }

data Votings = Votings {
      _votings :: Map Voter (Voter, Vote)
    }

data DelegationState = DelegationState {
      _delegationsState :: Delegations
    , _votingsState     :: Votings
    }

makeLenses ''DelegationMap
makeLenses ''CoDelegationMap
makeLenses ''Delegations
makeLenses ''Votings
makeLenses ''DelegationState

emptyDelegations = Delegations (DelegationMap Map.empty) (CoDelegationMap Map.empty)

setDelegationPure :: Voter -> Voter -> Delegations -> Delegations
setDelegationPure from to (Delegations (DelegationMap dmap) (CoDelegationMap coDmap))
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

canVotePure :: Voter -> DelegationMap -> Bool
canVotePure v (DelegationMap dmap) = isNothing $ Map.lookup v dmap

getDelegatorsPure :: Voter -> CoDelegationMap -> [Voter]
getDelegatorsPure v (CoDelegationMap dmap) = fix voters v
  where
    voters rec v = v : maybe [] (mconcat . map rec) (Set.toList <$> Map.lookup v dmap)

setVotePure :: Voter -> Vote -> Voter -> Votings -> Votings
setVotePure voter vote delegator (Votings vmap) =
    Votings $ Map.insert delegator (voter, vote) vmap


-- * monadic api

class Monad m => DelegationM m where
    setDelegation :: Voter -> Voter -> m ()
    canVote       :: Voter -> m Bool
    getDelegators :: Voter -> m [Voter]
    voteFor       :: Voter -> Vote -> Voter -> m ()

vote :: DelegationM m => Voter -> Vote -> m ()
vote voter voteValue = do
    c <- canVote voter
    when c $ do
        voteFor voter voteValue voter
        getDelegators voter >>= mapM_ (voteFor voter voteValue)

-- * state monad implementation

newtype DelegationT m a = DelegationT { unDelegationT :: StateT DelegationState m a }
  deriving (Functor, Applicative, Monad, MonadState DelegationState)

instance Monad m => DelegationM (DelegationT m) where
    setDelegation f t = delegationsState %= (setDelegationPure f t)
    canVote v         = use $ delegationsState . delegations   . to (canVotePure v)
    getDelegators v   = use $ delegationsState . coDelegations . to (getDelegatorsPure v)
    voteFor v x d     = votingsState %= (setVotePure v x d)
