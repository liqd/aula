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

data Topic = Topic Int
  deriving (Eq, Ord)

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
                (Voter, Topic) -- who delegates in which topic
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
      _votings :: Map (Voter, Topic) (Voter, Vote)
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

setDelegationPure :: Voter -> Topic -> Voter -> Delegations -> Delegations
setDelegationPure from topic to (Delegations (DelegationMap dmap) (CoDelegationMap coDmap))
    = Delegations dmap' coDmap'
  where
    dmap' = DelegationMap $ Map.insert (from, topic) to dmap
    coDmap' = CoDelegationMap $ case Map.lookup (from, topic) dmap of
        Nothing  -> Map.insert to (Set.singleton from) coDmap
        Just to' -> let coDmap0 = Map.update (deleteValue from) to coDmap
                        coDmap1 = case Map.lookup to' coDmap0 of
                                    Nothing -> Map.insert to' (Set.singleton from) coDmap0
                                    Just ds -> Map.insert to' (Set.insert from ds) coDmap0
                    in coDmap1
    deleteValue d ds = let ds' = Set.delete d ds in if Set.null ds' then Nothing else Just ds'

delegatedForTopic :: Voter -> Topic -> Voter -> DelegationMap -> Bool
delegatedForTopic from topic to (DelegationMap dmap) =
    maybe False (to ==) $ Map.lookup (from, topic) dmap

canVotePure :: Voter -> Topic -> DelegationMap -> Bool
canVotePure v t (DelegationMap dmap) = isNothing $ Map.lookup (v, t) dmap

getDelegatorsPure :: Voter -> Topic -> DelegationMap -> CoDelegationMap -> [Voter]
getDelegatorsPure v t dmap (CoDelegationMap codmap) = fix voters v
  where
    voters rec v =
        v : maybe [] (mconcat . map rec)
                (filter (\d -> delegatedForTopic d t v dmap) . Set.toList
                    <$> Map.lookup v codmap)

setVoteForPure :: Voter -> Topic -> Vote -> Voter -> Votings -> Votings
setVoteForPure voter topic vote delegator (Votings vmap) =
    Votings $ Map.insert (delegator, topic) (voter, vote) vmap


-- * monadic api

class Monad m => DelegationM m where
    setDelegation :: Voter -> Topic -> Voter -> m ()
    canVote       :: Voter -> Topic -> m Bool
    getDelegators :: Voter -> Topic -> m [Voter]
    voteFor       :: Voter -> Topic -> Vote -> Voter -> m ()

vote :: DelegationM m => Voter -> Topic -> Vote -> m ()
vote voter topic voteValue = do
    c <- canVote voter topic
    when c $ do
        voteFor voter topic voteValue voter
        getDelegators voter topic >>= mapM_ (voteFor voter topic voteValue)

-- * state monad implementation

newtype DelegationT m a = DelegationT { unDelegationT :: StateT DelegationState m a }
  deriving (Functor, Applicative, Monad, MonadState DelegationState)

instance Monad m => DelegationM (DelegationT m) where
    setDelegation f tp t = delegationsState %= (setDelegationPure f tp t)
    canVote v tp         = use $ delegationsState . delegations   . to (canVotePure v tp)

    getDelegators v t    = getDelegatorsPure v t
                            <$> use (delegationsState . delegations)
                            <*> use (delegationsState . coDelegations)

    voteFor v t x d      = votingsState %= (setVoteForPure v t x d)
