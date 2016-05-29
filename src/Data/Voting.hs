{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TemplateHaskell            #-}

module Data.Voting
where

import Prelude       hiding ((.))
import Data.Function hiding ((.))
import Data.String.Conversions (ST)
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

data Idea = Idea Int
  deriving (Eq, Ord)

data Topic
    = TopicIdea Idea
    | TopicRef  ST
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
                Voter -- who delegates in which topic
                (Map Topic Voter) -- to whom delegates
    }

lookupDMap :: Voter -> Topic -> DelegationMap -> Maybe Voter
lookupDMap v t (DelegationMap dmap) = Map.lookup v dmap >>= Map.lookup t

insertDMap :: Voter -> Topic -> Voter -> DelegationMap -> DelegationMap
insertDMap f tp t (DelegationMap dmap) = DelegationMap $ case Map.lookup f dmap of
    Nothing -> Map.insert f (Map.singleton tp t) dmap
    Just dm -> Map.insert f (Map.insert tp t dm) dmap

candidates :: Voter -> DelegationMap -> Map Topic Voter
candidates v (DelegationMap dmap) = fromMaybe Map.empty $ Map.lookup v dmap

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
      _votings :: Map (Voter, Idea) (Voter, Vote)
    }

data TopicTree = TopicTree {
     _topicTree :: Map Topic Topic
    }

data DelegationState = DelegationState {
      _delegationsState :: Delegations
    , _votingsState     :: Votings
    , _topicTreeState   :: TopicTree
    }


makeLenses ''DelegationMap
makeLenses ''CoDelegationMap
makeLenses ''Delegations
makeLenses ''Votings
makeLenses ''TopicTree
makeLenses ''DelegationState

emptyDelegations = Delegations (DelegationMap Map.empty) (CoDelegationMap Map.empty)
emptyVotings     = Votings   Map.empty
emptyTopicTree   = TopicTree Map.empty

emptyDelegationState = DelegationState emptyDelegations emptyVotings emptyTopicTree

setDelegationPure :: Voter -> Topic -> Voter -> Delegations -> Delegations
setDelegationPure from topic to (Delegations dmap (CoDelegationMap coDmap))
    = Delegations dmap' coDmap'
  where
    dmap' = insertDMap from topic to dmap
    coDmap' = CoDelegationMap $ case lookupDMap from topic dmap of
        Nothing  -> Map.insert to (Set.singleton from) coDmap
        Just to' -> let coDmap0 = Map.update (deleteValue from) to coDmap
                        coDmap1 = case Map.lookup to' coDmap0 of
                                    Nothing -> Map.insert to' (Set.singleton from) coDmap0
                                    Just ds -> Map.insert to' (Set.insert from ds) coDmap0
                    in coDmap1
    deleteValue d ds = let ds' = Set.delete d ds in if Set.null ds' then Nothing else Just ds'


topicHiearchy :: Topic -> TopicTree -> [Topic]
topicHiearchy t (TopicTree tmap) = fix path t
  where
    path rec t = maybe [] ((t:) . rec) (Map.lookup t tmap)

canVotePure :: Voter -> Idea -> DelegationMap -> TopicTree -> Bool
canVotePure v i dmap ttree =
    all isNothing $ (\t -> lookupDMap v t dmap) <$> topicHiearchy (TopicIdea i) ttree

getDelegatorsPure :: Voter -> Idea -> DelegationMap -> CoDelegationMap -> TopicTree -> [Voter]
getDelegatorsPure v i dmap (CoDelegationMap codmap) ttree = fix voters v
  where
    -- idea -> class -> school
    topicPath  = topicHiearchy (TopicIdea i) ttree

    -- If the first voter in the candidate list is the `v`, `v` is
    -- responsible for the voting for the user u
    supporter u =
        let cm = candidates u dmap
        in case catMaybes $ map (\t -> Map.lookup t cm) topicPath of
            []     -> False
            v' : _ -> v' == v

    voters rec v =
        v : maybe [] (mconcat . map rec)
                     (filter supporter . Set.toList <$> Map.lookup v codmap)

setVoteForPure :: Voter -> Idea -> Vote -> Voter -> Votings -> Votings
setVoteForPure voter idea vote delegator (Votings vmap) =
    Votings $ Map.insert (delegator, idea) (voter, vote) vmap

setTopicDepPure :: Topic -> Topic -> TopicTree -> TopicTree
setTopicDepPure f t (TopicTree tmap) = TopicTree (Map.insert f t tmap)


-- * monadic api

class Monad m => DelegationM m where
    setDelegation :: Voter -> Topic -> Voter -> m ()
    canVote       :: Voter -> Idea  -> m Bool
    getDelegators :: Voter -> Idea  -> m [Voter]
    voteFor       :: Voter -> Idea  -> Vote -> Voter -> m ()
    setTopicDep   :: Topic -> Topic -> m ()

vote :: DelegationM m => Voter -> Idea -> Vote -> m ()
vote voter idea voteValue = do
    c <- canVote voter idea
    when c $ do
        voteFor voter idea voteValue voter
        getDelegators voter idea >>= mapM_ (voteFor voter idea voteValue)


-- * deep embedding

data DelegationDSL a where
    SetDelegation :: Voter -> Topic -> Voter         -> DelegationDSL ()
    CanVote       :: Voter -> Idea                   -> DelegationDSL Bool
    GetDelegators :: Voter -> Idea                   -> DelegationDSL [Voter]
    VoteFor       :: Voter -> Idea  -> Vote -> Voter -> DelegationDSL ()
    SetTopicDep   :: Topic -> Topic                  -> DelegationDSL ()

delegation :: (DelegationM m) => DelegationDSL a -> m a
delegation (SetDelegation f tp t) = setDelegation f tp t
delegation (CanVote v t)          = canVote v t
delegation (GetDelegators v i)    = getDelegators v i
delegation (VoteFor f tp x t)     = voteFor f tp x t
delegation (SetTopicDep f t)      = setTopicDep f t


-- * state monad implementation

newtype DelegationT m a = DelegationT { unDelegationT :: StateT DelegationState m a }
  deriving (Functor, Applicative, Monad, MonadState DelegationState)

instance Monad m => DelegationM (DelegationT m) where
    setDelegation f tp t = delegationsState %= (setDelegationPure f tp t)
    canVote v i          = canVotePure v i
                            <$> use (delegationsState . delegations)
                            <*> use topicTreeState

    getDelegators v t    = getDelegatorsPure v t
                            <$> use (delegationsState . delegations)
                            <*> use (delegationsState . coDelegations)
                            <*> use topicTreeState

    voteFor v t x d      = votingsState %= (setVoteForPure v t x d)

    setTopicDep f t      = topicTreeState %= setTopicDepPure f t
