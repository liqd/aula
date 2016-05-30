{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.Voting
where

import Prelude       hiding ((.))
import Data.Function hiding ((.))
import Data.String.Conversions (ST, cs)
import Control.Applicative hiding (empty)
import Control.Category
import Control.Lens hiding (pre)
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Debug.Trace

data VoterKind = VoterKind

data Voter = Voter Int
  deriving (Eq, Ord, Show)

data Vote = Yes | No
  deriving (Eq, Ord, Show)

data Idea = Idea Int
  deriving (Eq, Ord, Show)

data Topic
    = TopicIdea Idea
    | TopicRef  ST
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data Delegations = Delegations {
      _delegations   :: DelegationMap
    , _coDelegations :: CoDelegationMap
    }
  deriving (Eq, Show)

data Votings = Votings {
      _votings :: Map (Voter, Idea) (Voter, Vote)
    }
  deriving (Eq, Show)

data TopicTree = TopicTree {
     _topicTree :: Map Topic Topic
    }
  deriving (Eq, Show)

data DelegationState = DelegationState {
      _delegationsState :: Delegations
    , _votingsState     :: Votings
    , _topicTreeState   :: TopicTree
    }
  deriving (Eq, Show)


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

-- | Non empty list of topics, from leaf to root.
topicHiearchyPure :: Topic -> TopicTree -> [Topic]
topicHiearchyPure t (TopicTree tmap) = fix path t
  where
    path rec t = maybe [t] ((t:) . rec) (Map.lookup t tmap)

canVotePure :: Voter -> Idea -> DelegationMap -> TopicTree -> Bool
canVotePure v i dmap ttree =
    all isNothing $ (\t -> lookupDMap v t dmap) <$> topicHiearchyPure (TopicIdea i) ttree

getSupportersPure :: Voter -> Idea -> DelegationMap -> CoDelegationMap -> TopicTree -> [Voter]
getSupportersPure v i dmap (CoDelegationMap codmap) ttree = fix voters v
  where
    -- idea -> class -> school
    topicPath  = topicHiearchyPure (TopicIdea i) ttree

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
    getSupporters :: Voter -> Idea  -> m [Voter]
    voteFor       :: Voter -> Idea  -> Vote -> Voter -> m ()
    setTopicDep   :: Topic -> Topic -> m ()
    topicHiearchy :: Topic -> m [Topic] 

vote :: DelegationM m => Voter -> Idea -> Vote -> m ()
vote voter idea voteValue = do
    c <- canVote voter idea
    when c $ do
        voteFor voter idea voteValue voter
        getSupporters voter idea >>= mapM_ (voteFor voter idea voteValue)


-- * deep embedding

data DelegationDSL a where
    SetDelegation :: Voter -> Topic -> Voter         -> DelegationDSL ()
    CanVote       :: Voter -> Idea                   -> DelegationDSL Bool
    GetSupporters :: Voter -> Idea                   -> DelegationDSL [Voter]
    VoteFor       :: Voter -> Idea  -> Vote -> Voter -> DelegationDSL ()
    SetTopicDep   :: Topic -> Topic                  -> DelegationDSL ()
    TopicHiearchy :: Topic                           -> DelegationDSL [Topic]

delegation :: (DelegationM m) => DelegationDSL a -> m a
delegation (SetDelegation f tp t) = setDelegation f tp t
delegation (CanVote v t)          = canVote v t
delegation (GetSupporters v i)    = getSupporters v i
delegation (VoteFor f tp x t)     = voteFor f tp x t
delegation (SetTopicDep f t)      = setTopicDep f t
delegation (TopicHiearchy t)      = topicHiearchy t
{-
instance Arbitrary (DelegationDSL a) where
    arbitrary = elements
        [ SetDelegation <$> arbitrary <*> arbitrary <*> arbitrary
        , SetTopicDep <$> arbitrary
        ]
-}
-- * state monad implementation

newtype DelegationT m a = DelegationT { unDelegationT :: StateT DelegationState m a }
  deriving (Functor, Applicative, Monad, MonadState DelegationState)

runDelegation :: DelegationT Identity a -> (a, DelegationState)
runDelegation d = runIdentity $ runStateT (unDelegationT d) emptyDelegationState

runDelegation' :: DelegationT Identity a -> a
runDelegation' d = runIdentity $ evalStateT (unDelegationT d) emptyDelegationState


instance Monad m => DelegationM (DelegationT m) where
    setDelegation f tp t = delegationsState %= (setDelegationPure f tp t)
    canVote v i          = canVotePure v i
                            <$> use (delegationsState . delegations)
                            <*> use topicTreeState

    getSupporters v t    = getSupportersPure v t
                            <$> use (delegationsState . delegations)
                            <*> use (delegationsState . coDelegations)
                            <*> use topicTreeState

    voteFor v t x d      = votingsState %= (setVoteForPure v t x d)

    setTopicDep f t      = topicTreeState %= setTopicDepPure f t

    topicHiearchy t      = topicHiearchyPure t <$> use (topicTreeState)

-- * testing

instance Arbitrary Voter where
    arbitrary = Voter <$> arbitrary
    shrink (Voter x) = Voter <$> shrink x

instance Arbitrary Idea where
    arbitrary = Idea <$> arbitrary
    shrink (Idea x) = Idea <$> shrink x

-- | Only generates TopicRefs
instance Arbitrary Topic where
     arbitrary = TopicRef . (cs :: String -> ST) <$> arbitrary
     shrink (TopicRef x) = TopicRef . cs <$> shrink (cs x :: String)

setDelegationProp1Pre :: Monad m => Voter -> Idea -> Voter -> DelegationT m Bool
setDelegationProp1Pre f i t = not . elem f <$> getSupporters t i

-- | No delegation between the voters for a given idea.
setDelegationProp1 :: Monad m => Voter -> Idea -> Voter -> DelegationT m Bool
setDelegationProp1 f i t = do
    setDelegation f (TopicIdea i) t
    elem f <$> getSupporters t i

setDelegationProp2Pre :: Monad m => Voter -> Idea -> Voter -> DelegationT m Bool
setDelegationProp2Pre f i t = elem f <$> getSupporters t i

-- | The number of supporters does not change if the delegates again in the same idea
setDelegationProp2 :: Monad m => Voter -> Idea -> Voter -> DelegationT m Bool
setDelegationProp2 f i t = do
    supporters <- getSupporters t i
    setDelegation f (TopicIdea i) t
    (supporters ==) <$> getSupporters t i

--setTopicDepProp1Pre :: Monad m => Idea -> Topic -> DelegationT m Bool
--setTopicDepProp1Pre i t = do

setTopicDepProp1Pre :: Monad m => Idea -> Topic -> DelegationT m Bool
setTopicDepProp1Pre i t = do
    let ti = TopicIdea i
    ([ti]==) <$> topicHiearchy ti

-- | Assign an idea to a topic, the idea is not assigned
setTopicDepProp1 :: Monad m => Idea -> Topic -> DelegationT m Bool
setTopicDepProp1 i t = do
    let ti = TopicIdea i
    hiearchy <- topicHiearchy t
    setTopicDep ti t
    (ti:hiearchy ==) <$> topicHiearchy ti

drawSeparator = putStrLn "*******************"

quickCheckDelegation p = do
    quickCheck $ monadic runDelegation' p
    drawSeparator

runTC comp = do
    print $ runDelegation comp
    drawSeparator

main = do
    let (f,i,t) = ((Voter 1), (Idea 1), (Voter 2))
        tp = TopicRef "tp"
    runTC $ do
        setDelegationProp1 f i t

    quickCheckDelegation $ do
        f <- pick arbitrary
        i <- pick arbitrary
        t <- pick arbitrary
        run (setDelegationProp1Pre f i t) >>= pre
        run (setDelegationProp1 f i t) >>= assert

    runTC $ do
        setDelegation f (TopicIdea i) t
        setDelegationProp2 f i t

    quickCheckDelegation $ do
        f <- pick arbitrary
        i <- pick arbitrary
        t <- pick arbitrary
        pre (t /= f)
        run (setDelegation f (TopicIdea i) t)
        run (setDelegationProp2Pre f i t) >>= pre
        run (setDelegationProp2 f i t) >>= assert

    runTC $ do
        setTopicDepProp1 i tp

    quickCheckDelegation $ do
        i <- pick arbitrary
        t <- pick arbitrary
        run (setTopicDepProp1Pre i t) >>= pre
        run (setTopicDepProp1 i t) >>= assert
