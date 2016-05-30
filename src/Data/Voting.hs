{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# LANGUAGE BangPatterns               #-}

module Data.Voting
where

import Prelude       hiding ((.))
import Data.Function hiding ((.))
import Data.String.Conversions (ST, cs)
import Control.Applicative hiding (empty)
import Control.Category
import Control.Lens hiding (elements, pre)
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
        -- There were no deligation in the given topic
        Nothing  -> Map.insert to (Set.singleton from) coDmap
        -- There was a deligation in the given topic
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

getSupportersPure :: Voter -> Topic -> DelegationMap -> CoDelegationMap -> TopicTree -> [Voter]
getSupportersPure v t dmap (CoDelegationMap codmap) ttree = fix voters v
  where
    -- idea -> class -> school
    topicPath  = topicHiearchyPure t ttree

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
setTopicDepPure f t tm@(TopicTree tmap) =
    case Map.lookup t tmap of
        Just f' | f == f' -> tm -- Avoid circular deps should throw an error
        _                 -> TopicTree (Map.insert f t tmap)

getVotePure :: Voter -> Idea -> Votings -> Maybe (Voter, Vote)
getVotePure v i (Votings vmap) = Map.lookup (v,i) vmap

-- * monadic api

class Monad m => DelegationM m where
    setDelegation :: Voter -> Topic -> Voter -> m ()
    canVote       :: Voter -> Idea  -> m Bool
    getSupporters :: Voter -> Topic -> m [Voter]
    voteFor       :: Voter -> Idea  -> Vote -> Voter -> m ()
    setTopicDep   :: Topic -> Topic -> m ()
    topicHiearchy :: Topic -> m [Topic]
    getVote       :: Voter -> Idea -> m (Maybe (Voter, Vote))

vote :: DelegationM m => Voter -> Idea -> Vote -> m ()
vote voter idea voteValue = do
    let topic = TopicIdea idea
    c <- canVote voter idea
    when c $ do
        voteFor voter idea voteValue voter
        getSupporters voter topic >>= mapM_ (voteFor voter idea voteValue)


-- * deep embedding

data DelegationDSL a where
    SetDelegation :: Voter -> Topic -> Voter         -> DelegationDSL ()
    CanVote       :: Voter -> Idea                   -> DelegationDSL Bool
    GetSupporters :: Voter -> Topic                  -> DelegationDSL [Voter]
    VoteFor       :: Voter -> Idea  -> Vote -> Voter -> DelegationDSL ()
    SetTopicDep   :: Topic -> Topic                  -> DelegationDSL ()
    TopicHiearchy :: Topic                           -> DelegationDSL [Topic]
    GetVote       :: Voter -> Idea                   -> DelegationDSL (Maybe (Voter, Vote))
    Vote          :: Voter -> Idea -> Vote           -> DelegationDSL ()

deriving instance Show a => Show (DelegationDSL a)

delegation :: (DelegationM m) => DelegationDSL a -> m a
delegation (SetDelegation f tp t) = setDelegation f tp t
delegation (CanVote v t)          = canVote v t
delegation (GetSupporters v t)    = getSupporters v t
delegation (VoteFor f tp x t)     = voteFor f tp x t
delegation (SetTopicDep f t)      = setTopicDep f t
delegation (TopicHiearchy t)      = topicHiearchy t
delegation (GetVote v i)          = getVote v i
delegation (Vote v i x)           = vote v i x


delegationStepGen :: Gen (DelegationDSL ())
delegationStepGen = frequency
    [ (3, (do v <- arbitrary
              SetDelegation v <$> arbitrary <*> arbitrary `suchThat` (/=v)))
    , (1, (do t <- arbitrary
              SetTopicDep t <$> arbitrary `suchThat` (/=t)))
    , (1, SetTopicDep   <$> (TopicIdea <$> arbitrary) <*> arbitrary)
    , (5, Vote          <$> arbitrary <*> arbitrary <*> arbitrary)
    ]

instance Arbitrary (DelegationDSL ()) where
    arbitrary = delegationStepGen
    shrink (SetDelegation x y z) =
        [ SetDelegation x' y' z'
        | x' <- shrink x, y' <- shrink y, z' <- shrink z
        , x' /= z'
        ]
    shrink (SetTopicDep x y) =
        [ SetTopicDep x' y'
        | x' <- shrink x, y' <- shrink y
        , x' /= y'
        ]
    shrink (Vote x y z) = Vote <$> shrink x <*> shrink y <*> shrink z

data DelegationProgramStep = DelegationProgramStep Int (DelegationDSL ())

delegationProgram :: Gen DelegationProgram
delegationProgram = (DelegationProgram . (zipWith DelegationProgramStep [1..])) <$> (listOf $ delegationStepGen)

instance Arbitrary DelegationProgram where
    arbitrary = delegationProgram
    shrink (DelegationProgram x) = DelegationProgram <$> shrink x

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

    topicHiearchy t      = topicHiearchyPure t <$> use topicTreeState

    getVote v i          = getVotePure v i <$> use votingsState


-- * testing

debug :: Monad m => Show x => x -> m ()
debug x = do
    () <- traceShow x $ pure ()
    pure ()

instance Arbitrary Vote where
    arbitrary = elements [Yes, No]
    shrink Yes = [No]
    shrink _   = []

voterNames :: [Int]
voterNames = [1..100]

instance Arbitrary Voter where
    arbitrary = Voter <$> elements voterNames
    shrink (Voter x) = Voter <$> shrink x

ideaNames :: [Int]
ideaNames = [1..100]

instance Arbitrary Idea where
    arbitrary = Idea <$> elements ideaNames
    shrink (Idea x) = Idea <$> shrink x

topicNames :: [ST]
topicNames = cs . show <$> [1..20]

-- | Only generates TopicRefs
instance Arbitrary Topic where
     arbitrary = TopicRef <$> elements topicNames
     shrink (TopicRef x) = TopicRef . cs <$> shrink (cs x :: String)
     shrink (TopicIdea i) = TopicIdea <$> shrink i

setDelegationProp1Pre :: Monad m => Voter -> Idea -> Voter -> DelegationT m Bool
setDelegationProp1Pre f i t = not . elem f <$> getSupporters t (TopicIdea i)

-- | No delegation between the voters for a given idea.
setDelegationProp1 :: Monad m => Voter -> Idea -> Voter -> DelegationT m Bool
setDelegationProp1 f i t = do
    let ti = TopicIdea i
    setDelegation f ti t
    elem f <$> getSupporters t ti

setDelegationProp2Pre :: Monad m => Voter -> Idea -> Voter -> DelegationT m Bool
setDelegationProp2Pre f i t = elem f <$> getSupporters t (TopicIdea i)

-- | The number of supporters does not change if the delegates again in the same idea
setDelegationProp2 :: Monad m => Voter -> Idea -> Voter -> DelegationT m Bool
setDelegationProp2 f i t = do
    let ti = TopicIdea i
    supporters <- getSupporters t ti
    setDelegation f ti t
    (supporters ==) <$> getSupporters t ti

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

setTopicDepProp2Pre :: Monad m => Idea -> Topic -> DelegationT m Bool
setTopicDepProp2Pre i t = do
    let ti = TopicIdea i
    ((> 1) . length) <$> topicHiearchy ti

-- Idea is already associated with the topic
setTopicDepProp2 :: Monad m => Idea -> Topic -> DelegationT m Bool
setTopicDepProp2 i t = do
    let ti = TopicIdea i
    hiearchy <- topicHiearchy ti
    setTopicDep ti t
    (hiearchy==) <$> topicHiearchy ti

newtype DelegationProgram = DelegationProgram { unDelegationProgram :: [DelegationProgramStep] }

instance Arbitrary DelegationProgramStep where
    arbitrary = error "Arbitrary DelegationProgramStep"
    shrink (DelegationProgramStep n p) = DelegationProgramStep n <$> shrink p

instance Show DelegationProgram where
    show (DelegationProgram instr) = unlines $ map (\(DelegationProgramStep n i) -> unwords [show n, "\t", show i]) instr

interpretDelegationProgram :: DelegationM m => DelegationProgram -> PropertyM m ()
interpretDelegationProgram =
    mapM_ (run . interpretDelegationStep >=> (maybe (pure ()) fail))
    . unDelegationProgram

interpretDelegationStep :: DelegationM m => DelegationProgramStep -> m (Maybe String)
interpretDelegationStep (DelegationProgramStep i step@(SetDelegation f tp t)) = do
    !supporters <- getSupporters t tp
    delegation step
    supporters' <- getSupporters t tp
    let r = (case elem f supporters of
                True  -> (supporters == )
                False -> (elem f)) $ supporters'
    pure $ if r
        then Nothing
        else Just $ show (i, step, elem f supporters, show f, supporters, supporters')
interpretDelegationStep (DelegationProgramStep i step@(SetTopicDep t0 t1)) = do
    hiearchy <- topicHiearchy t1
    delegation step
    hiearchy' <- topicHiearchy t0
    pure $ if (t0:hiearchy == hiearchy')
        then Nothing
        else Just $ show (i, step, t0:hiearchy, hiearchy')
interpretDelegationStep (DelegationProgramStep j step@(Vote v i x)) = do
    supporters <- getSupporters v (TopicIdea i)
    delegation step
    b <- all (Just (v, x) ==) <$> (forM (v:supporters) $ \s -> getVote s i)
    pure $ if b
        then Nothing
        else Just $ show (j, step, supporters)

drawSeparator = putStrLn "*******************"

quickCheckDelegation p = do
    quickCheck $ monadic runDelegation' p
    drawSeparator

quickCheckDelegationCtx c p = do
    quickCheck $ monadic (\m -> runDelegation' (c >> m)) p
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

    runTC $ do
        setTopicDep (TopicIdea i) tp
        setTopicDepProp2 i tp


    let topics = TopicRef . cs . show <$> [1..10]
    let buildTree = zipWithM setTopicDep topics (tail topics)
    quickCheckDelegationCtx buildTree $ do
        i <- pick arbitrary
        t <- pick $ elements topics
        run $ setTopicDep (TopicIdea i) t
        run (setTopicDepProp2Pre i t) >>= pre
        run (setTopicDepProp2 i t) >>= assert

    quickCheck $ forAllShrink arbitrary shrink $ \program ->
        monadic runDelegation' (interpretDelegationProgram program)

    runTC $ do
        setDelegationProp1 f i t
        setDelegationProp2 f i (Voter 3)
        getSupporters (Voter 3) (TopicIdea i)
