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

import Prelude       hiding ((.), id)
import Data.Function hiding ((.), id)
import Data.String.Conversions (ST, cs)
import Control.Applicative hiding (empty)
import Control.Category
import Control.Exception (SomeException)
import Control.Lens hiding (elements, pre)
import Control.Monad.Except
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Debug.Trace

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


-- * pure model

data Delegations = Delegations { unDelegations :: Set (Voter, Topic, Voter) }
  deriving (Eq, Show)

candidates :: Voter -> Delegations -> [(Voter, Topic, Voter)]
candidates v (Delegations dset) = filter ((v ==) . view _1) $ Set.toList dset

delegators :: Voter -> Delegations -> [(Voter, Topic, Voter)]
delegators v (Delegations dset) = filter ((v==) . view _3) $ Set.toList dset

topicDelegators :: Voter -> Topic -> Delegations -> [(Voter, Topic, Voter)]
topicDelegators v t = filter ((t==) . view _2) . delegators v

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

makeLenses ''Delegations
makeLenses ''Votings
makeLenses ''TopicTree
makeLenses ''DelegationState

emptyDelegations = Delegations Set.empty
emptyVotings     = Votings   Map.empty
emptyTopicTree   = TopicTree Map.empty

emptyDelegationState = DelegationState emptyDelegations emptyVotings emptyTopicTree

setDelegationPure :: Voter -> Topic -> Voter -> Delegations -> Delegations
setDelegationPure f tp t (Delegations dset) = Delegations .
    Set.insert (f,tp,t)
    . Set.filter (\(f',tp',_) -> not (f' == f && tp' == tp))
    $ dset

-- | Non empty list of topics, from leaf to root.
topicHiearchyPure :: Topic -> TopicTree -> [Topic]
topicHiearchyPure t (TopicTree tmap) = fix path t
  where
    path rec t = maybe [t] ((t:) . rec) (Map.lookup t tmap)

getSupportersPure :: Voter -> Topic -> Delegations -> TopicTree -> [Voter]
getSupportersPure v t dset ttree = fix voters v
  where
    -- idea -> class -> school
    topicPath  = topicHiearchyPure t ttree

    -- If the first voter in the candidate list is the `v`, `v` is
    -- responsible for the voting for the user u
    supporter u =
        -- FIXME: Assumption: only one candidate per topic
        let cm = Map.fromList . map ((,) <$> view _2 <*> view _3) $ candidates u dset
        in case catMaybes $ map (\t -> Map.lookup t cm) topicPath of
            []     -> False
            v' : _ -> v' == v

    voters rec v =
        v : (mconcat . map rec) (filter supporter <$> map (view _1) $ topicDelegators v t dset)

setVoteForPure :: Voter -> Idea -> Vote -> Voter -> Votings -> Votings
setVoteForPure voter idea vote delegator (Votings vmap) =
    Votings $ Map.insert (delegator, idea) (voter, vote) vmap

setTopicDepPure :: Topic -> Topic -> TopicTree -> Either DelegationError TopicTree
setTopicDepPure f t tt | elem f (topicHiearchyPure t tt) = Left DelegationCircularTopicDependency
setTopicDepPure f t tm@(TopicTree tmap) =
    Right $ case Map.lookup t tmap of
        Just f' | f == f' -> tm -- Avoid circular deps should throw an error
        _                 -> TopicTree (Map.insert f t tmap)

getVotePure :: Voter -> Idea -> Votings -> Maybe (Voter, Vote)
getVotePure v i (Votings vmap) = Map.lookup (v,i) vmap

-- * monadic api

class Monad m => DelegationM m where
    setDelegation :: Voter -> Topic -> Voter -> m ()
    getSupporters :: Voter -> Topic -> m [Voter]
    voteFor       :: Voter -> Idea  -> Vote -> Voter -> m ()
    setTopicDep   :: Topic -> Topic -> m ()
    topicHiearchy :: Topic -> m [Topic]
    getVote       :: Voter -> Idea -> m (Maybe (Voter, Vote))

vote :: DelegationM m => Voter -> Idea -> Vote -> m ()
vote voter idea voteValue = do
    let topic = TopicIdea idea
    voteFor voter idea voteValue voter
    getSupporters voter topic >>= mapM_ (voteFor voter idea voteValue)


-- * deep embedding

data DelegationDSL a where
    SetDelegation :: Voter -> Topic -> Voter         -> DelegationDSL ()
    GetSupporters :: Voter -> Topic                  -> DelegationDSL [Voter]
    SetTopicDep   :: Topic -> Topic                  -> DelegationDSL ()
    GetVote       :: Voter -> Idea                   -> DelegationDSL (Maybe (Voter, Vote))
    Vote          :: Voter -> Idea -> Vote           -> DelegationDSL ()

deriving instance Show a => Show (DelegationDSL a)

delegation :: (DelegationM m) => DelegationDSL a -> m a
delegation (SetDelegation f tp t) = setDelegation f tp t
delegation (GetSupporters v t)    = getSupporters v t
delegation (SetTopicDep f t)      = setTopicDep f t
delegation (GetVote v i)          = getVote v i
delegation (Vote v i x)           = vote v i x


-- * state monad implementation

data DelegationError
    = DelegationCircularTopicDependency
    | DelegationException SomeException
  deriving Show

newtype DelegationT m a = DelegationT { unDelegationT :: StateT DelegationState (ExceptT DelegationError m) a }
  deriving (Functor, Applicative, Monad, MonadState DelegationState, MonadError DelegationError)

runDelegation :: DelegationT Identity a -> Either DelegationError (a, DelegationState)
runDelegation d = runIdentity . runExceptT $ runStateT (unDelegationT d) emptyDelegationState

runDelegationInTest :: DelegationT Identity Property -> Property
runDelegationInTest d = either checkException (label "Valid Program") . runIdentity . runExceptT $ evalStateT (unDelegationT d) emptyDelegationState
  where
    checkException DelegationCircularTopicDependency = label "Circular Topic Dependency" True
    checkException s = error (show s)

throwDelegationError :: Monad m => Either DelegationError a -> DelegationT m a
throwDelegationError (Left e)  = throwError e
throwDelegationError (Right a) = return a

instance Monad m => DelegationM (DelegationT m) where
    setDelegation f tp t = delegationsState %= (setDelegationPure f tp t)

    getSupporters v t    = getSupportersPure v t
                            <$> use delegationsState
                            <*> use topicTreeState

    voteFor v t x d      = votingsState %= (setVoteForPure v t x d)

    setTopicDep f t      = setTopicDepPure f t <$> use topicTreeState
                            >>= throwDelegationError
                            >>= (topicTreeState .=)

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
voterNames = [1..50]

instance Arbitrary Voter where
    arbitrary = Voter <$> elements voterNames
    shrink (Voter x) = Voter <$> shrink x

ideaNames :: [Int]
ideaNames = [1..50]

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

newtype DelegationProgram = DelegationProgram { unDelegationProgram :: [DelegationDSL ()] }

instance Show DelegationProgram where
    show (DelegationProgram instr) = unlines . map (\(n, i) -> unwords [show n, "\t", show i]) $ zip [1..] instr

interpretDelegationProgram :: DelegationM m => DelegationProgram -> PropertyM m ()
interpretDelegationProgram =
    mapM_ (run . interpretDelegationStep >=> (maybe (pure ()) fail)) . zip [1..]
    . unDelegationProgram

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

delegationProgram :: Gen DelegationProgram
delegationProgram = DelegationProgram <$> (listOf1 $ delegationStepGen)

instance Arbitrary DelegationProgram where
    arbitrary = delegationProgram
    shrink (DelegationProgram x) = DelegationProgram <$> shrink x

interpretDelegationStep :: DelegationM m => (Int, DelegationDSL ())-> m (Maybe String)
interpretDelegationStep (i,step@(SetDelegation f tp t)) = do
    supporters <- getSupporters t tp
    delegation step
    supporters' <- getSupporters t tp
    let r = case elem f supporters of
                    True  -> supporters == supporters'
                    False -> (elem f supporters') && (length supporters' == 1 + (length supporters))
    pure $ if r
        then Nothing
        else Just $ show (i, step, elem f supporters, show f, supporters, supporters')
interpretDelegationStep (i,step@(SetTopicDep t0 t1)) = do
    hiearchy <- topicHiearchy t1
    delegation step
    hiearchy' <- topicHiearchy t0
    pure $ if (t0:hiearchy == hiearchy')
        then Nothing
        else Just $ show (i, step, t0:hiearchy, hiearchy')
interpretDelegationStep (j,step@(Vote v i x)) = do
    supporters <- getSupporters v (TopicIdea i)
    delegation step
    b <- all (Just (v, x) ==) <$> (forM (v:supporters) $ \s -> getVote s i)
    pure $ if b
        then Nothing
        else Just $ show (j, step, supporters)

drawSeparator = putStrLn "*******************"

noOfTests = 10000
sizeOfTests = 1000

quickCheckDelegation p = do
    quickCheckWith stdArgs { maxSuccess = noOfTests, maxSize = sizeOfTests } $ monadic runDelegationInTest p
    drawSeparator

quickCheckDelegationCtx c p = do
    quickCheckWith stdArgs { maxSuccess = noOfTests, maxSize = sizeOfTests } $ monadic (\m -> runDelegationInTest (c >> m)) p
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

    quickCheckWith stdArgs { maxSuccess = noOfTests, maxSize = sizeOfTests } $ forAllShrink arbitrary shrink $ \program ->
        monadic runDelegationInTest (interpretDelegationProgram program)

    runTC $ do
        setDelegationProp1 f i t
        setDelegationProp2 f i (Voter 3)
        getSupporters (Voter 3) (TopicIdea i)
