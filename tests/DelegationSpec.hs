{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE Rank2Types          #-}

{-# OPTIONS_GHC -Werror -Wall    #-}

module DelegationSpec
where

import Prelude hiding ((.))

import Arbitrary
import AulaTests
import DemoData
import Logger (nullLog)
import qualified Action
import qualified Action.Implementation as Action
import qualified Persistent
import qualified Persistent.Api as Persistent (RunPersist)
import qualified Persistent.Implementation.AcidState as Persistent

import Control.Category ((.))
import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, frequency, suchThat, listOf1)
import Test.QuickCheck.Monadic (monadicIO, run)
import qualified Test.QuickCheck as QC (elements)

universeSize :: UniverseSize
universeSize = UniverseSize
    { numberOfIdeaSpaces = 10
    , numberOfStudents = 20
    , numberOfTopics = 10
    , numberOfIdeas = 50
    , numberOfLikes = 0
    , numberOfComments = 0
    , numberOfReplies = 0
    , numberOfCommentVotes = 0
    }


spec :: Spec
spec = {- tag Large . -} do
    runner   <- runIO createActionRunner
    persist  <- runIO Persistent.mkRunPersistInMemory
    uni      <- runIO $ unNat (runner persist) (mkUniverse universeSize)
    snapshot <- runIO $ unNat (runner persist) getDBSnapShot
    let programGen = delegationProgram
                        (QC.elements $ unStudents uni)
                        (QC.elements $ unIdeas    uni)
                        (QC.elements $ universeToDelegationContexts uni)
    describe "Delegation simulation" $ do
        it "One delegation, one vote" $ do
            let student1 = unStudents uni !! 1
                student2 = unStudents uni !! 2
                idea     = unIdeas    uni !! 1
            persist' <- Persistent.mkRunPersistInMemoryWithState snapshot
            unNat (runner persist') . interpretDelegationProgram $ DelegationProgram
                [ SetDelegation student1
                                (DlgCtxIdeaId idea)
                                student2
                , Vote student1 idea Yes
                ]
        it "Random delegation programs" . property . forAllShrinkDef programGen $ \prg -> do
            monadicIO $ do
                persist' <- run $ Persistent.mkRunPersistInMemoryWithState snapshot
                run . unNat (runner persist') $ interpretDelegationProgram prg
  where
    getDBSnapShot :: Action.Action Persistent.AulaData
    getDBSnapShot = query (view Persistent.dbSnapshot)

    createActionRunner :: IO (Persistent.RunPersist -> (Action.Action :~> IO))
    createActionRunner = do
        cfg <- testConfig
        let runAction :: Persistent.RunPersist -> (Action.Action :~> IO)
            runAction persist = exceptToFail . Action.mkRunAction (Action.ActionEnv persist cfg nullLog)

        return runAction

    universeToDelegationContexts :: Universe -> [DelegationContext]
    universeToDelegationContexts u = DlgCtxGlobal:(spaces <> topics <> ideas)
      where
        spaces = DlgCtxIdeaSpace <$> unIdeaSpaces u
        topics = DlgCtxTopicId   <$> unTopics     u
        ideas  = DlgCtxIdeaId    <$> unIdeas      u

-- * delegation program

data DelegationDSL where
    SetDelegation :: AUID User -> DelegationContext -> AUID User -> DelegationDSL
    Vote          :: AUID User -> AUID Idea         -> IdeaVoteValue  -> DelegationDSL

deriving instance Show DelegationDSL


newtype DelegationProgram = DelegationProgram { unDelegationProgram :: [DelegationDSL] }

instance Show DelegationProgram where
    show (DelegationProgram instr) = unlines . map (\(n, i) -> unwords [show (n :: Int), "\t", show i]) $ zip [1..] instr

delegationStepGen :: Gen (AUID User) -> Gen (AUID Idea) -> Gen DelegationContext -> Gen DelegationDSL
delegationStepGen voters ideas topics = frequency
    [ (9, do v <- voters
             SetDelegation v <$> topics <*> voters `suchThat` (/=v))
    , (3, Vote          <$> voters <*> ideas <*> arbitrary)
    ]

dsGen :: Gen DelegationProgram
dsGen = arbitrary

instance Arbitrary DelegationDSL where
    arbitrary = delegationStepGen arb arb arb

delegationProgram :: Gen (AUID User) -> Gen (AUID Idea) -> Gen DelegationContext -> Gen DelegationProgram
delegationProgram voters ideas topics =
    DelegationProgram <$> listOf1 (delegationStepGen voters ideas topics)

instance Arbitrary DelegationProgram where
    arbitrary = delegationProgram arb arb arb
    shrink (DelegationProgram x) = DelegationProgram <$> shrink x

getSupporters :: ActionM m => AUID User -> DelegationContext -> m [AUID User]
getSupporters uid ctx = equery $ do
    _delegationFrom <$$> Persistent.scopeDelegatees uid ctx

getVote :: ActionM m => AUID User -> AUID Idea -> m (Maybe (AUID User, IdeaVoteValue))
getVote uid iid = equery $ do
    first (view _Id) <$$> Persistent.getVote uid iid

interpretDelegationProgram :: ActionM m => DelegationProgram -> m ()
interpretDelegationProgram =
    mapM_ interpretDelegationStep . zip [1..] . unDelegationProgram

interpretDelegationStep :: ActionM m => (Int, DelegationDSL) -> m ()
interpretDelegationStep (i,step@(SetDelegation f tp t)) = do
    Action.login f
    supporters <- getSupporters t tp
    Action.delegateTo tp t
    supporters' <- getSupporters t tp
    Action.logout
    let r = if f `elem` supporters
                then supporters == supporters'
                else f `elem` supporters' && (length supporters' == 1 + length supporters)
    unless r . fail $ show (i, step, f `elem` supporters, show f, supporters, supporters')
interpretDelegationStep (j,step@(Vote v i x)) = do
    Action.login v
    supporters <- (v:) <$> getSupporters v (DlgCtxIdeaId i)
    Action.voteOnIdea i x
    votes <- forM supporters $ \s -> getVote s i
    let rightVotes = all (Just (v, x) ==) votes
    Action.logout
    unless rightVotes . fail $ show (j, step, supporters, votes)
