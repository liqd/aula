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
import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, frequency, listOf1)
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
spec = do
    runner   <- runIO createActionRunner
    persist  <- runIO Persistent.mkRunPersistInMemory
    uni      <- runIO $ unNat (runner persist) (mkUniverse universeSize)
    snapshot <- runIO $ unNat (runner persist) getDBSnapShot
    let programGen = delegationProgram
                        (QC.elements $ (view _Id) <$> unStudents uni)
                        (QC.elements $ (view _Id) <$> unIdeas    uni)
                        (QC.elements $ universeToDelegationContexts uni)
    let runDelegationProgram program = do
            persist' <- Persistent.mkRunPersistInMemoryWithState snapshot
            unNat (runner persist') . interpretDelegationProgram
                $ DelegationProgram program
    let isIdeaWithTopic = has (ideaLocation . _IdeaLocationTopic . _2)
    let student1  = (unStudents uni !! 1) ^. _Id
        student2  = (unStudents uni !! 2) ^. _Id
        student3  = (unStudents uni !! 3) ^. _Id
        student4  = (unStudents uni !! 4) ^. _Id
        idea      = (unIdeas    uni !! 1) ^. _Id
        topic     = (unTopics   uni !! 1) ^. _Id
        (idea2, topic2) = case find isIdeaWithTopic (unIdeas uni) of
            Nothing -> error "No idea with topic is found."
            Just i  -> (i ^. _Id, fromJust (i ^? ideaLocation . _IdeaLocationTopic . _2))
        Just ideaspace = find (has _ClassSpace) $ unIdeaSpaces uni
    describe "Delegation simulation" $ do
        it "One delegation, one vote" $ do
            runDelegationProgram
                [ SetDelegation student1 (DlgCtxIdeaId idea) student2
                , Vote student1 idea Yes
                ]
        it "Self delegation" $ do
            runDelegationProgram
                [ SetDelegation student1 (DlgCtxIdeaId idea) student1
                , VotingPower student1 (DlgCtxIdeaId idea) 1
                , Vote student1 idea No
                ]
        it "Delegation on topic" $ do
            runDelegationProgram
                [ SetDelegation student1 (DlgCtxTopicId topic) student2
                , VotingPower student2 (DlgCtxTopicId topic) 2
                ]
        it "Delegation on ideaspace" $ do
            runDelegationProgram
                [ SetDelegation student1 (DlgCtxIdeaSpace ideaspace) student2
                , VotingPower student2 (DlgCtxIdeaSpace ideaspace) 2
                ]
        it "Delegation on schoolspace" $ do
            runDelegationProgram
                [ SetDelegation student1 (DlgCtxIdeaSpace SchoolSpace) student2
                , VotingPower student2 (DlgCtxIdeaSpace SchoolSpace) 2
                ]
        it "Delegation on global" $ do
            runDelegationProgram
                [ SetDelegation student1 DlgCtxGlobal student2
                , VotingPower student2 DlgCtxGlobal 2
                ]
        it "I change my mind before" $ do
            runDelegationProgram
                [ SetDelegation student1 (DlgCtxIdeaId idea) student2
                , Vote student1 idea No
                , Vote student2 idea Yes
                ]
        it "I change my mind after" $ do
            runDelegationProgram
                [ SetDelegation student1 (DlgCtxIdeaId idea) student2
                , Vote student2 idea Yes
                , Vote student1 idea No
                ]
        describe "No cyclical delegation" $ do
            it "I change my mind works on my delegatees" $ do
                runDelegationProgram
                    [ SetDelegation student1 (DlgCtxIdeaId idea) student2
                    , SetDelegation student2 (DlgCtxIdeaId idea) student3
                    , Vote student3 idea No
                    , Vote student2 idea Yes
                    , Vote student1 idea No
                    ]
            it "Transitive delegation paths work accross different hierarchy levels" $ do
                runDelegationProgram
                    [ SetDelegation student1 (DlgCtxIdeaId idea2) student2
                    , SetDelegation student2 (DlgCtxTopicId topic2) student3
                    , VotingPower student1 (DlgCtxIdeaId idea2) 1
                    , VotingPower student2 (DlgCtxIdeaId idea2) 2
                    , VotingPower student3 (DlgCtxIdeaId idea2) 3
                    , VotingPower student3 (DlgCtxTopicId topic2) 2
                    ]
        describe "Cyclical delegation" $ do
            it "Cycle in delegation" $ do
                pendingWith "Student2 should not change student1's vote."
                runDelegationProgram
                    [ SetDelegation student1 (DlgCtxIdeaId idea) student2
                    , VotingPower student2 (DlgCtxIdeaId idea) 2
                    , SetDelegation student2 (DlgCtxIdeaId idea) student1
                    , VotingPower student1 (DlgCtxIdeaId idea) 2
                    , VotingPower student2 (DlgCtxIdeaId idea) 2
                    , Vote student1 idea Yes
                    , Vote student2 idea No
                    ]
            it "I change my mind only works for me not my delegatees" $ do
                pendingWith "Postcondition check does not express this"
                runDelegationProgram
                    [ SetDelegation student1 (DlgCtxIdeaId idea) student2
                    , SetDelegation student2 (DlgCtxIdeaId idea) student3
                    , SetDelegation student3 (DlgCtxIdeaId idea) student1
                    , VotingPower student1 (DlgCtxIdeaId idea) 3
                    , VotingPower student2 (DlgCtxIdeaId idea) 3
                    , VotingPower student3 (DlgCtxIdeaId idea) 3
                    , Vote student3 idea No
                    , Vote student2 idea Yes
                    , Vote student1 idea No
                    ]
            it "Transitive delegation paths work accross different hierarchy levels" $ do
                runDelegationProgram
                    [ SetDelegation student1 (DlgCtxIdeaId idea2)   student2
                    , SetDelegation student2 (DlgCtxTopicId topic2) student3
                    , SetDelegation student3 DlgCtxGlobal student1
                    , VotingPower student1 (DlgCtxIdeaId idea2) 3
                    , VotingPower student2 (DlgCtxIdeaId idea2) 3
                    , VotingPower student3 (DlgCtxIdeaId idea2) 3
                    , VotingPower student1 (DlgCtxTopicId topic2) 3
                    , VotingPower student2 (DlgCtxTopicId topic2) 1
                    , VotingPower student3 (DlgCtxTopicId topic2) 2
                    , VotingPower student1 DlgCtxGlobal 2
                    , VotingPower student2 DlgCtxGlobal 1
                    , VotingPower student3 DlgCtxGlobal 1
                    ]
            it "Breaking Cycles" $ do
                runDelegationProgram
                    [ SetDelegation student1 (DlgCtxIdeaId idea) student2
                    , SetDelegation student2 (DlgCtxIdeaId idea) student3
                    , SetDelegation student3 (DlgCtxIdeaId idea) student1
                    , VotingPower student1 (DlgCtxIdeaId idea) 3
                    , VotingPower student2 (DlgCtxIdeaId idea) 3
                    , VotingPower student3 (DlgCtxIdeaId idea) 3
                    , SetDelegation student1 (DlgCtxIdeaId idea) student4
                    , VotingPower student1 (DlgCtxIdeaId idea) 3
                    , VotingPower student2 (DlgCtxIdeaId idea) 1
                    , VotingPower student3 (DlgCtxIdeaId idea) 2
                    , VotingPower student4 (DlgCtxIdeaId idea) 4
                    ]
        tag Large . it "Random delegation programs" . property . forAllShrinkDef programGen
            $ \(DelegationProgram program) -> monadicIO . run $ runDelegationProgram program
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
        topics = DlgCtxTopicId   . view _Id <$> unTopics     u
        ideas  = DlgCtxIdeaId    . view _Id <$> unIdeas      u

-- * delegation program

data DelegationDSL where
    SetDelegation :: AUID User -> DelegationContext -> AUID User      -> DelegationDSL
    Vote          :: AUID User -> AUID Idea         -> IdeaVoteValue  -> DelegationDSL
    VotingPower   :: AUID User -> DelegationContext -> Int            -> DelegationDSL

deriving instance Show DelegationDSL


newtype DelegationProgram = DelegationProgram { unDelegationProgram :: [DelegationDSL] }

instance Show DelegationProgram where
    show (DelegationProgram instr) = unlines . map (\(n, i) -> unwords [show (n :: Int), "\t", show i]) $ zip [1..] instr

delegationStepGen :: Gen (AUID User) -> Gen (AUID Idea) -> Gen DelegationContext -> Gen DelegationDSL
delegationStepGen voters ideas topics = frequency
    [ (9, SetDelegation <$> voters <*> topics <*> voters)
    , (3, Vote <$> voters <*> ideas <*> arbitrary)
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

getVotingPower :: ActionM m => AUID User -> DelegationContext -> m [AUID User]
getVotingPower u ctx = sort . nub <$> (view _Id <$$> equery (Persistent.votingPower u ctx))

interpretDelegationStep :: ActionM m => (Int, DelegationDSL) -> m ()
interpretDelegationStep (i,step@(SetDelegation f ctx t)) = do
    Action.login f
    delegateesTo   <- getVotingPower t ctx
    delegateesFrom <- getVotingPower f ctx
    Action.delegateTo ctx t
    delegatees' <- getVotingPower t ctx
    Action.logout
    let ds1 = sort delegatees'
        ds2 = sort (nub (delegateesFrom <> delegateesTo))
    let holds = if f `elem` delegateesTo
                    then delegateesTo == delegatees'
                    else f `elem` delegatees' && (ds1 == ds2)
    unless holds . fail $ show (
        if f `elem` delegateesTo
            then "The delegatees list has changed."
            else "The delegatees are not the sum of from and to",
        i, step, f `elem` delegateesTo, ds1, ds2)
interpretDelegationStep (j,step@(Vote v i x)) = do
    Action.login v
    delegatees <- getVotingPower v (DlgCtxIdeaId i)
    Action.voteOnIdea i x
    votes <- forM delegatees $ \s -> getVote s i
    let rightVotes = all (Just (v, x) ==) votes
    Action.logout
    unless rightVotes . fail $ show ("Not all the votes have right voter or vote.", j, step, delegatees, votes)
interpretDelegationStep (i,step@(VotingPower v ctx n)) = do
    Action.login v
    delegatees <- getVotingPower v ctx
    Action.logout
    unless (length delegatees == n) . fail $ show ("Number of delegatees was different.", i, step, length delegatees, delegatees)
