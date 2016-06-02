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
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck (Arbitrary(..), Testable(..), Gen, frequency, listOf1)
import Test.QuickCheck.Monadic (monadicIO, run)
import qualified Data.Map as Map
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
                        (QC.elements $ view _Id <$> unStudents uni)
                        (QC.elements $ view _Id <$> unIdeas    uni)
                        (QC.elements $ universeToDScopes uni)
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
    let noChecks (CheckNumVotingPower{}) = False
        noChecks (CheckVote{})           = False
        noChecks _                       = True
    let observableBehaviour program =
            forAllShrinkDef programGen $ \(DelegationProgram prefix) ->
            forAllShrinkDef programGen $ \(DelegationProgram postfix) ->
                monadicIO . run . runDelegationProgram
                $ prefix <> filter noChecks program <> postfix
    let delegationTest description program = do
            describe description $ do
                it "with empty context" $ runDelegationProgram program
                modifyMaxSuccess (round . (sqrt :: Double -> Double) . fromIntegral) .
                    tag Large . it "with random contexts" $ observableBehaviour program
    describe "Delegation simulation" $ do
        delegationTest "One delegation, one vote"
                [ SetDelegation student1 (DScopeIdeaId idea) student2
                , Vote student1 idea Yes
                ]
        delegationTest "Self delegation"
                [ SetDelegation student1 (DScopeIdeaId idea) student1
                , CheckNumVotingPower student1 (DScopeIdeaId idea) 1
                , Vote student1 idea No
                ]
        delegationTest "Delegation on topic"
                [ SetDelegation student1 (DScopeTopicId topic) student2
                , CheckNumVotingPower student2 (DScopeTopicId topic) 2
                ]
        delegationTest "Delegation on ideaspace"
                [ SetDelegation student1 (DScopeIdeaSpace ideaspace) student2
                , CheckNumVotingPower student2 (DScopeIdeaSpace ideaspace) 2
                ]
        delegationTest "Delegation on schoolspace"
                [ SetDelegation student1 (DScopeIdeaSpace SchoolSpace) student2
                , CheckNumVotingPower student2 (DScopeIdeaSpace SchoolSpace) 2
                ]
        delegationTest "Delegation on global"
                [ SetDelegation student1 DScopeGlobal student2
                , CheckNumVotingPower student2 DScopeGlobal 2
                ]
        delegationTest "I change my mind before"
                [ SetDelegation student1 (DScopeIdeaId idea) student2
                , Vote student1 idea No
                , Vote student2 idea Yes
                ]
        delegationTest "I change my mind after"
                [ SetDelegation student1 (DScopeIdeaId idea) student2
                , Vote student2 idea Yes
                , Vote student1 idea No
                ]
        delegationTest "Cycle of four"
                [ SetDelegation student1 (DScopeIdeaId idea) student4
                , SetDelegation student4 (DScopeIdeaId idea) student2
                , SetDelegation student2 (DScopeIdeaId idea) student3
                , SetDelegation student3 (DScopeIdeaId idea) student1
                , Vote student2 idea Yes
                , Vote student1 idea No
                ]
        describe "No cyclical delegation" $ do
            delegationTest "I change my mind works on my delegatees"
                    [ SetDelegation student1 (DScopeIdeaId idea) student2
                    , SetDelegation student2 (DScopeIdeaId idea) student3
                    , Vote student3 idea No
                    , Vote student2 idea Yes
                    , Vote student1 idea No
                    ]
            delegationTest "Transitive delegation paths work accross different hierarchy levels"
                    [ SetDelegation student1 (DScopeIdeaId idea2) student2
                    , SetDelegation student2 (DScopeTopicId topic2) student3
                    , CheckNumVotingPower student1 (DScopeIdeaId idea2) 1
                    , CheckNumVotingPower student2 (DScopeIdeaId idea2) 2
                    , CheckNumVotingPower student3 (DScopeIdeaId idea2) 3
                    , CheckNumVotingPower student3 (DScopeTopicId topic2) 2
                    ]
        describe "Cyclical delegation" $ do
            delegationTest "Cycle in delegation"
                    [ SetDelegation student1 (DScopeIdeaId idea) student2
                    , CheckNumVotingPower student2 (DScopeIdeaId idea) 2
                    , SetDelegation student2 (DScopeIdeaId idea) student1
                    , CheckNumVotingPower student1 (DScopeIdeaId idea) 2
                    , CheckNumVotingPower student2 (DScopeIdeaId idea) 2
                    , Vote student1 idea Yes
                    , Vote student2 idea No
                    ]
            delegationTest "I change my mind only works for me not my delegatees"
                    [ SetDelegation student1 (DScopeIdeaId idea) student2
                    , SetDelegation student2 (DScopeIdeaId idea) student3
                    , SetDelegation student3 (DScopeIdeaId idea) student1
                    , CheckNumVotingPower student1 (DScopeIdeaId idea) 3
                    , CheckNumVotingPower student2 (DScopeIdeaId idea) 3
                    , CheckNumVotingPower student3 (DScopeIdeaId idea) 3
                    , Vote student3 idea No
                    , Vote student2 idea Yes
                    , Vote student1 idea No
                    ]
            delegationTest "Transitive delegation paths work accross different hierarchy levels"
                    [ SetDelegation student1 (DScopeIdeaId idea2)   student2
                    , SetDelegation student2 (DScopeTopicId topic2) student3
                    , SetDelegation student3 DScopeGlobal student1
                    , CheckNumVotingPower student1 (DScopeIdeaId idea2) 3
                    , CheckNumVotingPower student2 (DScopeIdeaId idea2) 3
                    , CheckNumVotingPower student3 (DScopeIdeaId idea2) 3
                    , CheckNumVotingPower student1 (DScopeTopicId topic2) 3
                    , CheckNumVotingPower student2 (DScopeTopicId topic2) 1
                    , CheckNumVotingPower student3 (DScopeTopicId topic2) 2
                    , CheckNumVotingPower student1 DScopeGlobal 2
                    , CheckNumVotingPower student2 DScopeGlobal 1
                    , CheckNumVotingPower student3 DScopeGlobal 1
                    ]
            delegationTest "Breaking Cycles"
                    [ SetDelegation student1 (DScopeIdeaId idea) student2
                    , SetDelegation student2 (DScopeIdeaId idea) student3
                    , SetDelegation student3 (DScopeIdeaId idea) student1
                    , CheckNumVotingPower student1 (DScopeIdeaId idea) 3
                    , CheckNumVotingPower student2 (DScopeIdeaId idea) 3
                    , CheckNumVotingPower student3 (DScopeIdeaId idea) 3
                    , SetDelegation student1 (DScopeIdeaId idea) student4
                    , CheckNumVotingPower student1 (DScopeIdeaId idea) 3
                    , CheckNumVotingPower student2 (DScopeIdeaId idea) 1
                    , CheckNumVotingPower student3 (DScopeIdeaId idea) 2
                    , CheckNumVotingPower student4 (DScopeIdeaId idea) 4
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

    universeToDScopes :: Universe -> [DScope]
    universeToDScopes u = DScopeGlobal:(spaces <> topics <> ideas)
      where
        spaces = DScopeIdeaSpace <$> unIdeaSpaces u
        topics = DScopeTopicId   . view _Id <$> unTopics     u
        ideas  = DScopeIdeaId    . view _Id <$> unIdeas      u

-- * delegation program

data DelegationDSL where
    SetDelegation       :: AUID User -> DScope -> AUID User         -> DelegationDSL
    Vote                :: AUID User -> AUID Idea -> IdeaVoteValue  -> DelegationDSL
    CheckNumVotingPower :: AUID User -> DScope -> Int               -> DelegationDSL
    CheckVote           :: AUID User -> AUID Idea -> IdeaVoteValue  -> DelegationDSL

deriving instance Show DelegationDSL

newtype DelegationProgram = DelegationProgram { unDelegationProgram :: [DelegationDSL] }

instance Show DelegationProgram where
    show (DelegationProgram instr) = unlines . map (\(n, i) -> unwords [show (n :: Int), "\t", show i]) $ zip [1..] instr

delegationStepGen :: Gen (AUID User) -> Gen (AUID Idea) -> Gen DScope -> Gen DelegationDSL
delegationStepGen voters ideas topics = frequency
    [ (9, SetDelegation <$> voters <*> topics <*> voters)
    , (3, Vote <$> voters <*> ideas <*> arbitrary)
    ]

dsGen :: Gen DelegationProgram
dsGen = arbitrary

instance Arbitrary DelegationDSL where
    arbitrary = delegationStepGen arb arb arb

delegationProgram :: Gen (AUID User) -> Gen (AUID Idea) -> Gen DScope -> Gen DelegationProgram
delegationProgram voters ideas topics =
    DelegationProgram <$> listOf1 (delegationStepGen voters ideas topics)

instance Arbitrary DelegationProgram where
    arbitrary = delegationProgram arb arb arb
    shrink (DelegationProgram x) = DelegationProgram <$> shrink x

getSupporters :: ActionM m => AUID User -> DScope -> m [AUID User]
getSupporters uid scope = equery $ do
    _delegationFrom <$$> Persistent.scopeDelegatees uid scope

getVote :: ActionM m => AUID User -> AUID Idea -> m (Maybe (AUID User, IdeaVoteValue))
getVote uid iid = equery $ do
    first (view _Id) <$$> Persistent.getVote uid iid

interpretDelegationProgram :: ActionM m => DelegationProgram -> m ()
interpretDelegationProgram =
    mapM_ interpretDelegationStep . zip [1..] . unDelegationProgram

getVotingPower :: ActionM m => AUID User -> DScope -> m [AUID User]
getVotingPower u scope = sort . nub <$> (view _Id <$$> equery (Persistent.votingPower u scope))

interpretDelegationStep :: ActionM m => (Int, DelegationDSL) -> m ()
interpretDelegationStep (i,step@(SetDelegation f scope t)) = do
    Action.login f
    delegateesTo   <- getVotingPower t scope
    delegateesFrom <- getVotingPower f scope
    Action.delegateTo scope t
    delegatees' <- getVotingPower t scope
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
    -- For the delegates who are already voted for themselves the
    -- vote should not be changed, expect for the `v` voter who
    -- can always change his/her vote.
    let getVote' d = (,) d <$> getVote d i
    Action.login v
    delegatees <- getVotingPower v (DScopeIdeaId i)
    votedForThemselves <- Map.fromList . filter (\(d,dv) -> Just True == (((d ==) . fst) <$> dv))
                            <$> forM (delegatees \\ [v]) getVote'
    Action.voteOnIdea i x
    votes <- forM delegatees getVote'
    let checkVote (d, dv) = dv == fromMaybe (Just (v,x)) (Map.lookup d votedForThemselves)
    let rightVotes = all checkVote votes
    Action.logout
    unless rightVotes . fail $ show ("Not all the votes have right voter or vote.", j, step, delegatees, votes)
interpretDelegationStep (i,step@(CheckNumVotingPower v scope n)) = do
    Action.login v
    delegatees <- getVotingPower v scope
    Action.logout
    unless (length delegatees == n) . fail $ show ("Number of delegatees was different.", i, step, length delegatees, delegatees)
interpretDelegationStep (i,step@(CheckVote v scope vote)) = do
    Action.login v
    mvote <- getVote v scope
    Action.logout
    case mvote of
        Nothing -> fail $ show ("There should be a vote", i, step)
        Just (_voter, vote')
            | vote' /= vote -> fail $ show ("Wrong vote value", i, step, vote)
        _ -> pure ()
