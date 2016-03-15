{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Persistent.ApiSpec where

import Arbitrary ()
import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe
import Data.String.Conversions
import Servant.Server
import Test.Hspec
import Test.QuickCheck

import Persistent
import Persistent.Implementation.STM
import CreateRandom
import Types


-- | a database state containing one arbitrary item of each type (idea, user, ...)
mkInitial :: IO (Persist :~> IO)
mkInitial = do
    rp <- mkRunPersist
    unNat rp genInitialTestDb
    return rp

-- | the empty database
mkEmpty :: IO (Persist :~> IO)
mkEmpty = mkRunPersist

getDbSpec :: (Eq a, Show a) => String -> Persist [a] -> Spec
getDbSpec name getXs = do
    describe name $ do
        context "on empty database" . before mkEmpty $ do
            it "returns the empty list" $ \(Nat rp) -> do
                xs <- rp getXs
                xs `shouldBe` []
        context "on initial database" . before mkInitial $ do
            it "returns a non-empty list" $ \(Nat rp) -> do
                xs <- rp getXs
                length xs `shouldNotBe` 0

addDbSpecProp :: (Foldable f, Arbitrary proto)
              => String
              -> Persist (f a)
              -> (proto -> Persist a)
              -> (proto -> a -> Expectation)
              -> Spec
addDbSpecProp name getXs addX propX =
    describe name $ do
        let t = it "adds one" $ \(Nat rp) -> do
                    before' <- liftIO $ length <$> rp getXs
                    p <- liftIO $ generate arbitrary
                    r <- liftIO . rp $ addX p
                    after' <- liftIO $ length <$> rp getXs
                    after' `shouldBe` before' + 1
                    propX p r

        context "on empty database" . before mkEmpty $ t
        context "on initial database" . before mkInitial $ t

addDbSpec :: (Foldable f, Arbitrary proto) =>
             String -> Persist (f a) -> (proto -> Persist a) -> Spec
addDbSpec name getXs addX = addDbSpecProp name getXs addX (\_ _ -> passes)

findInBySpec :: (Eq a, Show a, Arbitrary k) =>
                String -> Persist [a] -> (k -> Persist (Maybe a)) ->
                Fold a k -> (k -> k) ->
                Spec
findInBySpec name getXs findXBy f change =
    describe name $ do
        context "on empty database" . before mkEmpty $ do
            it "will come up empty" $ \(Nat rp) -> do
                rf <- liftIO $ generate arbitrary
                mu <- liftIO . rp $ findXBy rf
                mu `shouldBe` Nothing

        context "on initial database" . before mkInitial $ do
            context "if it does not exist" $ do
                it "will come up empty" $ \(Nat rp) -> do
                    x:_ <- liftIO $ rp getXs
                    let Just y = x ^? f
                    mu <- liftIO . rp $ findXBy (change y)
                    mu `shouldBe` Nothing
            context "if it exists" $ do
                it "will come up with the newly added record" $ \(Nat rp) -> do
                    x:_ <- liftIO $ rp getXs
                    let Just y = x ^? f
                    mu <- liftIO . rp $ findXBy y
                    mu `shouldBe` Just x

findAllInBySpec :: (Eq a, Show a, Arbitrary k) =>
                    String -> Persist [a] -> (k -> Persist [a]) ->
                    Fold a k -> (k -> k) ->
                    Spec
findAllInBySpec name getXs findAllXBy f change =
    describe name $ do
        context "on empty database" . before mkEmpty $ do
            it "will come up empty" $ \(Nat rp) -> do
                rf <- liftIO $ generate arbitrary
                us <- liftIO . rp $ findAllXBy rf
                us `shouldBe` []

        context "on initial database" . before mkInitial $ do
            context "if it does not exist" $ do
                it "will come up empty" $ \(Nat rp) -> do
                    x:_ <- liftIO $ rp getXs
                    let Just y = x ^? f
                    us <- liftIO . rp $ findAllXBy (change y)
                    us `shouldBe` []
            context "if it exists" $ do
                it "will come up with the newly added record" $ \(Nat rp) -> do
                    x:_ <- liftIO $ rp getXs
                    let Just y = x ^? f
                    us <- liftIO . rp $ findAllXBy y
                    us `shouldBe` [x]

-- Given an AUID pick a different one
changeAUID :: AUID a -> AUID a
changeAUID (AUID i) = AUID (succ i)

spec :: Spec
spec = do
    getDbSpec "getIdeas" getIdeas
    addDbSpec "addIdea"  getIdeas (addIdea frameUserHack)

    getDbSpec "getUsers" getUsers
    addDbSpec "addUsers" getUsers (addUser frameUserHack)

    getDbSpec "getTopics" getTopics
    addDbSpec "addTopics" getTopics (addTopic frameUserHack)

    findInBySpec "findUserByLogin" getUsers findUserByLogin userLogin ("not" <>)
    findInBySpec "findTopic" getTopics findTopic _Id changeAUID

    {- FIXME: this test doesn't work well with arbitrary, inconsistent databases

    let getIdeasWithTopic :: Persist [Idea]
        getIdeasWithTopic = filter (not . isWild . view ideaLocation) <$> getIdeas
    findAllInBySpec "findIdeasByTopicId"
        getIdeasWithTopic findIdeasByTopicId (ideaMaybeTopicId . _Just) changeAUID

    -}

    describe "addIdeaSpace" $ do
        let test :: (Int -> Int) -> IdeaSpace -> SpecWith (Persist :~> IO)
            test upd ispace = do
                it ("can add " <> showIdeaSpace ispace) $ \(Nat rp) -> do
                    let getL = liftIO . rp $ getSpaces
                        addS = liftIO . rp $ addIdeaSpaceIfNotExists ispace
                    bef <- getL
                    addS
                    aft <- getL
                    upd (length bef) `shouldBe` length aft
                    (ispace `elem` aft) `shouldBe` True

        context "on empty database" . before mkEmpty $ do
            test (+1) SchoolSpace
            test (+1) (ClassSpace (SchoolClass 2016 "7a"))
        context "on initial database" . before mkInitial $ do
            test id SchoolSpace
            test id (ClassSpace (SchoolClass 2016 "7a"))

    regression

-- * Regression suite

regression :: Spec
regression = describe "regression" $ do
    describe "IdeaSpace in proto idea and saved idea should be the same" $
        addDbSpecProp
            "addIdea" getIdeas (addIdea frameUserHack)
            (\p i -> i ^. ideaLocation `shouldBe` p ^. protoIdeaLocation)


----------------------------------------------------------------------
-- Expectations

passes :: Expectation
passes = return ()
