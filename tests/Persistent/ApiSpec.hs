{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Persistent.ApiSpec where

import Arbitrary ()
import Control.Lens hiding (elements)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Functor.Infix ((<$$>))
import Data.String.Conversions
import Servant.Server
import Test.Hspec
import Test.QuickCheck

import Persistent
import Persistent.Implementation.STM
import CreateRandom
import Types


-- | a database state containing one arbitrary item of each type (idea, user, ...)
mkInitial :: IO (Persist :~> ExceptT PersistExcept IO)
mkInitial = do
    rp <- mkRunPersist
    _  <- runExceptT $ unNat rp genInitialTestDb
    return rp

-- | the empty database
mkEmpty :: IO (Persist :~> ExceptT PersistExcept IO)
mkEmpty = mkRunPersist

getDbSpec :: (Eq a, Show a) => String -> Persist [a] -> Spec
getDbSpec name getXs = do
    describe name $ do
        context "on empty database" . before mkEmpty $ do
            it "returns the empty list" $ \(Nat rp) -> do
                Right xs <- runExceptT $ rp getXs
                xs `shouldBe` []
        context "on initial database" . before mkInitial $ do
            it "returns a non-empty list" $ \(Nat rp) -> do
                Right xs <- runExceptT $ rp getXs
                length xs `shouldNotBe` 0

addDbSpecProp :: (Foldable f, Arbitrary proto)
              => String
              -> Persist (f a)
              -> ((User, proto) -> Persist a)
              -> (proto -> a -> Expectation)
              -> Spec
addDbSpecProp name getXs addX propX =
    describe name $ do
        let t :: SpecWith (Persist :~> ExceptT PersistExcept IO)
            t = it "adds one" $ \(Nat rp) -> do
                    Right before' <- liftIO $ length <$$> runExceptT (rp getXs)
                    p <- liftIO $ generate arbitrary
                    Right r <- liftIO . runExceptT . rp $ addX (frameUserHack, p)
                    Right after' <- liftIO $ length <$$> runExceptT (rp getXs)
                    after' `shouldBe` before' + 1
                    propX p r

        context "on empty database" . before mkEmpty $ t
        context "on initial database" . before mkInitial $ t

addDbSpec :: (Foldable f, Arbitrary proto) =>
             String -> Persist (f a) -> ((User, proto) -> Persist a) -> Spec
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
                Right mu <- liftIO . runExceptT . rp $ findXBy rf
                mu `shouldBe` Nothing

        context "on initial database" . before mkInitial $ do
            context "if it does not exist" $ do
                it "will come up empty" $ \(Nat rp) -> do
                    Right (x:_) <- liftIO . runExceptT . rp $ getXs
                    let Just y = x ^? f
                    Right mu <- liftIO . runExceptT . rp $ findXBy (change y)
                    mu `shouldBe` Nothing
            context "if it exists" $ do
                it "will come up with the newly added record" $ \(Nat rp) -> do
                    Right (x:_) <- liftIO . runExceptT . rp $ getXs
                    let Just y = x ^? f
                    Right mu <- liftIO . runExceptT . rp $ findXBy y
                    mu `shouldBe` Just x

findAllInBySpec :: (Eq a, Show a) =>
                    String -> Persist [a] -> Persist (Gen k) -> (k -> Persist [a]) ->
                    Fold a k -> (k -> k) ->
                    Spec
findAllInBySpec name getXs genKs findAllXBy f change =
    describe name $ do
        context "on empty database" . before mkEmpty $ do
            it "will come up empty" $ \(Nat rp) -> do
                Right genK <- liftIO . runExceptT . rp $ genKs
                rf <- liftIO $ generate genK
                Right us <- liftIO . runExceptT . rp $ findAllXBy rf
                us `shouldBe` []

        context "on initial database" . before mkInitial $ do
            context "if it does not exist" $ do
                it "will come up empty" $ \(Nat rp) -> do
                    Right [x] <- liftIO . runExceptT . rp $ getXs
                    let Just y = x ^? f
                    Right us <- liftIO . runExceptT . rp $ findAllXBy (change y)
                    us `shouldBe` []
            context "if it exists" $ do
                it "will come up with the newly added record" $ \(Nat rp) -> do
                    Right [x] <- liftIO . runExceptT . rp $ getXs
                    let [y] = x ^.. f
                    Right us <- liftIO . runExceptT . rp $ findAllXBy y
                    us `shouldBe` [x]

-- Given an AUID pick a different one
changeAUID :: AUID a -> AUID a
changeAUID (AUID i) = AUID (succ i)

spec :: Spec
spec = do
    getDbSpec "getIdeas" getIdeas
    addDbSpec "addIdea"  getIdeas addIdea
    getDbSpec "getWildIdeas"      getWildIdeas
    getDbSpec "getIdeasWithTopic" getIdeasWithTopic

    getDbSpec "getUsers" getUsers
    addDbSpec "addUsers" getUsers addUser

    getDbSpec "getTopics" getTopics
    addDbSpec "addTopics" getTopics addTopic

    findInBySpec "findUserByLogin" getUsers findUserByLogin userLogin ("not" <>)
    findInBySpec "findTopic" getTopics findTopic _Id changeAUID

    let elements' [] = arbitrary
        elements' xs = elements xs
        getArbTopicIds :: Persist (Gen (AUID Topic))
        getArbTopicIds = elements' . map (view _Id) <$> getTopics
    findAllInBySpec "findIdeasByTopicId"
        getIdeasWithTopic getArbTopicIds findIdeasByTopicId ideaTopicId changeAUID

    describe "addIdeaSpace" $ do
        let test :: (Int -> Int) -> IdeaSpace -> SpecWith (Persist :~> ExceptT PersistExcept IO)
            test upd ispace = do
                it ("can add " <> showIdeaSpace ispace) $ \(Nat rp) -> do
                    let getL = liftIO . runExceptT . rp $ getSpaces
                        addS = liftIO . runExceptT . rp $ addIdeaSpaceIfNotExists ispace
                    Right bef <- getL
                    Right _   <- addS
                    Right aft <- getL
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
            "addIdea" getIdeas addIdea
            (\p i -> i ^. ideaLocation `shouldBe` p ^. protoIdeaLocation)


-- * Expectations

passes :: Expectation
passes = return ()
