{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Persistent.ApiSpec where

import Test.Hspec
{-
import Arbitrary ()
import Control.Exception (ErrorCall(ErrorCall), throwIO, finally)
import Control.Lens hiding (elements)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.String.Conversions
import Servant.Server
import Test.Hspec
import Test.QuickCheck

import Config
import Persistent
import Persistent.Implementation
import Types

import AulaTests (testConfig)


-- FIXME: use @withPersist@ (instead of @before/it@?)

-- | a database state containing one arbitrary item of each type (idea, user, ...)
mkInitial :: IO (Persist :~> ExceptT PersistExcept IO, IO ())
mkInitial = do
    (rp, rpClose) <- mkEmpty
    runP rp genInitialTestDb
    return (rp, rpClose)

-- | the empty database
mkEmpty :: IO (Persist :~> ExceptT PersistExcept IO, IO ())
mkEmpty = testConfig >>= mkRunPersistInMemory

runPclose :: RunPersist -> Persist a -> m a
runPclose (persist, persistClose) m = -- (`liftIO $` here, and remove the `m ~ IO`, `-XGADTs` above?)
    runP persist m `finally` persistClose

runP :: (m ~ IO, MonadIO m) => (Persist :~> ExceptT PersistExcept m) -> Persist a -> m a
runP persist m =
    runExceptT (unNat persist m) >>= either (throwIO . ErrorCall . show) pure

getDbSpec :: (PersistM r, Eq a, Show a) => Config -> String -> r [a] -> Spec
getDbSpec cfg name getXs = do
    describe name $ do
        context "on empty database" . before (mkEmpty cfg) $ do
            it "returns the empty list" $ \rp -> do
                xs <- runPclose rp getXs
                xs `shouldBe` []
        context "on initial database" . before (mkInitial cfg) $ do
            it "returns a non-empty list" $ \rp -> do
                xs <- runPclose rp getXs
                length xs `shouldNotBe` 0

addDbSpecProp :: forall f proto r.
                 (Foldable f, Arbitrary proto, PersistM r)
              => String
              -> r (f a)
              -> ((User, proto) -> r a)
              -> (proto -> a -> Expectation)
              -> Spec
addDbSpecProp name getXs addX propX =
    describe name $ do
        let t :: SpecWith (r :~> ExceptT PersistExcept IO, IO ())
            t = it "adds one" $ \(rp, rpClose) -> do
                    before' <- liftIO $ length <$> runP rp getXs
                    p <- liftIO $ generate arbitrary
                    r <- liftIO . runP rp $ addX (frameUserHack, p)
                    after' <- liftIO $ length <$> runP rp getXs
                    after' `shouldBe` before' + 1
                    rpClose
                    propX p r

        context "on empty database" . before (mkEmpty cfg) $ t
        context "on initial database" . before (mkInitial cfg) $ t

addDbSpec :: (Foldable f, Arbitrary proto, PersistM r) =>
             Config -> String -> r (f a) -> ((User, proto) -> r a) -> Spec
addDbSpec cfg name getXs addX = addDbSpecProp cfg name getXs addX (\_ _ -> passes)

findInBySpec :: (Eq a, Show a, Arbitrary k, PersistM r) =>
                Config -> String -> r [a] -> (k -> r (Maybe a)) ->
                Fold a k -> (k -> k) ->
                Spec
findInBySpec cfg name getXs findXBy f change =
    describe name $ do
        context "on empty database" . before (mkEmpty cfg) $ do
            it "will come up empty" $ \(rp, rpClose) -> do
                rf <- liftIO $ generate arbitrary
                mu <- liftIO . runP rp $ findXBy rf
                rpClose
                mu `shouldBe` Nothing

        context "on initial database" . before (mkInitial cfg) $ do
            context "if it does not exist" $ do
                it "will come up empty" $ \(rp, rpClose) -> do
                    (x:_) <- liftIO . runP rp $ getXs
                    let Just y = x ^? f
                    mu <- liftIO . runP rp $ findXBy (change y)
                    rpClose
                    mu `shouldBe` Nothing
            context "if it exists" $ do
                it "will come up with the newly added record" $ \(rp, rpClose) -> do
                    (x:_) <- liftIO . runP rp $ getXs
                    let Just y = x ^? f
                    mu <- liftIO . runP rp $ findXBy y
                    rpClose
                    mu `shouldBe` Just x

findAllInBySpec :: (Eq a, Show a, PersistM r) =>
                    Config -> String -> r [a] -> r (Gen k) -> (k -> r [a]) ->
                    Fold a k -> (k -> k) ->
                    Spec
findAllInBySpec cfg name getXs genKs findAllXBy f change =
    describe name $ do
        context "on empty database" . before (mkEmpty cfg) $ do
            it "will come up empty" $ \(rp, rpClose) -> do
                genK <- liftIO . runP rp $ genKs
                rf <- liftIO $ generate genK
                us <- liftIO . runP rp $ findAllXBy rf
                rpClose
                us `shouldBe` []

        context "on initial database" . before (mkInitial cfg) $ do
            context "if it does not exist" $ do
                it "will come up empty" $ \(rp, rpClose) -> do
                    [x] <- liftIO . runP rp $ getXs
                    let Just y = x ^? f
                    us <- liftIO . runP rp $ findAllXBy (change y)
                    rpClose
                    us `shouldBe` []
            context "if it exists" $ do
                it "will come up with the newly added record" $ \(rp, rpClose) -> do
                    [x] <- liftIO . runP rp $ getXs
                    let [y] = x ^.. f
                    us <- liftIO . runP rp $ findAllXBy y
                    rpClose
                    us `shouldBe` [x]

-- Given an AUID pick a different one
changeAUID :: AUID a -> AUID a
changeAUID (AUID i) = AUID (succ i)

spec :: Spec
spec = do
    persistApiSpec STM
    persistApiSpec AcidStateInMem
    persistApiSpec AcidStateOnDisk

persistApiSpec :: Spec
persistApiSpec imp = do
    let cfg = defaultConfig & persistenceImpl .~ imp
    getDbSpec cfg "getIdeas" getIdeas
    addDbSpec cfg "addIdea"  getIdeas addIdea
    getDbSpec cfg "getWildIdeas"      getWildIdeas
    getDbSpec cfg "getIdeasWithTopic" getIdeasWithTopic

    getDbSpec cfg "getUsers" getUsers
    addDbSpec cfg "addUsers" getUsers addUser

    getDbSpec cfg "getTopics" getTopics
    addDbSpec cfg "addTopics" getTopics addTopic

    findInBySpec cfg "findUserByLogin" getUsers findUserByLogin userLogin ("not" <>)
    findInBySpec cfg "findTopic" getTopics findTopic _Id changeAUID

    let elements' [] = arbitrary
        elements' xs = elements xs
        getArbTopicIds :: PersistM r => r (Gen (AUID Topic))
        getArbTopicIds = elements' . map (view _Id) <$> getTopics
    findAllInBySpec cfg "findIdeasByTopicId"
        getIdeasWithTopic getArbTopicIds findIdeasByTopicId ideaTopicId changeAUID

    describe "addIdeaSpace" $ do
        let test :: (Int -> Int) -> IdeaSpace -> SpecWith RunPersist
            test upd ispace = do
                it ("can add " <> showIdeaSpace ispace) $ \(rp, rpClose) -> do
                    let getL = liftIO . runP rp $ getSpaces
                        addS = liftIO . runP rp $ addIdeaSpaceIfNotExists ispace
                    bef <- getL
                    addS
                    aft <- getL
                    rpClose
                    upd (length bef) `shouldBe` length aft
                    (ispace `elem` aft) `shouldBe` True

        context "on empty database" . before (mkEmpty cfg) $ do
            test (+1) SchoolSpace
            test (+1) (ClassSpace (SchoolClass 2016 "7a"))
        context "on initial database" . before (mkInitial cfg) $ do
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
-}
spec :: Spec
spec = return ()
