{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Persistent.ApiSpec where

import Arbitrary ()
import Control.Exception (ErrorCall(ErrorCall), throwIO, finally)
import Control.Lens hiding (elements)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (runExceptT)
import Data.String.Conversions
import Servant.Server
import Test.Hspec
import Test.QuickCheck

import CreateRandom
import Config
import Persistent
import Persistent.Implementation
import Types

import AulaTests (testConfig)


-- FIXME: use @withPersist@ (instead of @before/it@?)

-- | a database state containing one arbitrary item of each type (idea, user, ...)
mkInitial :: PersistenceImpl -> IO RunPersist
mkInitial imp = do
    RunPersist desc rp rpClose <- mkEmpty imp
    runP rp genInitialTestDb
    pure $ RunPersist desc rp rpClose

-- | the empty database
mkEmpty :: PersistenceImpl -> IO RunPersist
mkEmpty imp = do
    cfg <- (persistenceImpl .~ imp) <$> testConfig
    mkRunPersist cfg

runPclose :: RunPersist -> (forall r. PersistM r => RunPersistNat IO r -> IO a) -> IO a
runPclose (RunPersist _ persist persistClose) m =
    m persist `finally` persistClose

type PolyPersist a = forall r. PersistM r => r a

runP :: (MonadIO m, PersistM r) => RunPersistNat IO r -> r a -> m a
runP persist m = liftIO $
    runExceptT (unNat persist m) >>= either (throwIO . ErrorCall . show) pure

getDbSpec :: (Eq a, Show a) => PersistenceImpl -> String -> PolyPersist [a] -> Spec
getDbSpec imp name getXs = do
    describe name $ do
        context "on empty database" . before (mkEmpty imp) $ do
            it "returns the empty list" $ \rp' -> runPclose rp' (\rp -> do
                xs <- runP rp getXs
                xs `shouldBe` [])
        context "on initial database" . before (mkInitial imp) $ do
            it "returns a non-empty list" $ \rp' -> runPclose rp' (\rp -> do
                xs <- runP rp getXs
                length xs `shouldNotBe` 0)

addDbSpecProp :: forall f proto a.
                 (Foldable f, Arbitrary proto)
              => PersistenceImpl
              -> String
              -> PolyPersist (f a)
              -> ((User, proto) -> PolyPersist a)
              -> (proto -> a -> Expectation)
              -> Spec
addDbSpecProp imp name getXs addX propX =
    describe name $ do
        let t :: SpecWith RunPersist
            t = it "adds one" $ \rp' -> runPclose rp' (\rp -> do
                    before' <- length <$> runP rp getXs
                    p <- liftIO $ generate arbitrary
                    r <- runP rp $ addX (frameUserHack, p)
                    after' <- length <$> runP rp getXs
                    after' `shouldBe` before' + 1
                    propX p r)

        context "on empty database" . before (mkEmpty imp) $ t
        context "on initial database" . before (mkInitial imp) $ t

addDbSpec :: (Foldable f, Arbitrary proto) =>
             PersistenceImpl -> String -> PolyPersist (f a) -> ((User, proto) -> PolyPersist a) -> Spec
addDbSpec imp name getXs addX = addDbSpecProp imp name getXs addX (\_ _ -> passes)

findInBySpec :: (Eq a, Show a, Arbitrary k) =>
                PersistenceImpl -> String -> PolyPersist [a] -> (k -> PolyPersist (Maybe a)) ->
                Fold a k -> (k -> k) ->
                Spec
findInBySpec imp name getXs findXBy f change =
    describe name $ do
        context "on empty database" . before (mkEmpty imp) $ do
            it "will come up empty" $ \rp' -> runPclose rp' (\rp -> do
                rf <- liftIO $ generate arbitrary
                mu <- runP rp $ findXBy rf
                mu `shouldBe` Nothing)

        context "on initial database" . before (mkInitial imp) $ do
            context "if it does not exist" $ do
                it "will come up empty" $ \rp' -> runPclose rp' (\rp -> do
                    (x:_) <- runP rp getXs
                    let Just y = x ^? f
                    mu <- runP rp $ findXBy (change y)
                    mu `shouldBe` Nothing)
            context "if it exists" $ do
                it "will come up with the newly added record" $ \rp' -> runPclose rp' (\rp -> do
                    (x:_) <- runP rp getXs
                    let Just y = x ^? f
                    mu <- runP rp $ findXBy y
                    mu `shouldBe` Just x)

findAllInBySpec :: (Eq a, Show a) =>
                    PersistenceImpl -> String -> PolyPersist [a] -> PolyPersist (Gen k) -> (k -> PolyPersist [a]) ->
                    Fold a k -> (k -> k) ->
                    Spec
findAllInBySpec imp name getXs genKs findAllXBy f change =
    describe name $ do
        context "on empty database" . before (mkEmpty imp) $ do
            it "will come up empty" $ \rp' -> runPclose rp' (\rp -> do
                genK <- runP rp genKs
                rf <- liftIO $ generate genK
                us <- runP rp $ findAllXBy rf
                us `shouldBe` [])

        context "on initial database" . before (mkInitial imp) $ do
            context "if it does not exist" $ do
                it "will come up empty" $ \rp' -> runPclose rp' (\rp -> do
                    [x] <- runP rp getXs
                    let Just y = x ^? f
                    us <- runP rp $ findAllXBy (change y)
                    us `shouldBe` [])
            context "if it exists" $ do
                it "will come up with the newly added record" $ \rp' -> runPclose rp' (\rp -> do
                    [x] <- runP rp getXs
                    let [y] = x ^.. f
                    us <- runP rp $ findAllXBy y
                    us `shouldBe` [x])

-- Given an AUID pick a different one
changeAUID :: AUID a -> AUID a
changeAUID (AUID i) = AUID (succ i)

spec :: Spec
spec = do
    context "Using STM"             $ persistApiSpec STM
    context "Using AcidStateInMem"  $ persistApiSpec AcidStateInMem

    -- I guess we need to change the config to target a different directory.
    -- And wipe out the data every time.
    -- context "Using AcidStateOnDisk" $ persistApiSpec AcidStateOnDisk

persistApiSpec :: PersistenceImpl -> Spec
persistApiSpec imp = do
    getDbSpec imp "getIdeas" getIdeas
    addDbSpec imp "addIdea"  getIdeas addIdea
    getDbSpec imp "getWildIdeas"      getWildIdeas
    getDbSpec imp "getIdeasWithTopic" getIdeasWithTopic

    getDbSpec imp "getUsers" getUsers
    addDbSpec imp "addUsers" getUsers addUser

    getDbSpec imp "getTopics" getTopics
    addDbSpec imp "addTopics" getTopics addTopic

    findInBySpec imp "findUserByLogin" getUsers findUserByLogin userLogin ("not" <>)
    findInBySpec imp "findTopic" getTopics findTopic _Id changeAUID

    let elements' [] = arbitrary
        elements' xs = elements xs
        getArbTopicIds :: PersistM r => r (Gen (AUID Topic))
        getArbTopicIds = elements' . map (view _Id) <$> getTopics
    findAllInBySpec imp "findIdeasByTopicId"
        getIdeasWithTopic getArbTopicIds findIdeasByTopicId ideaTopicId changeAUID

    describe "addIdeaSpace" $ do
        let test :: (Int -> Int) -> IdeaSpace -> SpecWith RunPersist
            test upd ispace = do
                it ("can add " <> showIdeaSpace ispace) $ \rp' -> runPclose rp' (\rp -> do
                    let getL = runP rp getSpaces
                        addS = runP rp $ addIdeaSpaceIfNotExists ispace
                    bef <- getL
                    addS
                    aft <- getL
                    upd (length bef) `shouldBe` length aft
                    (ispace `elem` aft) `shouldBe` True)

        context "on empty database" . before (mkEmpty imp) $ do
            test (+1) SchoolSpace
            test (+1) (ClassSpace (SchoolClass 2016 "7a"))
        context "on initial database" . before (mkInitial imp) $ do
            test id SchoolSpace
            test id (ClassSpace (SchoolClass 2016 "7a"))

    regression imp

-- * Regression suite

regression :: PersistenceImpl -> Spec
regression imp = describe "regression" $ do
    describe "IdeaSpace in proto idea and saved idea should be the same" $
        addDbSpecProp imp
            "addIdea" getIdeas addIdea
            (\p i -> i ^. ideaLocation `shouldBe` p ^. protoIdeaLocation)


-- * Expectations

passes :: Expectation
passes = return ()
