{-# LANGUAGE FlexibleContexts    #-}
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
import Control.Monad.Reader
import Control.Monad.Trans.Except (runExceptT)
import Data.String.Conversions
import Servant.Server
import Test.Hspec
import Test.QuickCheck

import CreateRandom
import Config
import Persistent
import Persistent.Api
import Persistent.Implementation
import Types

import AulaTests (testConfig)

-- TODO decide if we use Action here or not:
-- If we do:
--   - We somewhat pull too much things that what we want to test
--   - Instead of AUpdate we have these event types everywhere
--   + Simpler
-- If we don't:
--   + We precisely test what is in Persistent.Pure (so we'll need to rename that module)
--   - We need to adapt genInitialTestDb to either be an update or we run it
--     with the action implementation and then extract the final state


-- FIXME: use @withPersist@ (instead of @before/it@?)

-- | a database state containing one arbitrary item of each type (idea, user, ...)
mkInitial :: PersistenceImpl -> IO RunPersist
mkInitial imp = do
    rp <- mkEmpty imp
    runA rp genInitialTestDb
    pure rp

-- | the empty database
mkEmpty :: PersistenceImpl -> IO RunPersist
mkEmpty imp = do
    cfg <- (persistenceImpl .~ imp) <$> testConfig
    mkRunPersist cfg

runPclose = withPersist' . pure

runQ :: MonadIO m => RunPersist -> AQuery a -> m a
runQ rp q = liftIO $ runReader q <$> rp ^. rpQuery

runU :: (MonadIO m) => RunPersist -> AUpdate a -> m a
runU rp u = liftIO $ (rp ^. rpUpdate) u

runA :: (MonadIO m, ActionM n) => RunPersist -> n a -> m a
runA rp m = error "TODO runA"

getDbSpec :: (Eq a, Show a) => PersistenceImpl -> String -> AQuery [a] -> Spec
getDbSpec imp name getXs = do
    describe name $ do
        context "on empty database" . before (mkEmpty imp) $ do
            it "returns the empty list" $ \rp' -> runPclose rp' (\rp -> do
                xs <- runQ rp getXs
                xs `shouldBe` [])
        context "on initial database" . before (mkInitial imp) $ do
            it "returns a non-empty list" $ \rp' -> runPclose rp' (\rp -> do
                xs <- runQ rp getXs
                length xs `shouldNotBe` 0)

addDbSpecProp :: forall f proto a.
                 (Foldable f, Arbitrary proto)
              => PersistenceImpl
              -> String
              -> AQuery (f a)
              -> (EnvWith proto -> AUpdate a)
              -> (proto -> a -> Expectation)
              -> Spec
addDbSpecProp imp name getXs addX propX =
    describe name $ do
        let t :: SpecWith RunPersist
            t = it "adds one" $ \rp' -> runPclose rp' (\rp -> do
                    before' <- length <$> runQ rp getXs
                    (now, p) <- liftIO $ generate arbitrary
                    r <- runU rp $ addX (EnvWith frameUserHack now p)
                    after' <- length <$> runQ rp getXs
                    after' `shouldBe` before' + 1
                    propX p r)

        context "on empty database" . before (mkEmpty imp) $ t
        context "on initial database" . before (mkInitial imp) $ t

addDbSpec :: (Foldable f, Arbitrary proto) =>
             PersistenceImpl -> String -> AQuery (f a) -> (EnvWith proto -> AUpdate a) -> Spec
addDbSpec imp name getXs addX = addDbSpecProp imp name getXs addX (\_ _ -> passes)

findInBySpec :: (Eq a, Show a, Arbitrary k) =>
                PersistenceImpl -> String -> AQuery [a] -> (k -> AQuery (Maybe a)) ->
                Fold a k -> (k -> k) ->
                Spec
findInBySpec imp name getXs findXBy f change =
    describe name $ do
        context "on empty database" . before (mkEmpty imp) $ do
            it "will come up empty" $ \rp' -> runPclose rp' (\rp -> do
                rf <- liftIO $ generate arbitrary
                mu <- runQ rp $ findXBy rf
                mu `shouldBe` Nothing)

        context "on initial database" . before (mkInitial imp) $ do
            context "if it does not exist" $ do
                it "will come up empty" $ \rp' -> runPclose rp' (\rp -> do
                    (x:_) <- runQ rp getXs
                    let Just y = x ^? f
                    mu <- runQ rp $ findXBy (change y)
                    mu `shouldBe` Nothing)
            context "if it exists" $ do
                it "will come up with the newly added record" $ \rp' -> runPclose rp' (\rp -> do
                    (x:_) <- runQ rp getXs
                    let Just y = x ^? f
                    mu <- runQ rp $ findXBy y
                    mu `shouldBe` Just x)

findAllInBySpec :: (Eq a, Show a) =>
                    PersistenceImpl -> String -> AQuery [a] -> AQuery (Gen k) -> (k -> AQuery [a]) ->
                    Fold a k -> (k -> k) ->
                    Spec
findAllInBySpec imp name getXs genKs findAllXBy f change =
    describe name $ do
        context "on empty database" . before (mkEmpty imp) $ do
            it "will come up empty" $ \rp' -> runPclose rp' (\rp -> do
                genK <- runQ rp genKs
                rf <- liftIO $ generate genK
                us <- runQ rp $ findAllXBy rf
                us `shouldBe` [])

        context "on initial database" . before (mkInitial imp) $ do
            context "if it does not exist" $ do
                it "will come up empty" $ \rp' -> runPclose rp' (\rp -> do
                    [x] <- runQ rp getXs
                    let Just y = x ^? f
                    us <- runQ rp $ findAllXBy (change y)
                    us `shouldBe` [])
            context "if it exists" $ do
                it "will come up with the newly added record" $ \rp' -> runPclose rp' (\rp -> do
                    [x] <- runQ rp getXs
                    let [y] = x ^.. f
                    us <- runQ rp $ findAllXBy y
                    us `shouldBe` [x])

-- Given an AUID pick a different one
changeAUID :: AUID a -> AUID a
changeAUID (AUID i) = AUID (succ i)

spec :: Spec
spec = do
    -- context "Using STM"             $ persistApiSpec STM
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
    addDbSpec imp "addUsers" getUsers (addUser (error "UserPass"))

    getDbSpec imp "getTopics" getTopics
    addDbSpec imp "addTopics" getTopics addTopic

    findInBySpec imp "findUserByLogin" getUsers findUserByLogin userLogin ("not" <>)
    findInBySpec imp "findTopic" getTopics findTopic _Id changeAUID

    let elements' [] = arbitrary
        elements' xs = elements xs
        getArbTopicIds :: AQuery (Gen (AUID Topic))
        getArbTopicIds = elements' . map (view _Id) <$> getTopics
    findAllInBySpec imp "findIdeasByTopicId"
        getIdeasWithTopic getArbTopicIds findIdeasByTopicId ideaTopicId changeAUID

    describe "addIdeaSpace" $ do
        let test :: (Int -> Int) -> IdeaSpace -> SpecWith RunPersist
            test upd ispace = do
                it ("can add " <> showIdeaSpace ispace) $ \rp' -> runPclose rp' (\rp -> do
                    let getL = runQ rp getSpaces
                        addS = runU rp $ addIdeaSpaceIfNotExists ispace
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
