{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Persistent.ApiSpec where

import Control.Exception (ErrorCall(ErrorCall), throwIO, finally)
import Control.Lens hiding (elements)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Except (runExceptT)
import Data.Acid
import Data.Acid.Core
import Data.String.Conversions
import Servant.Server
import Test.Hspec
import Test.QuickCheck

import Arbitrary ()
import Action  -- TODO: qualified
import Action.Implementation  -- TODO: qualified
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
mkInitial = mkState MkStateInitial

-- | the empty database
mkEmpty :: PersistenceImpl -> IO RunPersist
mkEmpty = mkState MkStateEmpty

data MkStateSetup = MkStateEmpty | MkStateInitial
  deriving (Eq, Show)

mkState :: MkStateSetup -> PersistenceImpl -> IO RunPersist
mkState setup impl = do
    cfg <- (persistenceImpl .~ impl) <$> testConfig
    rp  <- mkRunPersist cfg
    case setup of
        MkStateEmpty   -> pure ()
        MkStateInitial -> runA cfg rp genInitialTestDb
    pure rp

runA :: Config -> RunPersist -> Action a -> IO a
runA cfg rp = fmap (either (error . show) id) . runExceptT . unNat (mkRunAction (ActionEnv rp cfg))

runPclose = withPersist' . pure  -- FIXME: remove!

runQ :: (MonadIO m) => RunPersist -> AQuery a -> m a
runQ rp q = liftIO $ runReader q <$> rp ^. rpQuery

runU :: (MonadIO m, HasAUpdate ev a) => RunPersist -> ev -> m (Either PersistExcept a)
runU rp u = liftIO $ (rp ^. rpUpdate) u


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

addDbSpecProp :: forall f proto ev a.
                 (Foldable f, Arbitrary proto, HasAUpdate ev a)
              => PersistenceImpl
              -> String
              -> AQuery (f a)
              -> (EnvWith proto -> ev)
              -> (proto -> Either PersistExcept a -> Expectation)
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

addDbSpec :: (Foldable f, Arbitrary proto, HasAUpdate ev a)
          => PersistenceImpl -> String -> AQuery (f a) -> (EnvWith proto -> ev) -> Spec
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
    addDbSpec imp "addIdea"  getIdeas AddIdea
    getDbSpec imp "getWildIdeas"      getWildIdeas
    getDbSpec imp "getIdeasWithTopic" getIdeasWithTopic

    getDbSpec imp "getUsers" getUsers
    addDbSpec imp "addUsers" getUsers (AddUser (error "UserPass"))

    getDbSpec imp "getTopics" getTopics
    addDbSpec imp "addTopics" getTopics AddTopic

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
                        addS = runU rp $ AddIdeaSpaceIfNotExists ispace
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
            "addIdea" getIdeas AddIdea
            (\p (Right i) -> i ^. ideaLocation `shouldBe` p ^. protoIdeaLocation)


-- * Expectations

passes :: Expectation
passes = return ()
