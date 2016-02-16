{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeOperators      #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Api.PersistentSpec where

import Arbitrary ()
import Control.Lens
import Control.Monad.IO.Class
import Data.Maybe
import Data.String.Conversions
import Servant.Server
import Test.Hspec
import Test.QuickCheck

import Api.Persistent
import Types


-- | the empty database
mkEmpty :: IO (Persist :~> IO)
mkEmpty = mkRunPersist

-- | a database state containing one arbitrary item of each type (idea, user, ...)
mkInitial :: IO (Persist :~> IO)
mkInitial = do
    rp <- mkEmpty
    generate arbitrary >>= bootstrapUser rp
    _wildIdea <- generate arbitrary >>= unNat rp . addIdea
    topicIdea <- generate arbitrary >>= unNat rp . addIdea
    generate arbitrary >>= unNat rp . addTopic . (protoTopicIdeas .~ [topicIdea ^. _Id])
    return rp

-- | FIXME: who will create the first user?
bootstrapUser :: (Persist :~> IO) -> User -> IO User
bootstrapUser (Nat rp) protoUser = rp $ forceLogin uid >> addUser (tweak protoUser)
  where
    uid :: Integer
    uid = 0

    tweak :: User -> User
    tweak user = (userMeta . metaId .~ AUID 0)
               . (userMeta . metaCreatedByLogin .~ (user ^. userLogin))
               $ user

getDbSpec :: (Eq a, Show a) => String -> Persist [a] -> Spec
getDbSpec name getXs = do
    describe name $ do
        context "on empty database" . before mkEmpty $ do
            it "returns the empty list" $ \(Nat rp) -> do
                xs <- rp getXs
                xs `shouldBe` []
        context "on initial database" . before mkInitial $ do
            it "returs a non-empty list" $ \(Nat rp) -> do
                xs <- rp getXs
                length xs `shouldNotBe` 0

addDbSpec :: (Foldable f, Arbitrary proto) => String -> Persist (f a) -> (proto -> Persist a) -> Spec
addDbSpec name getXs addX =
    describe name $ do
        let t = it "adds one" $ \(Nat rp) -> do
                    before' <- liftIO $ length <$> rp getXs
                    liftIO $ generate arbitrary >>= rp . addX
                    after' <- liftIO $ length <$> rp getXs
                    after' `shouldBe` before' + 1

        context "on empty database" . before mkEmpty $ t
        context "on initial database" . before mkInitial $ t

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
                    mu `shouldBe` (Just x)

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
    addDbSpec "addIdea"  getIdeas addIdea

    getDbSpec "getUsers" getUsers
    addDbSpec "addUsers" getUsers addUser

    getDbSpec "getTopics" getTopics
    addDbSpec "addTopics" getTopics addTopic

    findInBySpec "findUserByLogin" getUsers findUserByLogin userLogin ("not" <>)
    findInBySpec "findTopic" getTopics findTopic _Id changeAUID
    let getIdeasWithTopic = filter (isJust . view ideaTopic) <$> getIdeas
    findAllInBySpec "findIdeasByTopicId" getIdeasWithTopic findIdeasByTopicId (ideaTopic . _Just) changeAUID

    describe "loginUser" $ do
        let t rp login predicate = do
                result <- liftIO . rp $ do
                    loginUser login
                    getDb dbCurrentUser
                result `shouldSatisfy` predicate

        context "on empty database" . before mkEmpty $ do
            it "will not log you in" $ \(Nat rp) -> t rp "nope" isNothing

        context "on initial database" . before mkInitial $ do
            context "if user does not exist" $ do
                it "will not log you in" $ \(Nat rp) -> do
                    user:_ <- liftIO $ rp getUsers
                    t rp ("not" <> (user ^. userLogin)) isNothing

            context "if user does exist" $ do
                context "if password is wrong" $ do
                    it "will not log you in" $ \_rp -> do
                        pendingWith "this prototype doesn't do this yet."

                context "if password is correct" $ do
                    it "will indeed log you in (yeay)" $ \(Nat rp) -> do
                        user:_ <- liftIO $ rp getUsers
                        t rp (user ^. userLogin) isJust
