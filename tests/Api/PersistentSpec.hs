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
    generate arbitrary >>= unNat rp . addIdea
    generate arbitrary >>= unNat rp . addUser
    return rp

getDbSpec name getXs = do
    describe name $ do
        context "on empty database" . before mkEmpty $ do
            it "returns the empty list" $ \(Nat rp) -> do
                xs <- rp getXs
                xs `shouldBe` []
        context "on initial database" . before mkInitial $ do
            it "returns a list with one element" $ \(Nat rp) -> do
                xs <- rp getXs
                length xs `shouldBe` 1

addDbSpec name getXs addX =
    describe name $ do
        let t = it "adds one" $ \(Nat rp) -> do
                    before' <- liftIO $ length <$> rp getXs
                    liftIO $ generate arbitrary >>= rp . addX
                    after' <- liftIO $ length <$> rp getXs
                    after' `shouldBe` before' + 1

        context "on empty database" . before mkEmpty $ t
        context "on initial database" . before mkInitial $ t

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
                    [x] <- liftIO $ rp getXs
                    let Just y = x ^? f
                    mu <- liftIO . rp $ findXBy (change y)
                    mu `shouldBe` Nothing
            context "if it exists" $ do
                it "will come up with the newly added record" $ \(Nat rp) -> do
                    [x] <- liftIO $ rp getXs
                    let Just y = x ^? f
                    mu <- liftIO . rp $ findXBy y
                    mu `shouldBe` (Just x)

spec :: Spec
spec = do
    getDbSpec "getIdeas" getIdeas
    addDbSpec "addIdea"  getIdeas addIdea

    getDbSpec "getUsers" getUsers
    addDbSpec "addUsers" getUsers addUser

    findInBySpec "findUserByLogin" getUsers findUserByLogin userLogin ("not" <>)

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
                    [user] <- liftIO $ rp getUsers
                    t rp ("not" <> (user ^. userLogin)) isNothing

            context "if user does exist" $ do
                context "if password is wrong" $ do
                    it "will not log you in" $ \_rp -> do
                        pendingWith "this prototype doesn't do this yet."

                context "if password is correct" $ do
                    it "will indeed log you in (yeay)" $ \(Nat rp) -> do
                        [user] <- liftIO $ rp getUsers
                        t rp (user ^. userLogin) isJust
