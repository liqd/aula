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

spec :: Spec
spec = do
    describe "getIdeas" $ do
        context "on empty database" . before mkEmpty $ do
            it "returns the empty list" $ \(Nat rp) -> do
                ideas <- rp getIdeas
                ideas `shouldBe` []
        context "on initial database" . before mkInitial $ do
            it "returns a list with one element" $ \(Nat rp) -> do
                ideas <- rp getIdeas
                length ideas `shouldBe` 1

    describe "addIdea" $ do
        let t = it "adds an idea" $ \(Nat rp) -> do
                    before' <- liftIO $ length <$> rp getIdeas
                    liftIO $ generate arbitrary >>= rp . addIdea
                    after' <- liftIO $ length <$> rp getIdeas
                    after' `shouldBe` before' + 1

        context "on empty database" . before mkEmpty $ t
        context "on initial database" . before mkInitial $ t

    describe "getUsers" $ do
        context "on empty database" . before mkEmpty $ do
            it "returns the empty list" $ \(Nat rp) -> do
                users <- rp getUsers
                users `shouldBe` []
        context "on initial database" . before mkInitial $ do
            it "returs a non-empty list" $ \(Nat rp) -> do
                users <- rp getUsers
                length users `shouldNotBe` 0

    describe "addUser" $ do
        let t = it "adds a user" $ \(Nat rp) -> do
                    before' <- liftIO $ length <$> rp getUsers
                    liftIO $ generate arbitrary >>= rp . addUser
                    after' <- liftIO $ length <$> rp getUsers
                    after' `shouldBe` before' + 1

        context "on empty database" . before mkEmpty $ t
        context "on initial database" . before mkInitial $ t

    describe "findUserByLogin" $ do
        context "on empty database" . before mkEmpty $ do
            it "will come up empty" $ \(Nat rp) -> do
                mu <- liftIO . rp $ findUserByLogin "samedifference"
                mu `shouldBe` Nothing

        context "on initial database" . before mkInitial $ do
            context "if user does not exist" $ do
                it "will come up empty" $ \(Nat rp) -> do
                    [user] <- liftIO $ rp getUsers
                    mu <- liftIO . rp $ findUserByLogin ("not" <> (user ^. userLogin))
                    mu `shouldBe` Nothing
            context "if user does exist" $ do
                it "will come up with the user" $ \(Nat rp) -> do
                    [user] <- liftIO $ rp getUsers
                    mu <- liftIO . rp $ findUserByLogin (user ^. userLogin)
                    mu `shouldBe` (Just user)

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
