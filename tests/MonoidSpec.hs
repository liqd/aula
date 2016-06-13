{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module MonoidSpec
where

import Data.Typeable
import Test.QuickCheck (Arbitrary, Gen, property)

import Data.UriPath

import Arbitrary (arb, forAllShrinkDef)
import AulaTests


spec :: Spec
spec = tag Large $ do
    mapM_ monoidSpec
        [ M (arb :: Gen Document)
--        , Proxy :: Proxy UriPath -- FIXME
        , M (arb :: Gen UriPart')
        , M (arb :: Gen RoleScope)
        ]

newtype UriPart' = UriPart' UriPart
  deriving (Monoid, Arbitrary)

instance Show UriPart' where
    show (UriPart' x) = cs x

instance Eq UriPart' where
    (UriPart' x) == (UriPart' y) = (cs x :: String) == cs y

data MonoidProxy where
    M :: (Arbitrary m, Eq m, Monoid m, Show m, Typeable m)
      => Gen m -> MonoidProxy

monoidSpec :: MonoidProxy -> Spec
monoidSpec (M g) = do
    describe ("Monoid " <> show (typeOf g)) $ do
        it "nil <> x === x" . property . forAllShrinkDef g $
            \x -> nil <> x `shouldBe` x
        it "x <> nil === x" . property . forAllShrinkDef g  $
            \x -> x <> nil `shouldBe` x
        it "x <> (y <> z) === (x <> y) <> z" . property . forAllShrinkDef g  $
            \x y z -> x <> (y <> z) `shouldBe` (x <> y) <> z
