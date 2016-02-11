module Api.PersistentSpec where

import Arbitrary
import Frontend.Html

import Control.Applicative ((<$>))
import Data.Typeable (Typeable, typeOf)

import Test.Hspec -- (Spec, describe, it)
import Test.QuickCheck -- (Arbitrary(..), Gen, Testable, forAll, property)

import Api.Persistent


spec :: Spec
spec = describe "Api.Persistent" $ do
    it "does something" $ True `shouldBe` True
