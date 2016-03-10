module AllModulesSpec
where

import Test.Hspec (Spec, describe, it, shouldBe)

import Action ()
import Api ()
import Arbitrary ()
import Config ()
import CreateRandom ()
import Data.UriPath ()
import Frontend.Core ()
import Frontend ()
import Frontend.Page.Admin ()
import Frontend.Page.Comment ()
import Frontend.Page.Delegation ()
import Frontend.Page.FileUpload ()
import Frontend.Page ()
import Frontend.Page.Idea ()
import Frontend.Page.Login ()
import Frontend.Page.Overview ()
import Frontend.Page.Static ()
import Frontend.Page.Topic ()
import Frontend.Page.User ()
import Frontend.Path ()
import Frontend.Prelude ()
import Lucid.Missing ()
import Persistent ()
import Types ()

spec :: Spec
spec = describe "check types of all modules" . it "works" $ True `shouldBe` True
