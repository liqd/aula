-- | Pull in everything under `../src/`.
-- This makes the test suite type-check all modules when working with sensei.
module AllModulesSpec
where

import Test.Hspec (Spec, describe, it, shouldBe)

-- to update the import list, run this:
-- find ../src/ -name '*.hs' | perl -ne 'chomp; s!^../src/!!; s!/!.!g; s!.hs$!!; print "import $_ ()\n"' | sort
import Action ()
import Action.Dummy ()
import Action.Implementation ()
import Action.Smtp ()
import Arbitrary ()
import Backend ()
import Config ()
import Daemon ()
import Data.Avatar ()
import Data.UriPath ()
import DemoData ()
import Frontend ()
import Frontend.Core ()
import Frontend.Filter ()
import Frontend.Fragment.Category ()
import Frontend.Fragment.Comment ()
import Frontend.Fragment.Feasibility ()
import Frontend.Fragment.IdeaList ()
import Frontend.Fragment.Note ()
import Frontend.Fragment.QuorumBar ()
import Frontend.Fragment.VotesBar ()
import Frontend.Page ()
import Frontend.Page.Admin ()
import Frontend.Page.Delegation ()
import Frontend.Page.Idea ()
import Frontend.Page.Login ()
import Frontend.Page.Overview ()
import Frontend.Page.Static ()
import Frontend.Page.Topic ()
import Frontend.Page.User ()
import Frontend.Path ()
import Frontend.Prelude ()
import Frontend.Testing ()
import Frontend.Validation ()
import LifeCycle ()
import Logger ()
import Logger.EventLog ()
import Lucid.Missing ()
import Persistent ()
import Persistent.Api ()
import Persistent.Idiom ()
import Persistent.Implementation ()
import Persistent.Implementation.AcidState ()
import Persistent.Pure ()
import Persistent.TemplateHaskell ()
import Types ()

spec :: Spec
spec = describe "check types of all modules" . it "works" $ True `shouldBe` True
