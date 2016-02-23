module AllModulesSpec
where

import Test.Hspec (Spec)

import Action()
import Api()
import Api.Persistent()
import Arbitrary()
import Config()
import CreateRandom()
import Frontend()
import Frontend.Core()
import Frontend.Html()
import Frontend.Page()
import Frontend.Page.CreateIdea()
import Frontend.Page.Login()
import Frontend.Page.Static()
import Frontend.Page.Topic()
import Frontend.Path()
import Frontend.Prelude()
import Persistent()
import Transaction()
import Types()

spec :: Spec
spec = return ()
