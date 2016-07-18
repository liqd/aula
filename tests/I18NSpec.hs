{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module I18NSpec where

import Test.Hspec
import I18N ()

spec :: Spec
spec = describe "I18N executable" . it "works" $ pendingWith "no tests yet, sorry!"
