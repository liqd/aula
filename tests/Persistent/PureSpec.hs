{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.PureSpec where

import Data.Either (isRight)
import Data.String.Conversions (ST, cs)
import qualified Data.Text as ST (all)

import Types.Core (usernameAllowedChar)
import Frontend.Validation (FieldValidator, testValidator, usernameV)
import Persistent.Pure (genUserLoginFromRealname)

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Missing (manyNM)

-- | The number of elements under inspection as the
-- genUserLoginFromRealname could generate many.
sampleUsernameSize :: Int
sampleUsernameSize = 5

spec :: Spec
spec = do
    describe "genUserLoginFromRealname" $ do

        let checkValidUsernames  = all (ST.all usernameAllowedChar)
            checkValidUsernames' = all (isRight . testValidator (usernameV :: FieldValidator ST String))
            validInvalidChars   = oneof
                [ elements ['a'..'z']
                , elements ['A'..'Z']
                , elements "_- "
                , elements "@#$%^&*()!"
                ]
            validInvalidRealnames =
                (,) <$> manyNM 3 6 validInvalidChars
                    <*> manyNM 3 6 validInvalidChars

        it "generates non-empty list and the first 5 items contain allowed characters only." $ do
            genUserLoginFromRealname "_a_" "b-b"
                `shouldSatisfy`
                ((&&) <$> not . null
                      <*> checkValidUsernames . take sampleUsernameSize)

        it "generates usernames that the usernameV can validate" . property .
            forAll validInvalidRealnames $ \(fn, ln) ->
                checkValidUsernames' . take sampleUsernameSize $ genUserLoginFromRealname (cs fn) (cs ln)
