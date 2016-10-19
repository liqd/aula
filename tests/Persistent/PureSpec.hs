{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Persistent.PureSpec where

import Data.Either (isRight)
import Data.String.Conversions (ST, cs)
import qualified Data.Text as ST (all)

import Types.Core (usernameAllowedChar, UserFirstName(..), UserLastName(..))
import Frontend.Validation (FieldValidator, testValidator, usernameV)
import Persistent.Pure (genUserLoginCandidates)

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Missing (manyNM)


-- | The number of elements under inspection as
-- 'genUserLoginCandidates' output is practially infinite.
sampleUsernameSize :: Int
sampleUsernameSize = 15

spec :: Spec
spec = do
    describe "genUserLoginFromRealname" $ do

        let checkValidUsernames  = all (ST.all usernameAllowedChar)
            checkValidUsernames' = all (isRight . testValidator (usernameV :: FieldValidator ST String))

            validChars = oneof
                [ elements ['a'..'z']
                , elements ['A'..'Z']
                , elements "_- "
                ]
            invalidChars = oneof
                [ elements "@#$%^&*()!"
                ]
            validOrInvalidChars = oneof [validChars, invalidChars]

            validOrInvalidRealnames =
                (,) <$> manyNM 3 6 validOrInvalidChars
                    <*> manyNM 3 6 validOrInvalidChars

        it "generates non-empty list and the first 5 items contain allowed characters only." $ do
            genUserLoginCandidates "_a_" "b-b"
                `shouldSatisfy`
                ((&&) <$> not . null
                      <*> checkValidUsernames . take sampleUsernameSize)

        it "generates usernames that the usernameV can validate" . property .
            forAll validOrInvalidRealnames $ \(fn, ln) ->
                checkValidUsernames' . take sampleUsernameSize $
                    genUserLoginCandidates (UserFirstName $ cs fn) (UserLastName $ cs ln)
