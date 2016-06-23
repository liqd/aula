{-# LANGUAGE LambdaCase #-}
module Data.PasswordTokensSpec
where

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck

import Data.PasswordTokens
import Arbitrary (arb, shr)
import Types

spec :: Spec
spec = do
    -- TODO better descriptions
    describe "Reset Password Token store properties" $ do

        it "New password token allways valid at insertion" .
            property .
                forAllShrink arb shr $ \u ->
                forAllShrink arb shr $ \token ->
                forAllShrink arb shr $ \timestamp ->
                forAll (nonNegativeTimespan `suchThat` (not . zeroTimespan)) $ \timespan ->
                forAllShrink arb shr $ \pt ->
                    newPasswordTokenAllwaysValidAtInsertion u token timestamp timespan pt

        it "New password token allways invalid after timeout" .
                forAllShrink arb shr $ \u ->
                forAllShrink arb shr $ \token ->
                forAllShrink arb shr $ \timestamp ->
                forAll (nonNegativeTimespan `suchThat` (not . zeroTimespan)) $ \timespan ->
                forAllShrink arb shr $ \pt ->
                    newPasswordTokenAllwaysInvalidAfterTimeout u token timestamp timespan pt

        it "Remove token invalidates token" $
            property removeTokenInvalidatesToken

        it "Clear timeout tokens removes tokens" $
            property clearTimeoutTokensRemovesTokens

isTimedOutForLater :: Timespan -> Timestamp -> Bool
isTimedOutForLater later now = isTimedOut (addTimespan later now) (Validity now)


newPasswordTokenAllwaysValidAtInsertion :: U -> PasswordToken -> Timestamp -> Timespan -> PasswordTokens -> Bool
newPasswordTokenAllwaysValidAtInsertion u t now later =
    isValid . checkValid u t now . newPasswordToken u t (addTimespan later now)

newPasswordTokenAllwaysInvalidAfterTimeout :: U -> PasswordToken -> Timestamp -> Timespan -> PasswordTokens -> Bool
newPasswordTokenAllwaysInvalidAfterTimeout u t now later =
    not . isValid . checkValid u t (addTimespan later now) . newPasswordToken u t (addTimespan later now)

removeTokenInvalidatesToken :: U -> PasswordToken -> Timestamp -> PasswordTokens -> Bool
removeTokenInvalidatesToken u t ts =
    not . isValid . checkValid u t ts . removeToken u t . newPasswordToken u t ts

clearTimeoutTokensRemovesTokens :: U -> Timestamp -> PasswordTokens -> Property
clearTimeoutTokensRemovesTokens u ts pt =
    (if checkForTimeoutTokens u ts pt
        then label "There are timeout tokens"
        else label "There were no timeout tokens")
    . not . checkForTimeoutTokens u ts $ clearTimeoutTokens u ts pt

isValid :: PasswordTokenState -> Bool
isValid Valid = True
isValid _     = False

zeroTimespan :: Timespan -> Bool
zeroTimespan = (0 ==) . timespanUs

nonNegativeTimespan :: Gen Timespan
nonNegativeTimespan = tabs <$> arb
  where
    tabs = \case
        TimespanUs    n -> TimespanUs    (abs n)
        TimespanMs    n -> TimespanMs    (abs n)
        TimespanSecs  n -> TimespanSecs  (abs n)
        TimespanMins  n -> TimespanMins  (abs n)
        TimespanHours n -> TimespanHours (abs n)
        TimespanDays  n -> TimespanDays  (abs n)
