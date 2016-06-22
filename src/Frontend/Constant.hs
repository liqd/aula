{-# LANGUAGE OverloadedStrings #-}
module Frontend.Constant
where

import Data.String.Conversions (ST)

-- FIXME: Search for global constants

topicDescMaxLength :: Int
topicDescMaxLength = 120

minElabPeriod :: Int
minElabPeriod = 1

maxElabPeriod :: Int
maxElabPeriod = 366

minUsernameLength :: Int
minUsernameLength = 4

maxUsernameLength :: Int
maxUsernameLength = 12

minPasswordLength :: Int
minPasswordLength = 4

maxPasswordLength :: Int
maxPasswordLength = 120

initialDemoPassword :: ST
initialDemoPassword = "1234"

avatarDefaultSize :: Int
avatarDefaultSize = 100

avatarExtraSizes :: [Int]
avatarExtraSizes = [64, 300]
