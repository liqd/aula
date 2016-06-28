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

-- | Avatar URLs must be persitent, hence changing these constants requires some care
-- if the system is already in production.
--
-- Here are some guidelines:
-- * Adding new sizes to avatarExtraSizes is OK
-- * When updating avatarDefaultSize add the current avatarDefaultSize to avatarExtraSizes.
-- * Prefer the use of avatarUrl over avatarFile.
-- * Instead updating a size in avatarExtraSizes just add the new one.
avatarDefaultSize :: Int
avatarDefaultSize = 600

-- | See avatarDefaultSize.
avatarExtraSizes :: [Int]
avatarExtraSizes = [64, 100, 300]

-- | This is just for showing the information.  To enforce this limit, set up a reverse proxie like
-- nginx and configure it accordingly.  See ./docs/deployment.md for details.  FIXME: this should be
-- in Config.hs so changing nginx config doesn't make recompilation necessary.
avatarMaxByteSize :: ST
avatarMaxByteSize = "10MB"
