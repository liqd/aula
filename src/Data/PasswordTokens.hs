{-# LANGUAGE TemplateHaskell  #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Data.PasswordTokens
where

import Control.Lens
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Map as Map

import Data.DoubleMap
import Types


-- * type constants

-- See: Data.Delegation for further explanation

type U = AUID User


-- * types

newtype Validity = Validity { _validityTimeOut :: Timestamp }
  deriving (Eq, Ord, Read, Show)

data PasswordTokens = PasswordTokens {
      _passwordTokens :: Map U (Map PasswordToken Validity)
    }
  deriving (Eq, Ord, Read, Show)

makeLenses ''PasswordTokens

isTimedOut :: Timestamp -> Validity -> Bool
isTimedOut ts (Validity v) = ts >= v


emptyPasswordTokens :: PasswordTokens
emptyPasswordTokens = PasswordTokens Map.empty

-- Add a new token to the tokens which will time out on the given timestamp
newPasswordToken :: U -> PasswordToken -> Timestamp -> PasswordTokens -> PasswordTokens
newPasswordToken u t ts (PasswordTokens pt) =
    PasswordTokens $ insertDoubleMap u t (Validity ts) pt

-- Checks if the token is valid and not expired at the given timestamp
checkValid :: U -> PasswordToken -> Timestamp -> PasswordTokens -> PasswordTokenState
checkValid u t ts (PasswordTokens pt) =
    maybe Invalid (\t' -> if isTimedOut ts t' then TimedOut else Valid) $ lookupDoubleMap u t pt

-- Clears the timed out tokens for the given user
clearTimeoutTokens :: U -> Timestamp -> PasswordTokens -> PasswordTokens
clearTimeoutTokens u ts (PasswordTokens pt) =
    PasswordTokens $ pt & at u . _Just %~ Map.filter (not . isTimedOut ts)

-- Checks for timeout tokens
-- Returns True if there is any
checkForTimeoutTokens :: U -> Timestamp -> PasswordTokens -> Bool
checkForTimeoutTokens u ts (PasswordTokens pt) =
    maybe False (any (isTimedOut ts) . Map.elems) $ Map.lookup u pt

removeToken :: U -> PasswordToken -> PasswordTokens -> PasswordTokens
removeToken u t (PasswordTokens pt) =
    PasswordTokens $ deleteDoubleMap u t pt


-- * behavior

insertPasswordToken :: U -> PasswordToken -> Timestamp -> Timespan -> PasswordTokens -> PasswordTokens
insertPasswordToken u token now later =
    newPasswordToken u token (addTimespan later now)
    . clearTimeoutTokens u now

removePasswordToken :: U -> PasswordToken -> Timestamp -> PasswordTokens -> PasswordTokens
removePasswordToken u token now =
    removeToken u token . clearTimeoutTokens u now

-- * acid-state

deriveSafeCopy 0 'base ''Validity
deriveSafeCopy 0 'base ''PasswordTokens
