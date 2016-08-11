{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Types.Log
where

import Data.Yaml
import Data.String.Conversions (ST)
import GHC.Generics


data LogLevel
    = DEBUG  -- ^ Too much noise
    | INFO   -- ^ Regular system behavior
    | WARN   -- ^ Bad smells, login errors, authentication error
    | ERROR  -- ^ Exceptions, requires investigation, 404
    | NOLOG  -- ^ Utter silence (do not log any messages with this level!)
  deriving (Eq, Generic, Ord, Show, ToJSON, FromJSON)

class LogMessage l where
    logLevel   :: l -> LogLevel
    logMessage :: l -> ST
