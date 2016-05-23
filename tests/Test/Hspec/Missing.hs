{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.Missing where

import Data.Monoid
import Data.String.Conversions (ST, cs)

import Test.Hspec (SpecWith, describe)


class Tag t where
    tagText :: t -> ST

tag :: Tag t => t -> SpecWith a -> SpecWith a
tag t = describe (cs ("@" <> tagText t))
