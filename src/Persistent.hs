{-# OPTIONS_GHC -Werror -Wall #-}

-- | Placeholder module for separate DSL and other
-- high level combinators.
module Persistent (
      module Persistent.Api
    , module Persistent.Pure
    , module Persistent.Idiom
    , module Persistent.Implementation
    )
where

import Persistent.Api
import Persistent.Pure
import Persistent.Idiom
import Persistent.Implementation
