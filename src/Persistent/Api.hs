{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GADTs                       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TupleSections               #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE ViewPatterns                #-}

-- {-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Persistent.Api
    ( RunPersistT(..)
    , RunPersist
    , module Persistent.Pure
    , module Persistent.Idiom
    )
where

import Control.Exception (finally)
import Control.Lens
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Except (ExceptT)
import Control.Monad (unless, replicateM, when)
import Data.Acid
import Data.Acid.Core
import Data.Elocrypt (mkPassword)
import Data.Foldable (find, for_)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Set (Set)
import Data.String.Conversions (ST, cs, (<>))
import Data.Time.Clock (getCurrentTime)
import Data.Typeable (Typeable)
import Servant (ServantErr)
import Servant.Missing (ThrowError500(..))
import Servant.Server ((:~>))

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as ST

import Types
import Persistent.Pure
import Persistent.Idiom


-- | well, not really 'Nat' any more.  too many constraints.
type RunPersistNat m r a = r a -> ExceptT PersistExcept m a

type PersistC  a r = (MethodState a ~ AulaData, MethodResult a ~ r)
type PersistCQ a r = (a ~ Query  AulaData r, QueryEvent  a, PersistC a r)
type PersistCU a r = (a ~ Update AulaData r, UpdateEvent a, PersistC a r)


data RunPersistT m =
      forall q u r. (PersistCQ q r, PersistCU u r) =>
        RunPersist
                  { _rpDesc  :: String
                  , _rpQNat  :: RunPersistNat m AQuery  q
                  , _rpUNat  :: RunPersistNat m AUpdate u
                  , _rpClose :: m ()
                  }

type RunPersist = RunPersistT IO


$(makeAcidic ''AulaData [])
