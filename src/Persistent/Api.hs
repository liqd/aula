{-# LANGUAGE FlexibleContexts            #-}
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
    ( RunPersistNat
    , RunPersistT(..)
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


type RunPersistNat m r = r :~> ExceptT PersistExcept m

data RunPersistT m =
    forall r. -- (PersistM r, GenArbitrary r) =>
        RunPersist
                  { _rpDesc  :: String
                  , _rpNat   :: RunPersistNat m r
                  , _rpClose :: m ()
                  }

type RunPersist = RunPersistT IO



{-

getCurrentTimestampIO :: AUpdate Timestamp
getCurrentTimestampIO = Timestamp <$> getCurrentTime

mkRandomPasswordIO :: AUpdate UserPass
mkRandomPasswordIO = UserPassInitial . cs . unwords <$> mkPassword `mapM` [4,3,5]

-}
