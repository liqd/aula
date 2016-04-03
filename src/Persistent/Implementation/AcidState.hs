{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE UndecidableInstances            #-}
{-# LANGUAGE GADTs                       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TupleSections               #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE ViewPatterns                #-}

-- {-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Persistent.Implementation.AcidState
    ( mkRunPersistOnDisk
    , mkRunPersistInMemory
    )
where

import Control.Exception (assert)
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except  -- (MonadError)
import Control.Monad.Trans.Except  -- (ExceptT(ExceptT), runExceptT)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Acid  -- (AcidState, Query, Update, closeAcidState, makeAcidic, query, update)
import Data.Acid.Core
import Data.Acid.Local (openLocalStateFrom, createCheckpointAndClose)
import Data.Acid.Memory (openMemoryState)
import Control.Monad.Trans.Reader (ReaderT(ReaderT), runReaderT)
import Servant.Server ((:~>)(Nat))

import Config
import Persistent.Pure
import Persistent.Idiom
import Persistent.Api
import Types


type PersistC xe x a r = (xe a, a ~ x AulaData r, MethodState a ~ AulaData, MethodResult a ~ r)

data Persist a where
    PersistQ :: (PersistC QueryEvent  Query  a r) => AQuery  a -> Persist a
    PersistU :: (PersistC UpdateEvent Update a r) => AUpdate a -> Persist a

instance (PersistC xe x a r) => Functor Persist where
--    fmap = assert False $ undefined  -- TODO
    fmap f (PersistQ q) = PersistQ (fmap f q)
    fmap f (PersistU u) = PersistU (fmap f u)

instance Applicative Persist where
    pure  = assert False $ undefined  -- TODO
    (<*>) = assert False $ undefined  -- TODO
{-
    pure = PersistQ . pure
    PersistQ ab <*> PersistQ a = PersistQ $            ab <*>            a
    PersistQ ab <*> PersistU a = PersistU $ liftAQuery ab <*>            a
    PersistU ab <*> PersistQ a = PersistU $            ab <*> liftAQuery a
    PersistU ab <*> PersistU a = PersistU $            ab <*>            a
-}

instance Monad Persist where
    return = pure
    (>>=) = assert False $ undefined  -- TODO

instance MonadError PersistExcept Persist where
    throwError = assert False $ undefined  -- TODO
    catchError = assert False $ undefined  -- TODO


mkRunPersistGeneric :: String -> (AulaData -> IO (AcidState AulaData))
                    -> (AcidState AulaData -> IO ())
                    -> IO RunPersist
mkRunPersistGeneric desc openState closeState = do
    db <- openState emptyAulaData

    let run :: Persist a -> ExceptT PersistExcept IO a
        run (PersistQ q) = query  db <$> q
        run (PersistU u) = update db <$> u

    pure RunPersist { _rpDesc  = desc
                    , _rpNat   = Nat run
                    , _rpClose = closeState db
                    }

mkRunPersistOnDisk :: Config -> IO RunPersist
mkRunPersistOnDisk cfg =
    mkRunPersistGeneric "acid-state (disk)"
        (openLocalStateFrom $ cfg ^. dbPath) createCheckpointAndClose

mkRunPersistInMemory :: IO RunPersist
mkRunPersistInMemory =
    mkRunPersistGeneric "acid-state (memory)"
        openMemoryState closeAcidState
