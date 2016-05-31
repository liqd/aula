{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DefaultSignatures           #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE InstanceSigs                #-}
{-# LANGUAGE KindSignatures              #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE Rank2Types                  #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE UndecidableInstances        #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -Wall -Werror #-}

{- | manage votes, delegations, visualize the delegation network, compute voting outcomes.

this is a preliminary draft of a module that should go into its own package, and become a library
dependency of aula.

-}
module Data.DelegatedVoting
where

import GHC.Generics (Generic)

import qualified Types
import qualified Action.Implementation as Action


-- * this part will go to its own general-purpose package

data Vote (m :: * -> *) v i user val =
  Vote
    { _voteContext         :: Ctx m v i
    , _voter               :: user
    , _voteDelegationChain :: [user]
    , _voteValue           :: val
    }
  deriving (Eq, Ord, Show, Read, Generic)

data Delegation (m :: * -> *) v i user =
  Delegation
    { _delegationContext :: Ctx m v i
    , _delegationFrom    :: user
    , _delegationTo      :: user
    }
  deriving (Eq, Ord, Show, Read, Generic)

data Ctx (m :: * -> *) v i = CtxVotable v | CtxInner i
  deriving (Eq, Ord, Show, Read, Generic)

data PollIsOpen = PollOpen | PollClosed
  deriving (Eq, Ord, Show, Read, Generic)


class ( Monad m
      , vote       ~ Vote       m (CtxVotable m) (CtxInner m) (User m) (VoteValue m)
      , delegation ~ Delegation m (CtxVotable m) (CtxInner m) (User m)
      , ctx        ~ Ctx        m (CtxVotable m) (CtxInner m)
      ) => Process m vote delegation ctx where

  type User m
  type VoteValue m  -- e.g., "YES | NO | NEUTRAL"
  type CtxVotable m  -- e.g., idea
  type CtxInner m  -- e.g., school class space
  type Key m v

  start          :: m ()
  start           = pure ()

  stop           :: m ()
  stop            = pure ()

  setCtx         :: Key m ctx -> ctx -> m ()
  rmCtx          :: Key m ctx -> m ()
  getContext     :: Key m ctx -> m ctx

  -- Networks of delegation contexts.  Sub-context-ship and super-context-ship are symmetrical.  If
  -- @a@ is sub-context of @a'@, then delegations in @a'@ are the default in @a@.  supers, subs
  -- constitute a partial ordering.  we can introduce a PartialCompare type for that and add
  -- assertions to supers, subs that those are satisfied.
  supers         :: ctx -> m [ctx]
  subs           :: ctx -> m [ctx]

  setVote        :: Key m (CtxVotable m) -> User m -> vote -> m ()
  rmVote         :: Key m (CtxVotable m) -> User m -> vote -> m ()
  getVotes       :: m [vote]

  setDelegation  :: Key m ctx -> User m -> User m -> m ()
  rmDelegation   :: Key m ctx -> User m -> User m -> m ()
  getDelegations :: m [delegation]

  getVotesByCtx :: Key m (CtxVotable m) -> m [vote]
  getVotesByCtx = undefined

  getDelegationsByCtx :: Key m ctx -> m [delegation]
  getDelegationsByCtx = undefined

  getDelegationsByDelegator :: [User m] -> m [delegation]
  getDelegationsByDelegator = undefined

  getDelegationsByDelegatee :: [User m] -> m [delegation]
  getDelegationsByDelegatee = undefined


data Outcome (m :: * -> *) v i user val =
  Outcome
    { _unOutcome :: [Vote m v i user val]
    }
  deriving (Eq, Ord, Show, Read, Generic)

-- | FIXME is 'tally' the right technical term?
tally :: Outcome m v i user val -> [(val, Int)]
tally = undefined

outcome :: (Process m v d c) => CtxVotable m -> m (Outcome m (CtxVotable m) (CtxInner m) (User m) (VoteValue m))
outcome = undefined


-- * this part is aula-specific

instance ( m ~ Action.Action
         , vote       ~ Vote       m (CtxVotable m) (CtxInner m) (User m) (VoteValue m)
         , delegation ~ Delegation m (CtxVotable m) (CtxInner m) (User m)
         , ctx        ~ Ctx        m (CtxVotable m) (CtxInner m)
         ) => Process Action.Action vote delegation ctx where

  type User Action.Action = Types.KeyOf Types.User
  type VoteValue Action.Action = Types.IdeaVoteValue
  type CtxVotable Action.Action = Types.AUID Types.Idea
  type CtxInner Action.Action = Types.DelegationContext
  type Key Action.Action ctx = Int
  type Key Action.Action (Types.AUID Types.Idea) = Int

  setCtx :: Key m ctx -> ctx -> m ()
  setCtx = undefined

  rmCtx :: Key m ctx -> m ()
  rmCtx = undefined

  getContext :: Key m ctx -> m ctx
  getContext = undefined


  supers :: ctx -> m [ctx]
  supers = undefined

  subs :: ctx -> m [ctx]
  subs = undefined


  setVote :: Key m (CtxVotable m) -> User m -> vote -> m ()
  setVote = undefined

  rmVote :: Key m (CtxVotable m) -> User m -> vote -> m ()
  rmVote = undefined

  getVotes :: m [vote]
  getVotes = undefined


  setDelegation :: Key m ctx -> User m -> User m -> m ()
  setDelegation = undefined

  rmDelegation :: Key m ctx -> User m -> User m -> m ()
  rmDelegation = undefined

  getDelegations :: m [delegation]
  getDelegations = undefined
