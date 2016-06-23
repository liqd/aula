{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -Wall -Werror    #-}

module Data.Delegation
    ( Delegatee(..)
    , Delegate(..)
    , Delegations
    , emptyDelegations
    , setDelegation
    , deleteDelegation
    , scopeDelegatees
    , scopeDelegateesSafe
    , votingPower
    , findDelegationsByScope
    )
where

import Control.Lens hiding (from, to)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Functor.Infix ((<$$>))
import Data.SafeCopy (base, deriveSafeCopy)

import Control.Monad.Reader

import Types


--  Type parameters for this module are 'U' and 'S'.  Fixing them
-- is necessary as a workaround for an issue of 'dericeSafeCopy'.
--
--  The deriveSafeCopy can not add the Ord constraints when
-- creating the SafeCopy class for the 'DelegationMap S U'.  The
-- constraints are necessary to copy the inner map.
--
--  The type constants U and S are created to be able to make this
-- module parametric easier when the issue is resolved.


-- * type constants

type U = AUID User
type S = DScope


-- * types

newtype Delegatee v = Delegatee { unDelegatee :: v }
  deriving (Eq, Ord, Show, Read)

newtype Delegate  v = Delegate { unDelegate :: v }
  deriving (Eq, Ord, Show, Read)

data DelegationMap = DelegationMap {
        _delegationMap
            :: Map
                (Delegatee U)
                (Map S (Delegate U))
    }
  deriving (Eq, Show, Read)

data CoDelegationMap = CoDelegationMap {
        _coDelegationMap
            :: Map
                (Delegate U)
                (Map S (Set (Delegatee U)))
    }
  deriving (Eq, Show, Read)

data Delegations = Delegations {
      _delegations   :: DelegationMap
    , _coDelegations :: CoDelegationMap
    }
  deriving (Eq, Show, Read)


-- * helpers

lookupDoubleMap :: (Ord k1, Ord k2) => k1 -> k2 -> Map k1 (Map k2 v) -> Maybe v
lookupDoubleMap k1 k2 m = Map.lookup k1 m >>= Map.lookup k2

insertDoubleMap :: (Ord k1, Ord k2) => k1 -> k2 -> v -> Map k1 (Map k2 v) -> Map k1 (Map k2 v)
insertDoubleMap k1 k2 v m = case Map.lookup k1 m of
    Nothing -> Map.insert k1 (Map.singleton k2 v) m
    Just m' -> Map.insert k1 (Map.insert k2 v m') m

deleteDoubleMap :: (Ord k1, Ord k2) => k1 -> k2 -> Map k1 (Map k2 v) -> Map k1 (Map k2 v)
deleteDoubleMap k1 k2 m = m
                        & at k1 . _Just . at k2 .~ Nothing
                        & at k1 %~ deleteEmpty


-- * delegation

emptyDelegations :: Delegations
emptyDelegations = Delegations (DelegationMap Map.empty) (CoDelegationMap Map.empty)


-- * delegation handling

setDelegation :: U -> S -> U -> Delegations -> Delegations
setDelegation f dscope t (Delegations (DelegationMap dmap) (CoDelegationMap coDmap))
    = Delegations dmap' coDmap'
  where
    from = Delegatee f
    to   = Delegate  t
    mOldTo = lookupDoubleMap from dscope dmap

    dmap'   = DelegationMap (insertDoubleMap from dscope to dmap)
    coDmap' = CoDelegationMap coDmap1
    coDmap0 = maybe coDmap (\to' -> coDmap & at to' . _Just . at dscope . _Just . at from .~ Nothing
                                           & at to' . _Just . at dscope %~ deleteEmpty
                                           & at to' %~ deleteEmpty) mOldTo
    coDmap1 = insertDoubleMap to dscope (Set.insert from $ fromMaybe Set.empty (lookupDoubleMap to dscope coDmap0)) coDmap0

deleteEmpty :: Foldable t => Maybe (t a) -> Maybe (t a)
deleteEmpty (Just s) | null s = Nothing
deleteEmpty x                 = x

-- | FIXME: if we mention the delegate here, not just the delegatee, we can confirm that is is the
-- one we expect, and we will catch errors more local to their source.
deleteDelegation :: U -> S -> Delegations -> Delegations
deleteDelegation f dscope (Delegations (DelegationMap dmap) (CoDelegationMap coDmap))
    = Delegations dmap' coDmap'
  where
    from = Delegatee f
    mOldTo = lookupDoubleMap from dscope dmap

    dmap'   = DelegationMap (deleteDoubleMap from dscope dmap)
    coDmap' = CoDelegationMap coDmap1
    coDmap1 = maybe coDmap (\to' -> coDmap & at to' . _Just . at dscope . _Just . at from .~ Nothing
                                           & at to' . _Just . at dscope %~ deleteEmpty
                                           & at to' %~ deleteEmpty) mOldTo

scopeDelegatees :: U -> S -> Delegations -> Set U
scopeDelegatees delegate scope =
    Set.map unDelegatee . scopeDelegateesSafe (Delegate delegate) scope

scopeDelegateesSafe :: Delegate U -> S -> Delegations -> Set (Delegatee U)
scopeDelegateesSafe delegate scope (Delegations _dmap (CoDelegationMap coDMap)) =
    fromMaybe Set.empty $ lookupDoubleMap delegate scope coDMap

-- | The number of votes a 'User' has in a 'DScope', including own vote, direct delegations,
-- transitive delegations, and implicit delegations from ancestor 'DScope's.
votingPower :: U -> [S] -> Delegations -> [U]
votingPower vid path ds = unDelegatee <$$> Set.toList . flip runReader ds $ do
    voters (Set.singleton (Delegatee vid)) (Delegate vid)
  where
    delegateeToDelegate (Delegatee d) = Delegate d

    voters :: Set (Delegatee U) -> Delegate U -> Reader Delegations (Set (Delegatee U))
    voters discovered user = do
        oneStepNewDelegatees
            <- (`Set.difference` discovered) . Set.unions
                <$> forM path (asks . scopeDelegateesSafe user)
        let oneStepNewDelegateesAndDiscovereds = oneStepNewDelegatees `Set.union` discovered
        allNewDelegatees <- Set.unions
            <$> forM (Set.toList oneStepNewDelegatees)
                     (voters oneStepNewDelegateesAndDiscovereds . delegateeToDelegate)
        pure (discovered `Set.union` allNewDelegatees)

-- FIXME: Speed-up using a third Map, which indexes the scope
findDelegationsByScope :: S -> Delegations -> [(Delegate U, S, [Delegatee U])]
findDelegationsByScope scope (Delegations _dmap (CoDelegationMap cdm)) =
    [ (delegate, scope, fromMaybe [] $ Set.toList <$> lookupDoubleMap delegate scope cdm)
    | delegate   <- Map.keys cdm
    ]

-- * safe copy

deriveSafeCopy 0 'base ''Delegate
deriveSafeCopy 0 'base ''Delegatee
deriveSafeCopy 0 'base ''CoDelegationMap
deriveSafeCopy 0 'base ''DelegationMap
deriveSafeCopy 0 'base ''Delegations
