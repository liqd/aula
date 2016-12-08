{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_GHC -Wall -Werror           #-}

module Data.Delegation
    ( Delegatee(..)
    , Delegate(..)
    , Delegations

    , DelegationMap
    , CoDelegationMap

    , emptyDelegations
    , removeDelegationsByScope
    , filterDelegationsByScope
    , setDelegation
    , deleteDelegation
    , delegates
    , delegatesSafe
    , delegateInScope
    , scopeDelegateSafe
    , delegatees
    , delegateesSafe
    , delegateesInScope
    , scopeDelegateesSafe
    , votingPower
    , findDelegationsByScope
    , renameDelegations
    , fromList
    , toList
    )
where

import Control.Exception (assert)
import Control.Lens hiding (from, to)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Generics.SOP as SOP

import Data.Functor.Infix ((<$$>))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable (Typeable)
import Data.Data (Data)
import GHC.Generics (Generic)

import Control.Monad.Reader

import Data.DoubleMap as DMap
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

-- | Delegatees give their vote to me, so i vote for them:
-- "für wen stimme ich ab?" ("for whom do i vote?")
newtype Delegatee v = Delegatee { unDelegatee :: v }
  deriving (Eq, Ord, Show, Read, Generic, Typeable, Data)

-- | Delegates get their vote from me, so they vote for me:
-- "wer stimmt für mich ab?" ("who votes for me?")
newtype Delegate  v = Delegate { unDelegate :: v }
  deriving (Eq, Ord, Show, Read, Generic, Typeable, Data)

newtype DelegationMap = DelegationMap {
        _delegationMap
            :: Map
                (Delegatee U)
                (Map S (Delegate U))
    }
  deriving (Eq, Show, Read, Generic, Typeable, Data)

newtype CoDelegationMap = CoDelegationMap {
        _coDelegationMap
            :: Map
                (Delegate U)
                (Map S (Set (Delegatee U)))
    }
  deriving (Eq, Show, Read, Generic, Typeable, Data)

data Delegations = Delegations {
      _delegations   :: DelegationMap
    , _coDelegations :: CoDelegationMap
    }
  deriving (Eq, Show, Read, Generic, Typeable, Data)

makeLenses ''DelegationMap
makeLenses ''CoDelegationMap
makeLenses ''Delegations


-- * delegation

emptyDelegations :: Delegations
emptyDelegations = Delegations (DelegationMap Map.empty) (CoDelegationMap Map.empty)

removeDelegationsByScope :: DScope -> Delegations -> Delegations
removeDelegationsByScope dscope (Delegations (DelegationMap dm) (CoDelegationMap codm)) =
    Delegations
        (DelegationMap   (Map.map (Map.delete dscope) dm))
        (CoDelegationMap (Map.map (Map.delete dscope) codm))

filterDelegationsByScope :: (DScope -> Bool) -> Delegations -> Delegations
filterDelegationsByScope predicate (Delegations (DelegationMap dm) (CoDelegationMap codm)) =
    Delegations
        (DelegationMap   (Map.map (Map.filterWithKey predicate') dm))
        (CoDelegationMap (Map.map (Map.filterWithKey predicate') codm))
  where
    predicate' k _ = predicate k


-- * delegation handling

setDelegation :: U -> S -> U -> Delegations -> Delegations
setDelegation f dscope t (Delegations (DelegationMap dmap) (CoDelegationMap coDmap))
    = Delegations dmap' coDmap'
  where
    from = Delegatee f
    to   = Delegate  t
    mOldTo = DMap.lookup from dscope dmap

    dmap'   = DelegationMap (DMap.insert from dscope to dmap)
    coDmap' = CoDelegationMap coDmap1
    coDmap0 = maybe coDmap (\to' -> coDmap & at to' . _Just . at dscope . _Just . at from .~ Nothing
                                           & at to' . _Just . at dscope %~ deleteEmpty
                                           & at to' %~ deleteEmpty) mOldTo
    coDmap1 = DMap.insert to dscope (Set.insert from $ fromMaybe Set.empty (DMap.lookup to dscope coDmap0)) coDmap0

deleteDelegation :: U -> S -> U -> Delegations -> Delegations
deleteDelegation delegatee dscope delegate ds@(Delegations (DelegationMap dmap) (CoDelegationMap coDmap))
    | DMap.lookup (Delegatee delegatee) dscope dmap /= Just (Delegate delegate) = ds
    | otherwise = Delegations dmap' coDmap'
  where
    from = Delegatee delegatee
    mOldTo = DMap.lookup from dscope dmap

    dmap'   = DelegationMap (DMap.remove from dscope dmap)
    coDmap' = CoDelegationMap coDmap1
    coDmap1 = maybe coDmap (\to' -> coDmap & at to' . _Just . at dscope . _Just . at from .~ Nothing
                                           & at to' . _Just . at dscope %~ deleteEmpty
                                           & at to' %~ deleteEmpty) mOldTo

delegates :: U -> Delegations -> [(DScope, U)]
delegates delegatee ds = over _2 unDelegate <$> delegatesSafe (Delegatee delegatee) ds

-- | Returns all the direct delegates for a given delegatee.
delegatesSafe :: Delegatee U -> Delegations -> [(DScope, Delegate U)]
delegatesSafe delegatee (Delegations (DelegationMap dmap) _coDmap)
    = maybe [] Map.toList $ Map.lookup delegatee dmap

delegateInScope :: U -> DScope -> Delegations -> Maybe U
delegateInScope delegatee scope ds =
    unDelegate <$> scopeDelegateSafe (Delegatee delegatee) scope ds

-- | Returns the direct delegate for a given delegatee in a scope.
scopeDelegateSafe :: Delegatee U -> DScope -> Delegations -> Maybe (Delegate U)
scopeDelegateSafe delegatee scope (Delegations (DelegationMap dmap) _coDmap)
    = DMap.lookup delegatee scope dmap

delegatees :: U -> Delegations -> [(DScope, Set U)]
delegatees delegate ds = over _2 (Set.map unDelegatee) <$> delegateesSafe (Delegate delegate) ds

-- | Returns all the direct delegatess for a given delegate
delegateesSafe :: Delegate U -> Delegations -> [(DScope, Set (Delegatee U))]
delegateesSafe delegate (Delegations _dmap (CoDelegationMap cmap))
    = maybe [] Map.toList $ Map.lookup delegate cmap

delegateesInScope :: U -> S -> Delegations -> Set U
delegateesInScope delegate scope =
    Set.map unDelegatee . scopeDelegateesSafe (Delegate delegate) scope

scopeDelegateesSafe :: Delegate U -> S -> Delegations -> Set (Delegatee U)
scopeDelegateesSafe delegate scope (Delegations _dmap (CoDelegationMap coDMap)) =
    fromMaybe Set.empty $ DMap.lookup delegate scope coDMap

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
    [ (delegate, scope, fromMaybe [] $ Set.toList <$> DMap.lookup delegate scope cdm)
    | delegate   <- Map.keys cdm
    ]

-- The renaming MUST be injective.
renameMapKeys :: Ord k' => (k -> k') -> Map k v -> Map k' v
renameMapKeys f = Map.fromList . map (_1 %~ f) . Map.toList

-- The renaming MUST be injective.
renameDelegations :: (S -> S) -> Delegations -> Delegations
renameDelegations f dls =
    dls & delegations . delegationMap . each %~ renameMapKeys f
        & coDelegations . coDelegationMap . each %~ renameMapKeys f

fromList :: [(U, S, U)] -> Delegations
fromList = foldr (\(f, d, t) -> setDelegation f d t) emptyDelegations

toList :: Delegations -> [(U, S, U)]
toList (Delegations (DelegationMap a) (CoDelegationMap b)) = assert (sort a' == sort b') a'
  where
    a' = mconcat $ (\(Delegatee f, submap) ->
            (\(d, Delegate t) -> (f, d, t)) <$> Map.toList submap)
                <$> Map.toList a
    b' = mconcat . mconcat $ (\(Delegate f, submap) ->
            (\(d, delegateeset) ->
                (\(Delegatee t) -> (t, d, f)) <$> Set.toList delegateeset)
                    <$> Map.toList submap)
                        <$> Map.toList b


-- * safe copy

deriveSafeCopy 0 'base ''Delegate
deriveSafeCopy 0 'base ''Delegatee
deriveSafeCopy 0 'base ''CoDelegationMap
deriveSafeCopy 0 'base ''DelegationMap
deriveSafeCopy 0 'base ''Delegations

-- * SOP

instance SOP.Generic (Delegate v)
instance SOP.Generic (Delegatee v)
instance SOP.Generic CoDelegationMap
instance SOP.Generic DelegationMap
instance SOP.Generic Delegations
