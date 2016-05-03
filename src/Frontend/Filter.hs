{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Frontend.Filter
    ( Filter(Filtered, applyFilter, renderFilter)

    , IdeasFilterApi, IdeasFilterQuery(..), _AllIdeas, _IdeasWithCat, catFilter
    , IdeasSortApi, SortIdeasBy(..), labelSortIdeasBy
    , IdeasQuery(..), mkIdeasQuery, ideasQueryF, ideasQueryS, emptyIdeasQuery
    , toggleIdeasFilter

    , UsersFilterApi, SearchUsers(..), UsersFilterQuery(..), _AllUsers, _UsersWithText, searchUsers
    , UsersSortApi, SortUsersBy(..), labelSortUsersBy
    , UsersQuery(..), mkUsersQuery, usersQueryF, usersQueryS, emptyUsersQuery
    )
where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Servant.API (QueryParam, FromHttpApiData, ToHttpApiData, parseUrlPiece, toUrlPiece)
import Thentos.Prelude

import qualified Data.Ord
import qualified Data.Text as ST
import qualified Generics.SOP as SOP

import Data.UriPath
import Types


-- * filter (also does sort)

class Filter a where
    type Filtered a
    applyFilter  :: a -> [Filtered a] -> [Filtered a]
    renderFilter :: a -> UriPath -> UriPath

type family FilterName a :: Symbol

type FilterApi a = QueryParam (FilterName a) a

renderQueryParam :: forall a. (KnownSymbol (FilterName a), ToHttpApiData a) => a -> UriPath -> UriPath
renderQueryParam a p = p </?> (cs (symbolVal (Proxy :: Proxy (FilterName a))), Just . cs $ toUrlPiece a)

instance Filter a => Filter (Maybe a) where
    type Filtered (Maybe a) = Filtered a
    applyFilter  = maybe id applyFilter
    renderFilter = maybe id renderFilter


-- * filter and sort ideas

data IdeasFilterQuery = AllIdeas | IdeasWithCat { _catFilter :: Category }
  deriving (Eq, Ord, Show, Read, Generic)
type IdeasFilterApi = FilterApi Category

instance SOP.Generic IdeasFilterQuery

makeLenses ''IdeasFilterQuery
makePrisms ''IdeasFilterQuery

toggleIdeasFilter :: Category -> IdeasFilterQuery -> IdeasFilterQuery
toggleIdeasFilter cat q
    | q == IdeasWithCat cat = AllIdeas
    | otherwise             = IdeasWithCat cat

instance Filter   Category where
    type Filtered Category = Idea
    applyFilter c = filter $ (== Just c) . view ideaCategory
    renderFilter  = renderQueryParam

type instance FilterName Category = "category"

instance Filter     IdeasFilterQuery where
    type Filtered   IdeasFilterQuery = Idea
    applyFilter  f = applyFilter  $ f ^? catFilter
    renderFilter f = renderFilter $ f ^? catFilter

data SortIdeasBy = SortIdeasByTime | SortIdeasBySupport
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

-- FIXME make an HasLabel type class for roleLabel, phaseName...
labelSortIdeasBy :: IsString s => SortIdeasBy -> s
labelSortIdeasBy = \case
    SortIdeasBySupport -> "Unterstützung"
    SortIdeasByTime    -> "Datum"

type IdeasSortApi = FilterApi SortIdeasBy

instance SOP.Generic SortIdeasBy

instance FromHttpApiData SortIdeasBy where
    parseUrlPiece = \case
        "time"    -> Right SortIdeasByTime
        "support" -> Right SortIdeasBySupport
        _         -> Left "no parse"

instance ToHttpApiData SortIdeasBy where
    toUrlPiece = \case
        SortIdeasByTime    -> "time"
        SortIdeasBySupport -> "support"

instance Filter   SortIdeasBy where
    type Filtered SortIdeasBy = Idea

    applyFilter = \case
        SortIdeasByTime    -> byTime
        SortIdeasBySupport -> bySupport . byTime
      where
        byTime = downSortOn createdAt
        bySupport = downSortOn $ ideaLikes . to length

    renderFilter = renderQueryParam

type instance FilterName SortIdeasBy = "sortby"

data IdeasQuery = IdeasQuery
    { _ideasQueryF :: IdeasFilterQuery
    , _ideasQueryS :: SortIdeasBy
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic IdeasQuery

makeLenses ''IdeasQuery

mkIdeasQuery :: Maybe Category -> Maybe SortIdeasBy -> IdeasQuery
mkIdeasQuery mc ms = IdeasQuery (maybe AllIdeas IdeasWithCat mc) (fromMaybe minBound ms)

emptyIdeasQuery :: IdeasQuery
emptyIdeasQuery = IdeasQuery AllIdeas minBound

instance Filter IdeasQuery where
    type Filtered IdeasQuery = Idea

    applyFilter  (IdeasQuery f s) = applyFilter  s . applyFilter  f
    renderFilter (IdeasQuery f s) = renderFilter s . renderFilter f


-- * users sorting

data SortUsersBy = SortUsersByTime | SortUsersByName | SortUsersByClass | SortUsersByRole
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

type UsersSortApi = FilterApi SortUsersBy

instance SOP.Generic SortUsersBy

instance FromHttpApiData SortUsersBy where
    parseUrlPiece = \case
        "time"  -> Right SortUsersByTime
        "name"  -> Right SortUsersByName
        "class" -> Right SortUsersByClass
        "role"  -> Right SortUsersByRole
        _       -> Left "no parse"

instance ToHttpApiData SortUsersBy where
    toUrlPiece = \case
        SortUsersByTime  -> "time"
        SortUsersByName  -> "name"
        SortUsersByClass -> "class"
        SortUsersByRole  -> "role"

instance Filter   SortUsersBy where
    type Filtered SortUsersBy = UserView

    applyFilter = \case
        SortUsersByTime  -> byTime
        SortUsersByName  -> byName  . byTime
        SortUsersByClass -> byClass . byTime
        SortUsersByRole  -> byRole  . byTime
      where
        by :: Ord a => Getter User a -> [UserView] -> [UserView]
        by g    = sortOn (pre $ activeUser . g)
        byTime  = by $ createdAt . to Data.Ord.Down
        byName  = by userLogin
        byClass = by userSchoolClass
        byRole  = by $ userRole . to (roleLabel :: Role -> ST)

    renderFilter = renderQueryParam

type instance FilterName SortUsersBy = "sortby"

-- FIXME see labelSortIdeasBy
labelSortUsersBy :: IsString s => SortUsersBy -> s
labelSortUsersBy = \case
    SortUsersByTime  -> "Datum"
    SortUsersByName  -> "Name"
    SortUsersByClass -> "Klasse"
    SortUsersByRole  -> "Rolle"

newtype SearchUsers = SearchUsers ST
  deriving (Eq, Ord, Show, Read, Generic, FromHttpApiData, ToHttpApiData)

instance SOP.Generic SearchUsers

instance Filter SearchUsers where
    type Filtered SearchUsers = UserView

    applyFilter (SearchUsers t) = filter $ anyOf (activeUser . searchee) (t `ST.isInfixOf`)
      where
        searchee :: (Monoid (f User), Functor f, Applicative f, Contravariant f)
                 => (ST -> f ST) -> User -> f User
        searchee = userLogin . _UserLogin <>
                   like " " <>
                   userSchoolClass . _Just . to showSchoolClass . csi

    renderFilter = renderQueryParam

type instance FilterName SearchUsers = "search"

data UsersFilterQuery = AllUsers | UsersWithText { _searchUsers :: SearchUsers }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic UsersFilterQuery

type UsersFilterApi = FilterApi SearchUsers

makeLenses ''UsersFilterQuery
makePrisms ''UsersFilterQuery

instance Filter UsersFilterQuery where
    type Filtered UsersFilterQuery = UserView

    applyFilter  f = applyFilter  $ f ^? searchUsers
    renderFilter f = renderFilter $ f ^? searchUsers

data UsersQuery = UsersQuery
    { _usersQueryF :: UsersFilterQuery
    , _usersQueryS :: SortUsersBy
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic UsersQuery

makeLenses ''UsersQuery

instance Filter UsersQuery where
    type Filtered UsersQuery = UserView

    applyFilter  (UsersQuery f s) = applyFilter  s . applyFilter  f
    renderFilter (UsersQuery f s) = renderFilter s . renderFilter f

mkUsersQuery :: Maybe SearchUsers -> Maybe SortUsersBy -> UsersQuery
mkUsersQuery mf ms = UsersQuery (maybe AllUsers UsersWithText mf) (fromMaybe minBound ms)

emptyUsersQuery :: UsersQuery
emptyUsersQuery = UsersQuery AllUsers minBound
