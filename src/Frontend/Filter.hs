{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Frontend.Filter
    ( Filter(Filtered,applyFilter,renderFilter)

    , IdeasFilterApi, IdeasFilterQuery(..), _AllIdeas, _IdeasWithCat, catFilter
    , IdeasSortApi, SortIdeasBy(..), labelSortIdeasBy
    , IdeasQuery(..), mkIdeasQuery, ideasQueryF, ideasQueryS, emptyIdeasQuery
    , toggleIdeasFilter
    )
where

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Thentos.Prelude
import Data.UriPath
import Servant.API -- (QueryParam, toUrlPiece)

import qualified Generics.SOP as SOP

import Types

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

-- FIXME make an HasLabel type class roleLabel, phaseName...
labelSortIdeasBy :: SortIdeasBy -> ST
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
        SortIdeasByTime     -> byTime
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
