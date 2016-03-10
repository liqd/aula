{-# LANGUAGE TypeFamilies #-}
module Frontend.Links
where

import Lucid.Missing
import Frontend.Page
import Frontend.PageMap

import Frontend.Core (Page)

import Data.Proxy

href :: (Reachable p q ~ (), Page p, Page q) => (p, q) -> UriPath
href _p q = ()

hrefOk = href (PageRoomsOverview []) (PageRoomsOverview)

