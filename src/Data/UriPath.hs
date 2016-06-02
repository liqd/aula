{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.UriPath
    ( UriPart
    , UriPath
    , (</>)
    , (</#>)
    , (</?>)
    , absoluteUriPath
    , relativeUriPath
    , HasUriPart(..))
    where

import Thentos.Prelude
import Data.String.Conversions

import qualified Data.Text as ST
import qualified Network.HTTP.Types as HTTP


newtype UriPart = SlashFreeUriPart { unUriPart :: ST }

instance IsString UriPart where
    fromString s
        | '/' `elem` s = error $ "UriPart.fromString: unexpected literal '/' in URI part: " <> s
        | otherwise    = SlashFreeUriPart . cs $ s

instance ConvertibleStrings UriPart ST where
    convertString = unUriPart

instance ConvertibleStrings UriPart String where
    convertString = cs . unUriPart

-- | An @UriPath@ is (1) a list of @UriPart@s stored as a difference list, and (2) an assoc list
-- containing the query.
--
-- When concatenating queries in 'Monoid', the query params of the parts are collected at the end of
-- the path.  Matrix parameters would offer a way to keep query params near the path segments they
-- aim at.  Mentioned in RFC3986 as "has been witnessed in the wild, but not often".
data UriPath = DiffUriParts
    { appendUriParts    :: [UriPart] -> [UriPart]
    , diffUriPartsQuery :: !HTTP.Query
    }

instance Monoid UriPath where
    mempty = DiffUriParts id []
    DiffUriParts ps q `mappend` DiffUriParts ps' q' = DiffUriParts (ps . ps') (q <> q')

infixl 7 </>
infixl 7 </#>
infixl 7 </?>

(</>) :: UriPath -> UriPart -> UriPath
DiffUriParts ps qs </> p = DiffUriParts (ps . (p :)) qs

(</#>) :: UriPath -> UriPart -> UriPath
ps </#> p = ps </> addHash p
  where
    addHash :: UriPart -> UriPart
    addHash (SlashFreeUriPart s) = SlashFreeUriPart ("#" <> s)

(</?>) :: UriPath -> HTTP.QueryItem -> UriPath
(DiffUriParts ps q) </?> q' = DiffUriParts ps (q' : q)

instance IsString UriPath where
    fromString s = DiffUriParts (ps <>) []
        where ps = SlashFreeUriPart <$> ST.splitOn "/" (cs s)

relativeUriPath :: UriPath -> ST
relativeUriPath u = p <> q
  where
    p = ST.intercalate "/" . map cs $ u `appendUriParts` []
    q = cs . HTTP.renderQuery True . diffUriPartsQuery $ u

absoluteUriPath :: UriPath -> ST
absoluteUriPath u = "/" <> relativeUriPath u

class HasUriPart p where
    uriPart :: p -> UriPart

instance HasUriPart ST where
    uriPart = fromString . cs

instance HasUriPart UriPart where
    uriPart = id
