{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.UriPath
    ( UriPart
    , UriPath
    , (</>)
    , absoluteUriPath
    , relativeUriPath
    , HasPath(..)
    , HasUriPart(..))
    where

import Thentos.Prelude
import Data.String.Conversions

import qualified Data.Text as ST

newtype UriPart = SlashFreeUriPart { fromUriPart :: ST }

instance IsString UriPart where
    fromString s
        | '/' `elem` s = error $ "UriPart.fromString: unexpected literal '/' in URI part: " <> s
        | otherwise    = SlashFreeUriPart . cs $ s

instance s ~ ST => ConvertibleStrings UriPart s where
    convertString = fromUriPart

-- | An @UriPath@ is a list of @UriPart@s stored as a difference list.
newtype UriPath = DiffUriParts { appendUriParts :: [UriPart] -> [UriPart] }

instance Monoid UriPath where
    mempty = DiffUriParts id

    DiffUriParts ps `mappend` DiffUriParts qs = DiffUriParts (ps . qs)

infixl 7 </>

(</>) :: UriPath -> UriPart -> UriPath
DiffUriParts ps </> p = DiffUriParts (ps . (p :))

instance IsString UriPath where
    fromString s = DiffUriParts (ps ++)
        where ps = SlashFreeUriPart <$> ST.splitOn "/" (cs s)

relativeUriPath :: UriPath -> ST
relativeUriPath u = ST.intercalate "/" . map cs $ u `appendUriParts` []

absoluteUriPath :: UriPath -> ST
absoluteUriPath u = "/" <> relativeUriPath u

class HasPath p where
    relPath :: p -> UriPath

class HasUriPart p where
    uriPart :: p -> UriPart

instance HasUriPart ST where
    uriPart = fromString . cs

instance HasUriPart UriPart where
    uriPart = id
