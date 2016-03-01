{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.UriPath
    ( UriPart
    , UriPath
    , (</>)
    , absoluteUriPath
    , relativeUriPath
    , script_
    , src_
    , href_
    , onclick_
    , HasPath(..)
    , HasUriPart(..))
    where

import Thentos.Prelude
import Data.String.Conversions

import qualified Data.Text as ST
import qualified Lucid
import qualified Lucid.Base as Lucid

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

-- | FIXME: slightly out of place here.  should probably go to Ludic.Missing.  see also
-- https://github.com/chrisdone/lucid/issues/30
script_ :: (Monad m) => [Lucid.Attribute] -> Lucid.HtmlT m ()
script_ attrs = Lucid.termRawWith "script" attrs mempty

src_ :: HasPath p => p -> Lucid.Attribute
src_ = Lucid.src_ . absoluteUriPath . relPath

href_ :: HasPath p => p -> Lucid.Attribute
href_ = Lucid.href_ . absoluteUriPath . relPath

onclick_ :: HasPath p => p -> Lucid.Attribute
onclick_ p = Lucid.onclick_ ("location.href='" <> absoluteUriPath (relPath p) <> "'")

class HasPath p where
    relPath :: p -> UriPath

class HasUriPart p where
    uriPart :: p -> UriPart

instance HasUriPart ST where
    uriPart = fromString . cs

instance HasUriPart UriPart where
    uriPart = id
