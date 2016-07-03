{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Werror -Wall -fno-warn-orphans #-}


module Lucid.I18N
    ( Html
    , HtmlT
    , ToHtml(..)
    , Lang(..)
    , whereToGetTheLangValue
    )
  where

import Control.Monad.Reader
import Data.String.Conversions
import qualified Lucid as L
import Thentos.Prelude

import Data.Markdown


-- * types

type Html = HtmlT Identity
type HtmlT m = L.HtmlT (ReaderT Lang m)

class ToHtml a where
    toHtmlRaw :: Monad m => a -> HtmlT m ()
    toHtml    :: Monad m => a -> HtmlT m ()

-- | TODO: 'Lang' is not lucid-specific.  make types here polymorphic in it, move to Types.*, and
-- also find a better name.  (it should be a project-specific type because it decides which
-- languages are supported.)
data Lang = DE
  deriving (Eq, Ord, Show, Read, Bounded, Enum)


-- | TODO: in some places, we don't have access to the session data yet, and cheat.  remove this and
-- follow the type errors.
whereToGetTheLangValue :: Lang
whereToGetTheLangValue = DE


-- * some handy instances

instance ToHtml () where
    toHtmlRaw = toHtml
    toHtml () = ""

instance ToHtml ST where
    toHtmlRaw = toHtml
    toHtml = toHtml

instance ToHtml String where
    toHtmlRaw = toHtml
    toHtml = toHtml

-- | Does no html escaping.  This is ok as long as we always use 'markdown' for constructing
-- 'Document' values.
instance ToHtml Document where
    toHtmlRaw = L.div_ [L.class_ "markdown"] . toHtmlRaw . unMarkdown
    toHtml    = toHtmlRaw

instance ToHtml PlainDocument where
    toHtmlRaw = L.div_ . toHtmlRaw . unDescription
    toHtml    = L.div_ . toHtml    . unDescription
