{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DefaultSignatures           #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE KindSignatures              #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE Rank2Types                  #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Data.Markdown
  ( Document, markdown, unMarkdown )
where

import Control.Lens
import Data.Binary
import Data.SafeCopy (deriveSafeCopy, base)
import Data.String.Conversions
import GHC.Generics (Generic)
import Lucid (ToHtml, toHtml, toHtmlRaw, div_, class_)

import qualified Data.Aeson as Aeson
import qualified Generics.Generic.Aeson as Aeson
import qualified Text.HTML.Parser as HTML

import Data.Markdown.HtmlWhiteLists


newtype Document = Markdown { unMarkdown :: ST }
  deriving (Eq, Ord, Show, Read, Generic)

-- | Does no html escaping.  This is ok as long as we always use 'markdown' for constructing
-- 'Document' values.
instance ToHtml Document where
    toHtmlRaw = div_ [class_ "markdown"] . toHtmlRaw . unMarkdown
    toHtml    = toHtmlRaw

instance Binary Document

makePrisms ''Document

deriveSafeCopy 0 'base ''Document

instance Aeson.ToJSON Document where toJSON = Aeson.gtoJson
instance Aeson.FromJSON Document where parseJSON = Aeson.gparseJson


-- * validation and construction

markdown :: ST -> Either [ST] Document
markdown raw = case HTML.tagStream raw of
    []  -> Right $ Markdown raw
    bad -> Left $ (("illegal html tag: " <>) . cs . show) <$> bad  -- TODO: be more lenient on benevolent html.

-- | Be careful not to use `mappend` on user input!  The concatenation will be checked by
-- `markdown`, but the failure case will crash hard.
instance Monoid Document where
    mempty = Markdown mempty
    mappend (Markdown a) (Markdown b) = case markdown $ mappend a b of
        Right v -> v
        Left es -> error $ "instance Monoid Document: " <> show (es, a, b)
