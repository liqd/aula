{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DefaultSignatures           #-}
{-# LANGUAGE DeriveDataTypeable          #-}
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

-- | WARNING: the html scrubbing may be inaccurate and may have security issues!
module Data.Markdown
  ( Document, markdown, unMarkdown
  , PlainDocument(PlainDocument), unDescription
  )
where

import Data.CaseInsensitive
import Control.Lens
import Data.Binary
import Data.Data
import Data.List ((\\))
import Data.Maybe (catMaybes)
import Data.SafeCopy (deriveSafeCopy, base)
import Data.String.Conversions
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Data.CSS.Syntax.Tokens as CSS
import qualified Data.Markdown.HtmlWhiteLists as WhiteLists
import qualified Data.Text.Normalize as Norm
import qualified Generics.Generic.Aeson as Aeson
import qualified Text.HTML.TagSoup as HTML
import qualified Data.Char.Properties as UnicodeProps

newtype Document = Markdown { unMarkdown :: ST }
  deriving (Eq, Ord, Show, Read, Generic, Typeable, Data)

instance Binary Document

makePrisms ''Document

deriveSafeCopy 0 'base ''Document

instance Aeson.ToJSON Document where toJSON = Aeson.gtoJson
instance Aeson.FromJSON Document where parseJSON = Aeson.gparseJson


-- * validation and construction

markdown :: ST -> Either [ST] Document
markdown raw = case mconcat $ tokenToErrors <$> HTML.parseTags (sanitizeUnicode raw) of
    []  -> Right $ Markdown raw
    bad -> Left bad

-- | I tried some things here to fix #1033 until the crash went away.  I don't really know what I'm
-- doing here, hope I didn't break anything!  ~fisx
sanitizeUnicode :: ST -> ST
sanitizeUnicode = two . one
  where
    one = Norm.normalize Norm.NFD . Norm.normalize Norm.NFKD . Norm.normalize Norm.NFC . Norm.normalize Norm.NFKC
    two = cs . mapMaybe f . cs
      where
        f c
          | UnicodeProps.isWhiteSpace c = Just ' '
          | UnicodeProps.gcMajorClass (UnicodeProps.getGeneralCategory c) == UnicodeProps.ClOther = Nothing
          | otherwise = Just c

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = catMaybes . fmap f

tokenToErrors :: HTML.Tag ST -> [ST]
tokenToErrors = mconcat . \case
    (HTML.TagOpen el attrs) -> badEl el : (badAttr el <$> attrs)
    (HTML.TagClose el)      -> [badEl el]
    _                       -> []


badEl :: ST -> [ST]
badEl (mk -> el) =
    ["unsafe html element: " <> foldedCase el | el `notElem` els]
  where
    WhiteLists.HtmlElements els = WhiteLists.htmlElements

badAttr :: ST -> HTML.Attribute ST -> [ST]
badAttr (mk -> el) (mk -> akey, mk -> aval) =
    ["unsafe html attribute: " <> foldedCase akey
        | not $ any (`elem` attrs) [(Nothing, akey), (Just el, akey)]]
    <> badCssPropsIn akey aval
  where
    WhiteLists.HtmlAttributes attrs = WhiteLists.htmlAttributes

-- | This is a conservative approximation.  Some substrings of the style attribute value that are
-- never accepted as css properties may be accepted here nevertheless.
badCssPropsIn :: CI ST -> CI ST -> [ST]
badCssPropsIn akey aval
    | akey /= "style" = []
    | otherwise = case fmap extractProps . CSS.tokenize . foldedCase $ aval of
        Left err -> ["could not parse style attribute: " <> cs err]
        Right ps -> ("unsafe css property: " <>) . foldedCase <$> (mk <$> ps) \\ props
  where
    WhiteLists.Css3Properties props = WhiteLists.css3Properties
    extractProps = catMaybes . fmap f
      where
        f (CSS.Ident n) = Just n
        f _ = Nothing


-- | Be careful not to use `mappend` on user input!  The concatenation will be checked by
-- `markdown`, but the failure case will crash hard.
instance Monoid Document where
    mempty = Markdown mempty
    mappend (Markdown a) (Markdown b) = case markdown $ mappend a b of
        Right v -> v
        Left es -> error $ "instance Monoid Document: " <> show (es, a, b)


newtype PlainDocument = PlainDocument { unDescription :: ST }
  deriving (Eq, Ord, Show, Read, Generic, Monoid, Typeable, Data)

deriveSafeCopy 0 'base ''PlainDocument
