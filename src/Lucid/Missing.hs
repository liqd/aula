{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}

-- | (FIXME: push most of this upstream to lucid or digestive-functors-lucid?  it'll be tricky in
-- some places as we are using Data.UriPath from this package.)
module Lucid.Missing
    ( script_
    , inputText_
    , inputTextArea_
    , inputSelect_
    , inputPassword_
    , inputSubmit_
    , src_
    , href_
    , onclick_
    )
  where

import Data.String.Conversions
import Lucid hiding (for_, script_, src_, href_, onclick_)
import Thentos.Prelude

import qualified Lucid
import qualified Lucid.Base as Lucid
import qualified Text.Digestive.View as DF
import qualified Text.Digestive.Lucid.Html5 as DF

import Data.UriPath


-- | See also https://github.com/chrisdone/lucid/issues/30
script_ :: (Monad m) => [Lucid.Attribute] -> Lucid.HtmlT m ()
script_ attrs = Lucid.termRawWith "script" attrs mempty

inputSelect_ :: [Lucid.Attribute] -> ST -> DF.View (Lucid.HtmlT m ()) -> Monad m => Lucid.HtmlT m ()
inputSelect_ attrs ref vw = select_
    ([ id_   ref'
     , name_ ref'
     ] <> attrs)
      . forM_ choices $ \(i, c, sel) -> option_
          ([value_ (value i)] <> DF.ifSingleton sel (selected_ "selected")) c
  where
    ref'    = DF.absoluteRef ref vw
    value i = ref' `mappend` "." `mappend` i
    choices = DF.fieldInputChoice ref vw

inputText_ :: [Lucid.Attribute] -> ST -> DF.View v -> Monad m => Lucid.HtmlT m ()
inputText_ attrs ref vw = Lucid.input_ $
    [ Lucid.type_    "text"
    , Lucid.id_      ref'
    , Lucid.name_    ref'
    , Lucid.value_ $ DF.fieldInputText ref vw
    ] <> attrs
  where
    ref' = DF.absoluteRef ref vw

inputTextArea_ :: (Monad m) => [Lucid.Attribute]
    -> Maybe Int -> Maybe Int -> ST -> DF.View (HtmlT m ()) -> HtmlT m ()
inputTextArea_ attrs r c ref view_ = textarea_
    ([ id_     ref'
     , name_   ref'
     ] <> rows' r <> cols' c <> attrs) $
        toHtmlRaw $ DF.fieldInputText ref view_
  where
    ref'           = DF.absoluteRef ref view_
    rows' (Just x) = [rows_ . cs $ show x]
    rows' _        = []
    cols' (Just x) = [cols_ . cs $ show x]
    cols' _        = []

inputPassword_ :: [Lucid.Attribute] -> ST -> DF.View v -> Monad m => Lucid.HtmlT m ()
inputPassword_ attrs ref vw = input_ $
    [ type_    "password"
    , id_      ref'
    , name_    ref'
    , value_ $ DF.fieldInputText ref vw
    ] <> attrs
  where
    ref' = DF.absoluteRef ref vw

inputSubmit_ :: [Lucid.Attribute] -> ST -> Monad m => Lucid.HtmlT m ()
inputSubmit_ attrs value = input_ $
    [ type_  "submit"
    , value_ value
    ] <> attrs

src_ :: HasPath p => p -> Lucid.Attribute
src_ = Lucid.src_ . absoluteUriPath . relPath

href_ :: HasPath p => p -> Lucid.Attribute
href_ = Lucid.href_ . absoluteUriPath . relPath

onclick_ :: HasPath p => p -> Lucid.Attribute
onclick_ p = Lucid.onclick_ ("location.href='" <> absoluteUriPath (relPath p) <> "'")
