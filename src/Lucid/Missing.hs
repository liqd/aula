{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Werror -Wall -fno-warn-orphans #-}


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
    , formMethod_
    , postLink_
    , postButton_
    , nbsp
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
import Frontend.Path


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
inputTextArea_ attrs r c ref view_ = textarea_ attrs' . toHtmlRaw $ DF.fieldInputText ref view_
  where
    attrs' = [ id_     ref'
             , name_   ref'
             ]
          <> rows' r <> cols' c
          <> attrs

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

src_ :: HasPath p => p 'AllowGetPost -> Lucid.Attribute
src_ = Lucid.src_ . absoluteUriPath . relPath

href_ :: HasPath p => p 'AllowGetPost -> Lucid.Attribute
href_ = Lucid.href_ . absoluteUriPath . relPath

onclick_ :: HasPath p => p 'AllowGetPost -> Lucid.Attribute
onclick_ p = Lucid.onclick_ ("location.href='" <> absoluteUriPath (relPath p) <> "'")

formMethod_ :: (Monad m, HasPath p) => ST -> [Lucid.Attribute] -> p a -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
formMethod_ meth attrs path =
    Lucid.form_ $ [ Lucid.method_ meth
                  , Lucid.action_ (absoluteUriPath (relPath path))
                  ] <> attrs

postLink_ :: HasPath p => [Lucid.Attribute] -> p 'AllowPost -> ST -> Monad m => Lucid.HtmlT m ()
postLink_ attrs path = formMethod_ "POST" [] path . inputSubmit_ attrs

postButton_ :: (Monad m, HasPath p) => [Lucid.Attribute] -> p 'AllowPost -> Lucid.HtmlT m () -> Lucid.HtmlT m ()
postButton_ attrs path = formMethod_ "POST" [] path . Lucid.button_ ([ type_ "submit" ] <> attrs)


-- | non-breaking space with a type.
nbsp :: ST
nbsp = "&nbsp;"


instance Lucid.ToHtml (Lucid.HtmlT Identity ()) where
    toHtmlRaw = toHtml
    toHtml = Lucid.HtmlT . return . runIdentity . Lucid.runHtmlT

instance Lucid.ToHtml () where
    toHtmlRaw = Lucid.toHtml
    toHtml () = ""
