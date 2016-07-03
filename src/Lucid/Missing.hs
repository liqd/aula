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
    , toHtmlGeneralizeIdentity
    , module Lucid.I18N
    )
  where

import Control.Monad.Reader
import Data.String.Conversions
import Thentos.Prelude

import qualified Lucid as L
import qualified Lucid.Base as L
import qualified Text.Digestive.Lucid.Html5 as DF
import qualified Text.Digestive.View as DF

import Data.UriPath
import Frontend.Path
import Lucid.I18N


-- | See also https://github.com/chrisdone/lucid/issues/30
script_ :: (Monad m) => [L.Attribute] -> L.HtmlT m ()
script_ attrs = L.termRawWith "script" attrs mempty

inputSelect_ :: [L.Attribute] -> ST -> DF.View (L.HtmlT m ()) -> Monad m => L.HtmlT m ()
inputSelect_ attrs ref vw = L.select_
    ([ L.id_   ref'
     , L.name_ ref'
     ] <> attrs)
      . forM_ choices $ \(i, c, sel) -> L.option_
          ([L.value_ (value i)] <> DF.ifSingleton sel (L.selected_ "selected")) c
  where
    ref'    = DF.absoluteRef ref vw
    value i = ref' `mappend` "." `mappend` i
    choices = DF.fieldInputChoice ref vw

inputText_ :: [L.Attribute] -> ST -> DF.View v -> Monad m => L.HtmlT m ()
inputText_ attrs ref vw = L.input_ $
    [ L.type_    "text"
    , L.id_      ref'
    , L.name_    ref'
    , L.value_ $ DF.fieldInputText ref vw
    ] <> attrs
  where
    ref' = DF.absoluteRef ref vw

inputTextArea_ :: (Monad m) => [L.Attribute]
    -> Maybe Int -> Maybe Int -> ST -> DF.View (L.HtmlT m ()) -> L.HtmlT m ()
inputTextArea_ attrs r c ref view_ = L.textarea_ attrs' . L.toHtmlRaw $ DF.fieldInputText ref view_
  where
    attrs' = [ L.id_     ref'
             , L.name_   ref'
             ]
          <> rows' r <> cols' c
          <> attrs

    ref'           = DF.absoluteRef ref view_
    rows' (Just x) = [L.rows_ . cs $ show x]
    rows' _        = []
    cols' (Just x) = [L.cols_ . cs $ show x]
    cols' _        = []

inputPassword_ :: [L.Attribute] -> ST -> DF.View v -> Monad m => L.HtmlT m ()
inputPassword_ attrs ref vw = L.input_ $
    [ L.type_    "password"
    , L.id_      ref'
    , L.name_    ref'
    , L.value_ $ DF.fieldInputText ref vw
    ] <> attrs
  where
    ref' = DF.absoluteRef ref vw

inputSubmit_ :: [L.Attribute] -> ST -> Monad m => L.HtmlT m ()
inputSubmit_ attrs value = L.input_ $
    [ L.type_  "submit"
    , L.value_ value
    ] <> attrs

src_ :: HasPath p => p 'AllowGetPost -> L.Attribute
src_ = L.src_ . absoluteUriPath . relPath

href_ :: HasPath p => p 'AllowGetPost -> L.Attribute
href_ = L.href_ . absoluteUriPath . relPath

onclick_ :: HasPath p => p 'AllowGetPost -> L.Attribute
onclick_ p = L.onclick_ ("location.href='" <> absoluteUriPath (relPath p) <> "'")

formMethod_ :: (Monad m, HasPath p) => ST -> [L.Attribute] -> p a -> L.HtmlT m () -> L.HtmlT m ()
formMethod_ meth attrs path =
    L.form_ $ [ L.method_ meth
                  , L.action_ (absoluteUriPath (relPath path))
                  ] <> attrs

postLink_ :: HasPath p => [L.Attribute] -> p 'AllowPost -> ST -> Monad m => L.HtmlT m ()
postLink_ attrs path = formMethod_ "POST" [] path . inputSubmit_ attrs

postButton_ :: (Monad m, HasPath p)
  => [L.Attribute] -> p 'AllowPost -> L.HtmlT m () -> L.HtmlT m ()
postButton_ = postButton' []

postButton' :: (Monad m, HasPath p)
  => [L.Attribute] -> [L.Attribute] -> p 'AllowPost -> L.HtmlT m () -> L.HtmlT m ()
postButton' formAttrs buttonAttrs path =
    formMethod_ "POST" formAttrs path .
        L.button_ ([ L.type_ "submit" ] <> buttonAttrs)


-- | non-breaking space with a type.
nbsp :: ST
nbsp = "&nbsp;"


-- | (Before using this function, consider changing the type of its argument to be more general by
-- nature.)
toHtmlGeneralizeIdentity :: Monad m => HtmlT Identity a -> HtmlT m a
toHtmlGeneralizeIdentity (L.HtmlT (ReaderT r)) = L.HtmlT . ReaderT $ pure . runIdentity . r

instance L.ToHtml () where
    toHtmlRaw = L.toHtml
    toHtml () = ""
