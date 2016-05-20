{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Fragment.Note
where

import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

import Frontend.Prelude
import Frontend.Validation


data Note h = Note
    { noteHeaderText        :: h -> ST
    , noteValidationOnField :: ST
    , noteLabelText         :: ST
    }

noteForm
    :: (Monad m)
    => Note h
    -> View (HtmlT m ())
    -> (HtmlT m () -> HtmlT m ())
    -> h
    -> HtmlT m ()
noteForm note v form header = do
    div_ [class_ "container-note"] $ do
        h1_ [class_ "main-heading"] . toHtml $ noteHeaderText note header
        form $ do
            label_ $ do
                span_ [class_ "label-text"] . toHtml $ noteLabelText note
                inputTextArea_ [placeholder_ "..."] Nothing Nothing "note-text" v
                footer_ [class_ "form-footer"] $ do
                    DF.inputSubmit "Abschicken"

noteFormInput :: (Monad m) => Note t -> Maybe Document -> DF.Form (Html ()) m Document
noteFormInput note mdoc =
    "note-text" .: validate (cs $ noteValidationOnField note)
                            markdownV
                            (Markdown <$> DF.text (unMarkdown <$> mdoc))

noteFormOptionalInput :: (Monad m) => Note t -> Maybe Document -> DF.Form (Html ()) m (Maybe Document)
noteFormOptionalInput note mdoc =
    "note-text" .: validateOptional (cs $ noteValidationOnField note)
                                    markdownV
                                    (Markdown . cs <$$> DF.optionalString (cs . unMarkdown <$> mdoc))
