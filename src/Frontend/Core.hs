{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Core
where

import Data.String.Conversions
import Lucid
import Text.Digestive.View

----------------------------------------------------------------------
-- building blocks

-- | Render Form based Views 
class FormPageView p where
    -- | Generates a Html view from the given view, form action, and the @p@ page
    formPageView :: (Monad m) => View (HtmlT m ()) -> ST -> p -> HtmlT m ()

-- | The page after submitting a form should be redirected 
class RedirectOf p where
    -- | Calculates a redirect address from the given page
    redirectOf :: p -> ST

-- | Wrap anything that has 'ToHtml' and wrap it in an HTML body with complete page.
newtype Frame body = Frame body

instance (ToHtml body) => ToHtml (Frame body) where
    toHtmlRaw          = toHtml
    toHtml (Frame bdy) = pageFrame (toHtml bdy)

pageFrame :: (Monad m) => HtmlT m a -> HtmlT m ()
pageFrame bdy = do
    head_ $ do
        title_ "AuLA"
        link_ [rel_ "stylesheet", href_ "/screen.css"]
    body_ $ do
        headerMarkup >> bdy >> footerMarkup

headerMarkup :: forall m. (Monad m) => HtmlT m ()
headerMarkup = div_ $ do
    span_ "aula"
    -- TODO: these should be links
    span_ "Ideenräume"
    span_ "Beauftragungsnetzwerk"
    span_ "Hi VorNac"
    span_ $ img_ [src_ "the_avatar"]
    hr_ []

footerMarkup :: forall m. (Monad m) => HtmlT m ()
footerMarkup = div_ $ do
    hr_ []
    -- TODO: these should be links
    span_ "Nutzungsbedingungen"
    span_ "Impressum"
    -- TODO: Should be on the right (and we need to specify encoding in html)
    span_ "Made with ♡ by Liqd"

-- | Debugging page, uses the 'Show' instance of the underlying type.
newtype PageShow a = PageShow { _unPageShow :: a }

instance Show a => ToHtml (PageShow a) where
    toHtmlRaw = toHtml
    toHtml = pre_ . code_ . toHtml . show . _unPageShow
