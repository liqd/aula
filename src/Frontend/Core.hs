{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Core
where

import Data.String.Conversions
import Data.Typeable
import Lucid
import Lucid.Base
import Network.Wai.Internal (Response(ResponseFile, ResponseBuilder, ResponseStream, ResponseRaw))
import Network.Wai (Middleware)
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


-- | This will generate the following snippet:
--
-- > <div data-aula="PageIdea"> ... </div>
--
-- Which serves two purposes:
--
--     * It helps the front-en developer to identify which part of the generated pages comes from which
--       combinator
--     * Later on when we write selenium suite, the semantic tags helps up to parse, identify and test
--       elements on the page.
semanticDiv :: forall m a. (Monad m, ToHtml a, Typeable a) => a -> HtmlT m () -> HtmlT m ()
semanticDiv t = div_ [makeAttribute "data-aula-type" (cs . show . typeOf $ t)]


-- | 'serveDirectory' lets wai guess the mime type, and wai's guess is not good enough.  This
-- 'Middleware' solves that.  (Alternatively, we could clone serveDirectory and solve the problem
-- closer to its cause, but the current solution makes it easier to add other tweaks as the need
-- arises.)
aulaTweaks :: Middleware
aulaTweaks app req cont = app req $ \resp -> do cont $ f resp
  where
    f :: Response -> Response
    f (ResponseFile s hs fp mfpart) = ResponseFile s (g <$> hs) fp mfpart
      where
        g ("content-type", "text/html") = ("content-type", "text/html;charset=utf8")
        g h = h
    f r@(ResponseBuilder _ _ _) = r
    f r@(ResponseStream _ _ _) = r
    f r@(ResponseRaw _ _) = r
