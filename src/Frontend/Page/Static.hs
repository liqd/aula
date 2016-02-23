{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Static
where

import Frontend.Prelude

----------------------------------------------------------------------
-- page

-- | 14. Static page: Imprint
data PageStaticImprint = PageStaticImprint
  deriving (Eq, Show, Read)

instance Page PageStaticImprint where
    isPublicPage _ = True

----------------------------------------------------------------------
-- template

instance ToHtml PageStaticImprint where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p $ do
        div_ $ h1_ "Impressum"
        div_ $ do
            div_ . p_ $ do
                b_ "Inhaltich"
                "politik-digital e.V."
            div_ . p_ $ do
                p_ $ b_ "Projektleitung"
                p_ $ b_ "Technische Leitung"

----------------------------------------------------------------------
-- page

-- | 15. Static page: Terms of use
data PageStaticTermsOfUse = PageStaticTermsOfUse
  deriving (Eq, Show, Read)

instance Page PageStaticTermsOfUse where
    isPublicPage _ = True

----------------------------------------------------------------------
-- template

instance ToHtml PageStaticTermsOfUse where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p $ do
        h1_ "Nutzungsbedingungen"
        p_  "Lorem ipsum"
        p_  "Duis aumet"
        p_  "Ut wisi"
