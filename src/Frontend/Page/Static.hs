{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Static
where

import Access (publicPage)
import Action (ActionM, query)
import Frontend.Prelude
import Persistent (termsOfUse)


-- * page

-- | 14. Static page: Imprint
data PageStaticImprint = PageStaticImprint
  deriving (Eq, Show, Read)

instance Page PageStaticImprint where
    isAuthorized = publicPage


-- * template

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


-- * page

-- | 15. Static page: Terms of use
data PageTermsOfUse = PageTermsOfUse Document
  deriving (Eq, Show, Read)

instance Page PageTermsOfUse where
    isAuthorized = publicPage


-- * template

instance ToHtml PageTermsOfUse where
    toHtmlRaw = toHtml
    toHtml p@(PageTermsOfUse terms) = semanticDiv p .
        div_ [class_ "text-markdown"] $
            terms ^. html

termsOfUse :: ActionM m => m PageTermsOfUse
termsOfUse = PageTermsOfUse <$> query Persistent.termsOfUse
