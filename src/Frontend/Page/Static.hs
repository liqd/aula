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

-- TODO: Rename
-- | 15. Static page: Terms of use
data PageStaticTermsOfUse = PageStaticTermsOfUse Document
  deriving (Eq, Show, Read)

instance Page PageStaticTermsOfUse where
    isAuthorized = publicPage


-- * template

instance ToHtml PageStaticTermsOfUse where
    toHtmlRaw = toHtml
    toHtml p@(PageStaticTermsOfUse terms) = semanticDiv p .
        div_ [class_ "text-markdown"] $
            terms ^. html

termsOfUse :: ActionM m => m PageStaticTermsOfUse
termsOfUse = PageStaticTermsOfUse <$> query Persistent.termsOfUse
