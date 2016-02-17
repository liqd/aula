{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.HomePageWithLoginPrompt
where

import Frontend.Prelude

import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

----------------------------------------------------------------------
-- page

-- | 16. Home page with login prompt
data PageHomeWithLoginPrompt = PageHomeWithLoginPrompt
  deriving (Eq, Show, Read)

instance Page PageHomeWithLoginPrompt where
  isPublicPage _ = True

----------------------------------------------------------------------
-- templates

instance ToHtml PageHomeWithLoginPrompt where
    toHtmlRaw = toHtml
    toHtml p = semanticDiv p "PageHomeWithLoginPrompt"

newtype LoginFormData = LoginFormData (ST, ST)
  deriving (Eq, Ord, Show)

instance FormPageView PageHomeWithLoginPrompt where
    type FormPageResult PageHomeWithLoginPrompt = LoginFormData

    makeForm _ = LoginFormData <$> do
        (,) <$> ("user" .: DF.text Nothing)
            <*> ("pass" .: DF.text Nothing)

    formPage v formAction p = do
        semanticDiv p $ do
            div_ $ DF.form v formAction $ do
                DF.inputText     "user" v >> br_ []
                DF.inputPassword "pass" v >> br_ []
                DF.inputSubmit   "Login"
            div_ $ do
                p_ $ "Solltest du dein Passwort nich mehr kennen, melde dich bitte bei den Admins euer Schule."

----------------------------------------------------------------------
-- handlers

instance RedirectOf PageHomeWithLoginPrompt where
    redirectOf _ = "/ideas"

login :: Server (FormH HTML (Html ()) ST)
login = redirectFormHandler "/login" PageHomeWithLoginPrompt makeUserLogin
  where
    makeUserLogin (LoginFormData (user, _pass)) = liftIO . runPersist $ loginUser user
