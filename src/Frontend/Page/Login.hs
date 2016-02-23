{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Login
where

import Action (ActionM)
import qualified Action
import Frontend.Prelude

import qualified Frontend.Path as P
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

data LoginFormData = LoginFormData ST ST
  deriving (Eq, Ord, Show)

instance FormPageView PageHomeWithLoginPrompt where
    type FormPageResult PageHomeWithLoginPrompt = LoginFormData

    formAction _ = P.path P.Login

    makeForm _ = LoginFormData
        <$> ("user" .: DF.text Nothing)
        <*> ("pass" .: DF.text Nothing)

    formPage v fa p =
        semanticDiv p $ do
            div_ . DF.form v fa $ do
                DF.inputText     "user" v >> br_ []
                DF.inputPassword "pass" v >> br_ []
                DF.inputSubmit   "Login"
            div_ $ do
                p_ "Solltest du dein Passwort nich mehr kennen, melde dich bitte bei den Admins euer Schule."

----------------------------------------------------------------------
-- handlers

instance RedirectOf PageHomeWithLoginPrompt where
    redirectOf _ = P.path P.SpaceAll

login :: (ActionM action) => ServerT (FormH HTML (Html ()) ST) action
login = redirectFormHandler (pure PageHomeWithLoginPrompt) makeUserLogin
  where
    makeUserLogin (LoginFormData user _pass) = Action.login user
