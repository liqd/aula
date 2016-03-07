{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Login
where

import Action (ActionM)
import qualified Action
import qualified Frontend.Path as P
import Frontend.Prelude

import qualified Frontend.Path as U
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

----------------------------------------------------------------------
-- page

-- | 16. Home page with login prompt
data PageHomeWithLoginPrompt = PageHomeWithLoginPrompt
  deriving (Eq, Show, Read)

instance Page PageHomeWithLoginPrompt where
    isPrivatePage _ = False

data PageLogout = PageLogout
  deriving (Eq, Show, Read)

instance Page PageLogout where
    isPrivatePage _ = False

----------------------------------------------------------------------
-- templates

data LoginFormData = LoginFormData ST ST
  deriving (Eq, Ord, Show)

instance FormPageView PageHomeWithLoginPrompt where
    type FormPageResult PageHomeWithLoginPrompt = LoginFormData

    formAction _ = relPath U.Login
    redirectOf _ = relPath U.ListSpaces

    makeForm _ = LoginFormData
        <$> ("user" .: DF.text Nothing)
        <*> ("pass" .: DF.text Nothing)

    formPage v fa p =
        semanticDiv p $ do
            div_ [class_ "container container-main"] $ do
                h1_ [class_ "main-heading"] "Willkommen bei Aula"
                div_ [class_ "login-register-form"] . DF.form v fa $ do
                    inputText_     [placeholder_ "Dein Benutzername"] "user" v
                    inputPassword_ [placeholder_ "Dein Passwort"] "pass" v
                    inputSubmit_   [class_ "btn-cta"] "Login"
                    p_ [class_ "text-muted login-register-form-notice"] "Solltest du dein Passwort nich mehr kennen, melde dich bitte bei den Admins euer Schule."

instance ToHtml PageLogout where
    toHtmlRaw = toHtml
    toHtml p@PageLogout = semanticDiv p $ do
        p_ "Du bist ausgelogt."
        button_ [onclick_ P.Login] "Login"

----------------------------------------------------------------------
-- handlers

login :: (ActionM action) => ServerT (FormHandler PageHomeWithLoginPrompt ST) action
login = redirectFormHandler (pure PageHomeWithLoginPrompt) makeUserLogin
  where
    makeUserLogin (LoginFormData user _pass) = Action.login $ UserLogin user

logout :: (ActionM m) => m (Frame PageLogout)
logout = Action.logout >> makeFrame PageLogout
