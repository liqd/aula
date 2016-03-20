{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Login
where

import Action (ActionM)
import qualified Action
import Frontend.Prelude

import qualified Frontend.Path as U
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF


-- * page

-- | 16. Home page with login prompt
data PageHomeWithLoginPrompt = PageHomeWithLoginPrompt (Maybe Bool)
  deriving (Eq, Show, Read)

instance Page PageHomeWithLoginPrompt where
    isPrivatePage _ = False


-- * templates

data LoginFormData = LoginFormData ST ST
  deriving (Eq, Ord, Show)

instance FormPage PageHomeWithLoginPrompt where
    type FormPageResult PageHomeWithLoginPrompt = LoginFormData

    formAction _ = relPath $ U.Login Nothing
    redirectOf _ = relPath U.ListSpaces

    makeForm _ = LoginFormData
        <$> ("user" .: DF.text Nothing)
        <*> ("pass" .: DF.text Nothing)

    formPage v fa p@(PageHomeWithLoginPrompt status) =
        semanticDiv p $ do
            div_ [class_ "login-register-form"] $ do
                h1_ [class_ "main-heading"] "Willkommen bei Aula"
                div_ . DF.form v fa $ do
                    unless (fromMaybe True status) $ do
                        p_ "Falscher Nutzername und/oder falsches Passwort."
                    inputText_     [placeholder_ "Dein Benutzername"] "user" v
                    inputPassword_ [placeholder_ "Dein Passwort"] "pass" v
                    inputSubmit_   [] "Login"
                    p_ [class_ "text-muted login-register-form-notice"]
                        "Solltest du dein Passwort nicht mehr kennen, melde dich bitte bei den Admins euer Schule."


-- * handlers

login :: (ActionM r action) => Maybe Bool -> ServerT (FormHandler PageHomeWithLoginPrompt) action
login status = redirectFormHandler (pure $ PageHomeWithLoginPrompt status) makeUserLogin
  where
    makeUserLogin (LoginFormData user _pass) = Action.login $ UserLogin user
