{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Login
where

import qualified Text.Digestive.Form as DF

import Action (ActionM)
import qualified Action
import Frontend.Prelude

import qualified Frontend.Path as U


-- * page

-- | 16. Home page with login prompt
data PageHomeWithLoginPrompt = PageHomeWithLoginPrompt Bool LoginDemoHints
  deriving (Eq, Show, Read)

instance Page PageHomeWithLoginPrompt where
    isPrivatePage _ = False

-- FIXME: remove (or otherwise protect) this type before going to production!
data LoginDemoHints = LoginDemoHints { fromLoginDemoHints :: [User] }
  deriving (Eq, Show, Read)


-- * templates

data LoginFormData = LoginFormData ST ST
  deriving (Eq, Ord, Show)

instance FormPage PageHomeWithLoginPrompt where
    type FormPagePayload PageHomeWithLoginPrompt = LoginFormData

    formAction _ = relPath $ U.Login Nothing
    redirectOf _ _ = relPath U.ListSpaces

    makeForm _ = LoginFormData
        <$> ("user" .: DF.text Nothing)
        <*> ("pass" .: DF.text Nothing)

    formPage v form p@(PageHomeWithLoginPrompt status loginDemoHints) =
        semanticDiv p $ do
            div_ [class_ "login-register-form"] $ do
                h1_ [class_ "main-heading"] "Willkommen bei Aula"
                div_ . form $ do
                    unless status $ do
                        p_ "Falscher Nutzername und/oder falsches Passwort."
                    inputText_     [placeholder_ "Dein Benutzername"] "user" v
                    inputPassword_ [placeholder_ "Dein Passwort"] "pass" v
                    inputSubmit_   [] "Login"
                    p_ [class_ "text-muted login-register-form-notice"]
                        "Solltest du dein Passwort nicht mehr kennen, melde dich bitte bei den Admins euer Schule."
            toHtml loginDemoHints

    guardPage _ = do
        -- Redirect from login if the user is already logged in.
        li <- Action.isLoggedIn
        pure $ if li then Just $ relPath U.ListSpaces else Nothing


instance ToHtml LoginDemoHints where
    toHtmlRaw = toHtml
    toHtml (LoginDemoHints users) = do
        hr_ []
        div_ $ do
            "DEMO-SYSTEM.  LOGIN IST MIT FOLGENDEN NUTZERN MÖGLICH:"
            table_ [class_ "admin-table", style_ "padding: 30px"] $ do
                tr_ $ do
                    th_ "login"
                    th_ "password"
                (\u -> tr_ $ do
                    td_ . toHtml $ u ^. userLogin . fromUserLogin
                    td_ . toHtml . (\case (UserPassInitial s) -> s; s -> cs $ show s) $ u ^. userPassword)
                  `mapM_` users


-- * handlers

login :: (ActionM r action) => Bool -> ServerT (FormHandler PageHomeWithLoginPrompt) action
login success = redirectFormHandler getPage makeUserLogin
  where
    makeUserLogin (LoginFormData user _pass) = Action.login $ UserLogin user
    getPage = PageHomeWithLoginPrompt success . LoginDemoHints <$> Action.persistent getUsers
