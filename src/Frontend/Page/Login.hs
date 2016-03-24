{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Login
where

import Text.Digestive

import Action (ActionM, persistent)
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

checkLogin :: (v ~ Html (), ActionM r m) => LoginFormData -> m (Result v User)
checkLogin (LoginFormData uLogin _pass) = do
    muser <- persistent $ findUserByLogin (UserLogin uLogin)
    pure $ case muser of
        Nothing ->
            Error $ span_ [class_ "form-error"] "Falscher Nutzername und/oder falsches Passwort."
        Just user -> do
            -- FIXME check password
            pure user

instance FormPage PageHomeWithLoginPrompt where
    type FormPagePayload PageHomeWithLoginPrompt = User

    formAction _ = relPath $ U.Login Nothing
    redirectOf _ _ = relPath U.ListSpaces

    makeForm _ = validateM checkLogin $
        LoginFormData
        <$> ("user" .: text Nothing)
        <*> ("pass" .: text Nothing)

    formPage v form p@(PageHomeWithLoginPrompt status loginDemoHints) =
        semanticDiv p $ do
            div_ [class_ "login-register-form"] $ do
                h1_ [class_ "main-heading"] "Willkommen bei Aula"
                div_ . form $ do
                    unless status $ do
                        span_ [class_ "form-error"] "Falscher Nutzername und/oder falsches Passwort."
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
login success = redirectFormHandler getPage Action.login
  where
    getPage = PageHomeWithLoginPrompt success . LoginDemoHints <$> Action.persistent getUsers
