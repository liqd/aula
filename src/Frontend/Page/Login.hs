{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.Login
where

import Text.Digestive as DF hiding (validate)

import Action (ActionM, query)
import qualified Action
import Persistent
import Frontend.Prelude
import Frontend.Validation

import qualified Frontend.Path as U
import qualified Lucid


-- * page

-- | 16. Home page with login prompt
data PageHomeWithLoginPrompt = PageHomeWithLoginPrompt LoginDemoHints
  deriving (Eq, Show, Read)

instance Page PageHomeWithLoginPrompt where
    isPrivatePage _ = False

-- FIXME: remove (or otherwise protect) this type before going to production!
data LoginDemoHints = LoginDemoHints { unLoginDemoHints :: [User] }
  deriving (Eq, Show, Read)


-- * templates

data LoginFormData = LoginFormData ST ST
  deriving (Eq, Ord, Show)

checkLogin :: (v ~ Html (), ActionM m) => LoginFormData -> m (Result v User)
checkLogin (LoginFormData uLogin _pass) = do
    muser <- query $ findUserByLogin (UserLogin uLogin)
    pure $ case muser of
        Nothing ->
            Error $ span_ [class_ "form-error"] "Falscher Nutzername und/oder falsches Passwort."
        Just user -> do
            -- FIXME check password
            pure user

instance FormPage PageHomeWithLoginPrompt where
    type FormPagePayload PageHomeWithLoginPrompt = User

    formAction _   = U.Login
    redirectOf _ _ = U.ListSpaces

    makeForm _ = validateM checkLogin $
        LoginFormData
        <$> ("user" .: validate "Login" username (DF.string Nothing))
        <*> ("pass" .: validate "Passwort" password (DF.string Nothing))

    formPage v form p@(PageHomeWithLoginPrompt loginDemoHints) =
        semanticDiv p $ do
            div_ [class_ "login-register-form"] $ do
                h1_ [class_ "main-heading"] "Willkommen bei Aula"
                div_ . form $ do
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
            "DEMO-SYSTEM."
            br_ []
            br_ []
            "mailinator-emails können "
            a_ [Lucid.href_ "https://mailinator.com/"] "hier eingesehen werden."
            br_ []
            br_ []
            "LOGIN IST MIT FOLGENDEN NUTZERN MÖGLICH:"
            br_ []
            table_ [class_ "admin-table", style_ "padding: 30px"] $ do
                tr_ $ do
                    th_ "login"
                    th_ "rolle"
                    th_ "klasse"
                    th_ "email"
                    th_ "password"
                (\u -> tr_ $ do
                    td_ . toHtml $ u ^. userLogin . unUserLogin
                    td_ . toHtml $ u ^. userRole . labeledST
                    td_ $ case u ^. userRole of
                              Student     c -> toHtml $ showSchoolClass c
                              ClassGuest  c -> toHtml $ showSchoolClass c
                              SchoolGuest   -> nil
                              Moderator     -> nil
                              Principal     -> nil
                              Admin         -> nil
                    td_ . toHtml $ (u ^. userEmailAddress :: ST)
                    td_ . toHtml . (\case (UserPassInitial s) -> s; s -> cs $ show s) $ u ^. userPassword)
                  `mapM_` users


-- * handlers

login :: ActionM m => FormPageHandler m PageHomeWithLoginPrompt
login = formPageHandler getPage Action.loginByUser
  where
    getPage = PageHomeWithLoginPrompt . LoginDemoHints <$> query getActiveUsers
