{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.Login
where

import Text.Digestive as DF hiding (validate)

import Access
import Action (ActionM, mquery, query)
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
    isAuthorized = \case
        -- Redirect from login if the user is already logged in.
        NotLoggedIn -> accessGranted
        -- TODO: Translate
        LoggedIn{}  -> accessRedirected "You are already logged in" U.ListSpaces

-- FIXME: remove (or otherwise protect) this type before going to production!
data LoginDemoHints = LoginDemoHints { unLoginDemoHints :: [User] }
  deriving (Eq, Show, Read)


data ForgottenPassword = ForgottenPassword
  deriving (Eq, Show, Read)

instance Page ForgottenPassword where
    isAuthorized = pure accessGranted

data SetForgottenPassword = SetForgottenPassword User PassForgottenToken
  deriving (Eq, Show, Read)

instance Page SetForgottenPassword where
    -- TODO: Check if the user has the valid token
    isAuthorized = pure accessGranted


-- * login page

data LoginFormData = LoginFormData ST ST
  deriving (Eq, Ord, Show)

checkLogin :: (v ~ Html (), ActionM m) => LoginFormData -> m (Result v User)
checkLogin (LoginFormData uLogin pass) = do
    muser <- query $ findUserByLogin (UserLogin uLogin)
    pure $ case muser of
        Just user | verifyUserPass pass (user ^. userPassword) -> pure user
        _ -> Error $ span_ [class_ "form-error"] "Falscher Nutzername und/oder falsches Passwort."

instance FormPage PageHomeWithLoginPrompt where
    type FormPagePayload PageHomeWithLoginPrompt = User

    formAction _   = U.Login
    redirectOf _ _ = U.ListSpaces

    makeForm _ = validateM checkLogin $
        LoginFormData
        <$> ("user" .: validate "Login" usernameV (DF.string Nothing))
        <*> ("pass" .: DF.text Nothing)
            -- No validation is needed when login only when setting/changing passwords.

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
                    a_ [href_ U.forgottenPassword] "Forgot password?" -- TODO: Translation
            toHtml loginDemoHints


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
                forM_ users $ \u -> tr_ $ do
                    td_ $ u ^. userLogin . unUserLogin . html
                    td_ $ u ^. userRole . uilabeledST . html
                    td_ $ case u ^. userRole of
                              Student     c -> toHtml $ showSchoolClass c
                              ClassGuest  c -> toHtml $ showSchoolClass c
                              SchoolGuest   -> nil
                              Moderator     -> nil
                              Principal     -> nil
                              Admin         -> nil
                    td_ . toHtml $ (u ^. userEmailAddress :: ST)
                    td_ . toHtml $
                        case u ^. userPassword of
                            UserPassInitial (InitialPassword s) -> s
                            UserPassEncrypted{}                 -> "<hashed-password>"
                            UserPassDeactivated                 -> "<deactivated-password>"
                            UserPassForgotten{}                 -> "<forgotten-password>"



login :: ActionM m => FormPageHandler m PageHomeWithLoginPrompt
login = formPageHandler getPage Action.loginByUser
  where
    getPage = PageHomeWithLoginPrompt . LoginDemoHints <$> query getActiveUsers


-- * forgotten password

data ForgottenPasswordPayload = ForgottenPasswordPayload { unForgottenPasswordPayload :: ST }

instance FormPage ForgottenPassword where
    type FormPagePayload ForgottenPassword = ForgottenPasswordPayload

    formAction _   = U.forgottenPassword
    redirectOf _ _ = U.Login

    makeForm _ =
        ForgottenPasswordPayload
        <$> ("user" .: validate "Login" usernameV (DF.string Nothing))

    formPage v form p@ForgottenPassword =
        semanticDiv p $ do
            div_ [class_ "login-register-form"] $ do
                h1_ [class_ "main-heading"] "Please enter your username"
                div_ . form $ do
                    inputText_     [placeholder_ "Dein Benutzername"] "user" v
                    inputSubmit_   [] "Send password reset email"

forgottenPassword :: ActionM m => FormPageHandler m ForgottenPassword
forgottenPassword =
    formPageHandler
        (pure ForgottenPassword)
        (Action.forgottenPassword . UserLogin . unForgottenPasswordPayload)


-- * new password instead of a forgotten password

data SetForgottenPasswordPayload = SetForgottenPasswordPayload
    { newPassword1 :: ST
    , newPassword2 :: ST
    }

instance FormPage SetForgottenPassword where
    type FormPagePayload SetForgottenPassword = SetForgottenPasswordPayload

    formAction (SetForgottenPassword u t) = U.setForgottenPassword u t
    redirectOf _ _ = U.Login

    makeForm _ =
        DF.check "Die neuen Passwörter passen nicht (Tippfehler?)" checkNewPassword
        $ SetForgottenPasswordPayload
            <$> ("new-password1" .:
                    validate "neues Passwort" passwordV (DF.text Nothing))
            <*> ("new-password2" .:
                    validate "neues Passwort (Wiederholung)" passwordV (DF.text Nothing))
      where
        checkNewPassword u = newPassword1 u == newPassword2 u


    -- TODO: Translate
    formPage v form p@(SetForgottenPassword _user _token) = do
        semanticDiv' [class_ "container-main container-narrow popup-page"] p $ do
            h1_ [class_ "main-heading"] "New password"
            form $ do
                label_ $ do
                    span_ [class_ "label-text"] "neues Passwort"
                    inputPassword_ [class_ "m-small"]
                        "new-password1" v
                label_ $ do
                    span_ [class_ "label-text"] "neues Passwort bestätigen"
                    inputPassword_ [class_ "m-small"]
                        "new-password2" v
                footer_ [class_ "form-footer"] $ do
                    inputSubmit_ [] "Save password"

setForgottenPassword :: ActionM m => AUID User -> PassForgottenToken -> FormPageHandler m SetForgottenPassword
setForgottenPassword uid tkn =
    formPageHandler
        (SetForgottenPassword <$> mquery (findUser uid) <*> pure tkn)
        undefined -- (Action.forgottenPassword . UserLogin . unForgottenPasswordPayload)
