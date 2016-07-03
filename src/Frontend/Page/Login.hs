{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.Login
where

import Text.Digestive as DF hiding (validate)
import qualified Data.Text as ST
import qualified Lucid

import Access
import Action (ActionM, query)
import qualified Action
import Persistent
import Frontend.Prelude
import Frontend.Validation
import qualified Frontend.Path as U


-- * translation keys

-- this section should be auto-generated from transifex data with TH.

t_forgot_passwd_reset_yourself :: Monad m => HtmlT m ()
t_forgot_passwd_reset_yourself = do
    _lang :: Lang <- ask
    "Wenn Du eine email-Adresse eingegeben hast, kannst du dein Passwort hier neu setzen."

t_forgot_passwd_reset_with_admin :: Monad m => HtmlT m ()
t_forgot_passwd_reset_with_admin = do
    "Solltest du dein Passwort nicht mehr kennen und keine email-Adresse haben, melde dich bitte bei den Admins euer Schule."


-- * page

-- | 16. Home page with login prompt
data PageHomeWithLoginPrompt = PageHomeWithLoginPrompt LoginDemoHints
  deriving (Eq, Show, Read)

instance Page PageHomeWithLoginPrompt where
    isAuthorized = loginPage

-- FIXME: remove (or otherwise protect) this type before going to production!
data LoginDemoHints = LoginDemoHints { unLoginDemoHints :: [User] }
  deriving (Eq, Show, Read)


-- * templates

data LoginFormData = LoginFormData ST ST
  deriving (Eq, Ord, Show)

checkLogin :: (Monad n, v ~ HtmlT n (), ActionM m) => LoginFormData -> m (Result v User)
checkLogin (LoginFormData uLogin pass) = do
    muser <- query $ findUserByLogin (UserLogin uLogin)
    pure $ case muser of
        Just user | verifyUserPass pass (user ^. userPassword) -> pure user
        _ -> Error $ span_ [class_ "form-error"] "Falscher Nutzername und/oder falsches Passwort."

instance FormPage PageHomeWithLoginPrompt where
    type FormPagePayload PageHomeWithLoginPrompt = User

    formAction _   = U.login
    redirectOf _ _ = U.completeRegistration

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
                    p_ [class_ "text-muted login-register-form-notice"] $ do
                        a_ [href_ U.resetPasswordViaEmail]
                            t_forgot_passwd_reset_yourself
                        br_ nil
                        t_forgot_passwd_reset_with_admin
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
            "(Das voreingestellte Passwort bei Nutzern mit \"<hashed-password>\" ist "
            initialDemoPassword ^. showed . html <> ".  "
            "Initiale Passwörter, die noch geändert werden müssen, werden angezeigt.)"
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
                    td_ . toHtml $ ST.intercalate ","
                                    (u ^.. userRoles . uilabeled)
                    td_ . toHtml $ ST.intercalate ","
                                    (u ^.. userSchoolClasses . uilabeled)
                    td_ . toHtml $ (u ^. userEmailAddress :: ST)
                    td_ . toHtml $
                        case u ^. userPassword of
                            UserPassInitial (InitialPassword s) -> s
                            UserPassEncrypted{}                 -> "<hashed-password>"
                            UserPassDeactivated                 -> "<deactivated-password>"


-- * handlers

login :: ActionM m => FormPageHandler m PageHomeWithLoginPrompt
login = formPageHandlerWithoutCsrf getPage Action.loginByUser
  where
    getPage = PageHomeWithLoginPrompt . LoginDemoHints <$> query getActiveUsers
