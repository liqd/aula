{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.ResetPassword
where

import Access
import Action
import Frontend.Prelude
import Frontend.Validation
import Persistent

import qualified Frontend.Path as U

import Text.Digestive as DF hiding (validate)


-- * password reset via email

data PasswordResetViaEmail = PasswordResetViaEmail
  deriving (Eq, Show, Read)

instance Page PasswordResetViaEmail where
    isAuthorized = loginPage

data ResetPasswordFormData =
        ResetPasswordFormData
            { unResetPasswordFormData :: EmailAddress }
  deriving (Eq, Show)

instance FormPage PasswordResetViaEmail where
    type FormPagePayload PasswordResetViaEmail = ResetPasswordFormData

    formAction _   = U.resetPasswordViaEmail
    redirectOf _ _ = U.login

    makeForm _ =
        ResetPasswordFormData
        <$> ("email" .: emailField "Email" Nothing)

    formPage v form p@PasswordResetViaEmail =
        semanticDiv p $ do
            div_ [class_ "login-register-form"] $ do
                h1_ [class_ "main-heading"] "Passwort neu setzen"
                p_ [class_ "text-muted login-register-form-notice"]
                    "Du kannst Dir einen Link an Deine email-Adresse schicken lassen, unter dem Du ein neues Passwort eingeben kannst."
                div_ . form $ do
                    inputText_     [placeholder_ "Deine email-Adresse"] "email" v
                    inputSubmit_   [] "Anfordern"


passwordResetViaEmail :: ActionM m => FormPageHandler m PasswordResetViaEmail
passwordResetViaEmail =
    formPageHandlerWithMsg
        (pure PasswordResetViaEmail)
        (Action.resetPasswordViaEmail . unResetPasswordFormData)
        "Die email mit dem Link wurde versendet."
    & formRequireCsrf .~ False


-- * finalize password via email

data FinalizePasswordViaEmail
    = FinalizePasswordViaEmail User PasswordToken PasswordTokenState
  deriving (Eq, Show, Read)

instance Page FinalizePasswordViaEmail where
    isAuthorized = loginPage

data FinalizePasswordViaEmailPayload = FinalizePasswordViaEmailPayload
    { newPassword1 :: ST
    , newPassword2 :: ST
    }
  deriving (Eq, Show)

instance FormPage FinalizePasswordViaEmail where
    type FormPagePayload FinalizePasswordViaEmail = FinalizePasswordViaEmailPayload

    formAction (FinalizePasswordViaEmail u t _) = U.finalizePasswordViaEmail u t
    redirectOf _ _ = U.login

    makeForm _ =
        DF.check "Die neuen Passwörter passen nicht (Tippfehler?)" checkNewPassword
        $ FinalizePasswordViaEmailPayload
            <$> ("new-password1" .:
                    validate "neues Passwort" passwordV (DF.text Nothing))
            <*> ("new-password2" .:
                    validate "neues Passwort (Wiederholung)" passwordV (DF.text Nothing))
                    -- FUTUREWORK: validating second password leads to obnoxious double-errors.
      where
        checkNewPassword u = newPassword1 u == newPassword2 u

    formPage v form p@(FinalizePasswordViaEmail user _token Valid) = do
        semanticDiv' [class_ "container-main container-narrow popup-page"] p $ do
            h1_ [class_ "main-heading"] $ "Neues Passwort fuer " <> (user ^. userLogin . unUserLogin . html)
            form $ do
                label_ $ do
                    span_ [class_ "label-text"] "Neues Passwort"
                    inputPassword_ [class_ "m-small"]
                        "new-password1" v
                label_ $ do
                    span_ [class_ "label-text"] "Neues Passwort bestätigen"
                    inputPassword_ [class_ "m-small"]
                        "new-password2" v
                footer_ [class_ "form-footer"] $ do
                    inputSubmit_ [] "Password speichern"

    formPage _v _form p@(FinalizePasswordViaEmail _user _token _state) = do
        semanticDiv' [class_ "container-main container-narrow popup-page"] p $ do
            h1_ [class_ "main-heading"] "Der Link ist abgelaufen"
            p_ "Auf der Login-Seite kannst du einen neuen Link anfordern."
            a_ [href_ U.login] "Zurueck zum Login"


finalizePasswordViaEmail :: ActionM m => AUID User -> PasswordToken -> FormPageHandler m FinalizePasswordViaEmail
finalizePasswordViaEmail uid tkn =
    formPageHandler
        (FinalizePasswordViaEmail
            <$> mquery (findUser uid)
            <*> pure tkn
            <*> Action.checkValidPasswordToken uid tkn)
        (Action.finalizePasswordViaEmail uid tkn . newPassword1)
