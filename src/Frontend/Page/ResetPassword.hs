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

instance FormPage PasswordResetViaEmail where
    type FormPagePayload PasswordResetViaEmail = ResetPasswordFormData

    formAction _   = U.resetPasswordViaEmail
    redirectOf _ _ = U.login

    makeForm _ =
        ResetPasswordFormData
        <$> ("email" .: emailField "Email" Nothing)

    -- TODO: Translate
    formPage v form p@PasswordResetViaEmail =
        semanticDiv p $ do
            div_ [class_ "login-register-form"] $ do
                h1_ [class_ "main-heading"] "Password reset"
                div_ . form $ do
                    inputText_     [placeholder_ "Your email"] "email" v
                    inputSubmit_   [] "Reset Password"
                    p_ [class_ "text-muted login-register-form-notice"]
                        "We will send a link to your email address."


passwordResetViaEmail :: ActionM m => FormPageHandler m PasswordResetViaEmail
passwordResetViaEmail =
    formPageHandlerWithMsg
        (pure PasswordResetViaEmail)
        (Action.resetPasswordViaEmail . unResetPasswordFormData)
        -- TODO: Translate
        "The email has sent to your email address"


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
      where
        checkNewPassword u = newPassword1 u == newPassword2 u

    -- TODO: Translate
    formPage v form p@(FinalizePasswordViaEmail user _token Valid) = do
        semanticDiv' [class_ "container-main container-narrow popup-page"] p $ do
            h1_ [class_ "main-heading"] $ "New password for " <> (user ^. userLogin . unUserLogin . html)
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

    -- TODO: Translate
    formPage _v _form p@(FinalizePasswordViaEmail _user _token _state) = do
        semanticDiv' [class_ "container-main container-narrow popup-page"] p $ do
            h1_ [class_ "main-heading"] "The password reset request is timed out"
            a_ [href_ U.login] "Back to login"



finalizePasswordViaEmail :: ActionM m => AUID User -> PasswordToken -> FormPageHandler m FinalizePasswordViaEmail
finalizePasswordViaEmail uid tkn =
    formPageHandler
        (FinalizePasswordViaEmail
            <$> mquery (findUser uid)
            <*> pure tkn
            <*> Action.checkValidPasswordToken uid tkn)
        (Action.finalizePasswordViaEmail uid tkn . newPassword1)
