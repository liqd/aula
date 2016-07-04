{-# LANGUAGE OverloadedStrings #-}
module Frontend.Translation.Login
where

import Lucid.I18N
import Frontend.Prelude

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}

-- * translation keys

-- this section should be auto-generated from transifex data with TH.

t_forgot_passwd_reset_yourself :: Monad m => HtmlT m ()
t_forgot_passwd_reset_yourself = do
    _lang <- getLang
    "Wenn Du eine email-Adresse eingegeben hast, kannst du dein Passwort hier neu setzen."

t_forgot_passwd_reset_with_admin :: Monad m => HtmlT m ()
t_forgot_passwd_reset_with_admin = do
    "Solltest du dein Passwort nicht mehr kennen und keine email-Adresse haben, melde dich bitte bei den Admins euer Schule."

