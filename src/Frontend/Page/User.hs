{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.User
where

import Action
import Frontend.Prelude

import qualified Frontend.Path as P
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

----------------------------------------------------------------------
-- page

-- | 9. User settings
data PageUserSettings = PageUserSettings User
  deriving (Eq, Show, Read)

instance Page PageUserSettings where
    isPrivatePage _ = True

data UserSettingData = UserSettingData
    { profileEmail    :: Maybe Email
    , profileOldPass  :: Maybe ST
    , profileNewPass1 :: Maybe ST
    , profileNewPass2 :: Maybe ST
    }
    deriving (Eq, Show)

instance FormPageView PageUserSettings where
    type FormPageResult PageUserSettings = UserSettingData

    formAction _ =
        P.path . P.TopTesting $ P.path P.UserSettings

    makeForm (PageUserSettings user) =
        UserSettingData
        <$> ("email"         .: (fmap Email <$> DF.optionalText (fmap unEmail $ user ^. userEmail)))
        <*> ("old-password"  .: DF.optionalText Nothing)
        <*> ("new-password1" .: DF.optionalText Nothing)
        <*> ("new-password2" .: DF.optionalText Nothing)
        where
            unEmail (Email e) = e

    formPage v fa p = do
        semanticDiv p $ do
            DF.form v fa $ do
                DF.inputText "email" v >> br_ []
                DF.inputText "old-password" v >> br_ []
                DF.inputText "new-password1" v >> br_ []
                DF.inputText "new-password2" v >> br_ []
                DF.inputSubmit "ANDERUNGEN SPEICHERN"

-- FIXME: Redirect to the right place
instance RedirectOf PageUserSettings where
    redirectOf _ = P.path $ P.TopTesting "/ideas"

userSettings :: (ActionM action) => ServerT (FormH HTML (Html ()) ST) action
userSettings = redirectFormHandler (PageUserSettings <$> currentUser) changeUser
  where
    -- FIXME: Set the password
    changeUser (UserSettingData email _oldPass _newPass1 _newPass2) = do
        modifyCurrentUser (maybe id (\ e -> userEmail .~ Just e) email)
