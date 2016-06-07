{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.User
where

import System.FilePath

import Access
import Action
import Data.Avatar
import Frontend.Fragment.IdeaList
import Frontend.Fragment.Note
import Frontend.Prelude hiding ((</>), (<.>))
import Frontend.Validation
import Persistent.Api
    ( SetUserEmail(SetUserEmail)
    , SetUserPass(SetUserPass)
    , SetUserProfileDesc(SetUserProfileDesc)
    , SetUserProfile(SetUserProfile)
    )
import Persistent (findUser, findIdeasByUserId, getIdeaStats)

import qualified Frontend.Path as U
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF


-- * page

-- | 9. User settings
data PageUserSettings = PageUserSettings User
  deriving (Eq, Show, Read)

instance Page PageUserSettings where
    -- The use settings page always goes to the profile of the current logged in user.
    -- However we do not want to rely on that so we check that the current user is the
    -- same as the edited user.
    isAuthorized = authNeedPage $ \cUser (PageUserSettings u) ->
        if cUser ^. _Id == u ^. _Id
            then accessGranted
            else accessDenied Nothing

-- | 8.1 User profile: Created ideas
data PageUserProfileCreatedIdeas = PageUserProfileCreatedIdeas CapCtx UserView ListItemIdeas
  deriving (Eq, Show, Read)

instance Page PageUserProfileCreatedIdeas where
    isAuthorized = userPage -- Are profiles public?

-- | 8.2 User profile: Delegated votes
data PageUserProfileDelegatedVotes = PageUserProfileDelegatedVotes CapCtx UserView [Delegation]
  deriving (Eq, Show, Read)

instance Page PageUserProfileDelegatedVotes where
    isAuthorized = userPage -- Are profiles public?

-- | 8.X User profile: Editing the public profile
data EditUserProfile = EditUserProfile CapCtx User
  deriving (Eq, Show, Read)

instance Page EditUserProfile where
    -- Can the admin edit any profile through that endpoint?
    isAuthorized = authNeedPage $ \_ (EditUserProfile ctx u) ->
        if isOwnProfile ctx u
            then accessGranted
            else accessDenied Nothing

-- | 8.X Report user profile
data ReportUserProfile = ReportUserProfile User
  deriving (Eq, Show, Read)

instance Page ReportUserProfile where
    -- If you can view the profile of a user then you can report on it.
    -- Any user who is logged in can view the profile of any other user.
    isAuthorized = userPage


-- * templates

-- ** User Settings

data UserSettingData = UserSettingData
    { profileEmail    :: Maybe EmailAddress
    , profileOldPass  :: Maybe ST
    , profileNewPass1 :: Maybe ST
    , profileNewPass2 :: Maybe ST
    }
    deriving (Eq, Show)

-- This function checks that IF provided the password must be correct.
-- See checkPwdAllOrNothing which checks that the three passwords are present at once.
checkUserPassword :: ActionM m => Maybe ST -> m Bool
checkUserPassword Nothing    = pure True
checkUserPassword (Just pwd) = verifyUserPass pwd . view userPassword <$> currentUser

instance FormPage PageUserSettings where
    type FormPagePayload PageUserSettings = UserSettingData

    formAction _ = U.UserSettings
    redirectOf _ _ = U.UserSettings

    makeForm (PageUserSettings user) =
          DF.check "Die neuen Passwörter passen nicht (Tippfehler?)" checkNewPassword
        . DF.check "Passwort-Felder sind nur teilweise ausgefüllt."  checkPwdAllOrNothing
        $ UserSettingData
            <$> ("email"         .:
                    emailField "Email" (user ^. userEmail))
            <*> ("old-password"  .:
                    -- while we need to the check that the old password is the correct
                    -- one we do not need to validate it against the rules for new passwords.
                    DF.checkM "Das alte Passwort ist nicht korrekt" checkUserPassword
                    (DF.optionalText Nothing))
            <*> ("new-password1" .:
                    validateOptional "neues Passwort" passwordV (DF.optionalText Nothing))
            <*> ("new-password2" .:
                    validateOptional "neues Passwort (Wiederholung)" passwordV (DF.optionalText Nothing))
      where
        checkPwdAllOrNothing (UserSettingData _ Nothing  Nothing  Nothing)  = True
        checkPwdAllOrNothing (UserSettingData _ (Just _) (Just _) (Just _)) = True
        checkPwdAllOrNothing _                                              = False

        checkNewPassword u = profileNewPass1 u == profileNewPass2 u

    formPage v form p = do
        semanticDiv p $ do
            div_ [class_ "container-main popup-page"] $ do
                div_ [class_ "container-narrow"] $ do
                    h1_ [class_ "main-heading"] "Einstellungen"
                    form $ do
                        label_ $ do
                            span_ [class_ "label-text"] "E-mailadresse (optional)"
                            inputText_ [class_ "m-small"] -- FIXME should be inputEmail_
                                "email" v
                        h2_ [class_ "label-header"] "Passwort ändern"
                        label_ $ do
                            span_ [class_ "label-text"] "aktualles Passwort"
                            inputPassword_ [class_ "m-small"]
                                "old-password" v
                        label_ $ do
                            span_ [class_ "label-text"] "neues Passwort"
                            inputPassword_ [class_ "m-small"]
                                "new-password1" v
                        label_ $ do
                            span_ [class_ "label-text"] "neues Passwort bestätigen"
                            inputPassword_ [class_ "m-small"]
                                "new-password2" v
                        footer_ [class_ "form-footer"] $ do
                            DF.inputSubmit "Änderungen speichern"


userSettings :: forall m . ActionM m => FormPageHandler m PageUserSettings
userSettings =
    formPageHandlerWithMsg
        (PageUserSettings <$> currentUser)
        changeUser
        "Die Änderungen wurden gespeichert."
  where
    changeUser :: UserSettingData -> m ()
    changeUser (UserSettingData memail _moldPass mnewPass1 mnewPass2) = do
        uid <- currentUserId
        (update . SetUserEmail uid) `mapM_` memail
        when (mnewPass1 /= mnewPass2) $ throwError500 "passwords do not match!"
        forM_ mnewPass1 $ encryptPassword >=> update . SetUserPass uid

userHeaderDiv :: (Monad m) => CapCtx -> UserView -> HtmlT m ()
userHeaderDiv _   (DeletedUser user) =
    div_ $ do
        h1_ [class_ "main-heading"] $ user ^. userLogin . _UserLogin . html
        p_ "Dieser Nutzer ist gelöscht"

userHeaderDiv ctx (ActiveUser user) =
    div_ $ do
        div_ [class_ "heroic-avatar"] $ user ^. userAvatar . to avatarImgFromMaybeURL
        h1_ [class_ "main-heading"] $ user ^. userLogin . _UserLogin . html
        span_ [class_ "post-title"] $ user ^. userRole . roleSchoolClass . to showSchoolClass . html
        div_ [class_ "sub-header"] $ user ^. userDesc . html

        let btn lnk = a_ [class_ "btn-cta heroic-cta", href_ lnk]
            editProfileBtn = btn (U.editUserProfile user) "+ Profil bearbeiten"

        div_ [class_ "heroic-btn-group"] $ if isOwnProfile ctx user
            then do
                editProfileBtn
            else do
                let caps = capabilities ctx
                when (CanDelegate `elem` caps) $ do
                    btn U.Broken "Klassenweit beauftragen"
                    btn U.Broken "Schulweit beauftragen"
                btn (U.reportUser user) "melden"
                when (CanEditUser `elem` caps) editProfileBtn


-- ** User Profile: Created Ideas

instance ToHtml PageUserProfileCreatedIdeas where
    toHtmlRaw = toHtml
    toHtml p@(PageUserProfileCreatedIdeas ctx u@(DeletedUser _user) _ideas) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx u
    toHtml p@(PageUserProfileCreatedIdeas ctx u@(ActiveUser user) ideas) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx u
            -- Tab selection
            div_ [class_ "heroic-tabs"] $ do
                span_ [class_ "heroic-tab-item m-active"]
                    "Erstellte Ideen"
                a_ [class_ "heroic-tab-item", href_ (U.UserProf (user ^. _Id) U.UserDelegations)]
                    "Erhaltene Stimmen"
        -- List of ideas
        div_ [class_ "m-shadow"] $ do
            div_ [class_ "grid"] $ toHtml ideas

-- | List all the created ideas for the given user.
createdIdeas :: (ActionPersist m, ActionUserHandler m)
    => AUID User -> m PageUserProfileCreatedIdeas
createdIdeas userId = do
    ctx <- currentUserCapCtx
    equery (do
        user  <- makeUserView <$> (maybe404 =<< findUser userId)
        ideas <- ListItemIdeas ctx IdeaInUserProfile
                    (IdeaLocationSpace SchoolSpace) emptyIdeasQuery
              <$> (findIdeasByUserId userId >>= mapM getIdeaStats)
        pure $ PageUserProfileCreatedIdeas ctx user ideas)


-- ** User Profile: Delegated Votes

instance ToHtml PageUserProfileDelegatedVotes where
    toHtmlRaw = toHtml
    toHtml p@(PageUserProfileDelegatedVotes ctx u@(DeletedUser _user) _delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx u
    toHtml p@(PageUserProfileDelegatedVotes ctx u@(ActiveUser user) delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx u
            div_ [class_ "heroic-tabs"] $ do
                a_ [class_ "heroic-tab-item", href_ (U.viewUserProfile user)]
                    "Erstellte Ideen"
                span_ [class_ "heroic-tab-item  m-active"]
                    "Erhaltene Stimmen"
        div_ [class_ "m-shadow"] $ do
            div_ [class_ "grid"] $ do
                div_ [class_ "container-narrow"] $ do
                    -- School / Class select buttons: FIXME mechanics!
                    div_ [class_ "filter-toggles"] $ do
                        button_ [class_ "filter-toggle-btn", value_ ""] "Schulweit"
                        button_ [class_ "filter-toggle-btn m-active", value_ ""] "Klassenweit"
                    renderDelegations delegations

renderDelegations :: forall m. Monad m => [Delegation] -> HtmlT m ()
renderDelegations _ = do
    h2_ $ "Insgesamt " <> total ^. showed . html
    ul_ [class_ "small-avatar-list"] $ renderLi `mapM_` [undefined, undefined, undefined]  -- FIXME
  where
    total :: Int
    total = 20

    renderLi :: Delegation -> HtmlT m ()  -- FIXME
    renderLi _ = do
        li_ [class_ "small-avatar-list-item"] $ do
            div_ [class_ "col-1-12"] $ do
                div_ [class_ "small-avatar-list-image"] $ do
                    nil -- FIXME Make a real image a child here (avatarImgFromHasMeta)
            div_ [class_ "col-11-12"] $ do
                h3_ "UserName"
                p_ $ do
                    "5 Stimmen von "
                    strong_ $ do
                        a_ [href_ U.Broken] "UserName, "
                        a_ [href_ U.Broken] "UserName, "
                        a_ [href_ U.Broken] "UserName"

delegatedVotes :: (ActionPersist m, ActionUserHandler m)
      => AUID User -> m PageUserProfileDelegatedVotes
delegatedVotes userId = do
    PageUserProfileDelegatedVotes
    <$> currentUserCapCtx
    <*> (makeUserView <$> mquery (findUser userId))
    <*> pure [] -- FIXME


-- ** User Profile: Edit profile

instance FormPage EditUserProfile where
    type FormPagePayload EditUserProfile = UserProfile

    formAction (EditUserProfile _ctx u) = U.editUserProfile u
    redirectOf (EditUserProfile _ctx u) _ = U.viewUserProfile u

    makeForm (EditUserProfile _ctx user) =
        UserProfile
        <$> ("avatar" .: (cs <$$> DF.file))
        <*> ("desc"   .: validate "Beschreibung" markdownV (DF.text . Just . unMarkdown $ user ^. userDesc))

    formPage v form p@(EditUserProfile ctx user) = do
        semanticDiv p $ do
            div_ [class_ "container-main popup-page"] $ do
                div_ [class_ "container-narrow"] $ do
                    h1_ [class_ "main-heading"] .
                        toHtml $ if isOwnProfile ctx user
                            then "Eigenes Nutzerprofil bearbeiten"
                            else "Nutzerprofil von " <> user ^. userLogin . unUserLogin <> " bearbeiten"
                    form $ do
                        label_ $ do
                            span_ [class_ "label-text"] "Avatar"
                            DF.inputFile "avatar" v
                        label_ $ do
                            span_ [class_ "label-text"] "Beschreibung"
                            inputTextArea_ [placeholder_ "..."] Nothing Nothing "desc" v
                        footer_ [class_ "form-footer"] $ do
                            DF.inputSubmit "Änderungen speichern"

editUserProfile :: ActionM m => AUID User -> FormPageHandler m EditUserProfile
editUserProfile uid = formPageHandlerWithMsg
    (EditUserProfile <$> currentUserCapCtx <*> mquery (findUser uid))
    (\up -> do
        case up ^. profileAvatar of
            Nothing ->
                -- FIXME: this should not be impossible
                throwError500 "IMPOSSIBLE: editUserProfile"
                -- update . SetUserProfileDesc uid $ up ^. profileDesc
            Just file -> do
                let dst = "static" </> "avatars" </> cs (uriPart uid) <.> "png"
                    url = "/" <> dst
                img <- readImageFile (cs file)
                case img of
                    Left _e ->
                        -- FIXME: this should be dealt with the Nothing case.
                        -- throwError500 $ "image decoding failed: " <> e
                        update . SetUserProfileDesc uid $ up ^. profileDesc
                    Right pic -> savePngImageFile dst (dynamicResize (53, 53) pic)
                update . SetUserProfile uid $ up & profileAvatar ?~ cs url
    )
    "Die Änderungen wurden gespeichert."


-- ** User profile: Report user

reportUserNote :: Note User
reportUserNote = Note
    { noteHeaderText                = ("Report: " <>) . view (userLogin . unUserLogin)
    , noteExplanation               = Just "Hier kannst ein Nutzerprofil wegen eines verletzenden oder anstößigen Inhalts beim Moderationsteam melden. Das Team erhält eine Benachrichtigung und wird die Idee schnellstmöglich überprüfen. Bitte gib unten einen Grund an, warum du den Inhalt für anstößig oder verletzend hältst."
    , noteLabelText                 = "Warum möchtest du das Nutzerprofil melden?"
    , noteFieldNameInValiationError = "Begründung"
    }

instance FormPage ReportUserProfile where
    type FormPagePayload ReportUserProfile = Document

    formAction (ReportUserProfile user) = U.reportUser user
    redirectOf (ReportUserProfile user) _ = U.viewUserProfile user

    makeForm ReportUserProfile{} =
        noteFormInput reportUserNote Nothing

    formPage v form p@(ReportUserProfile user) =
        semanticDiv p $ do
            noteForm reportUserNote v form user

reportUser :: AUID User -> ActionM m => FormPageHandler m ReportUserProfile
reportUser userId = formPageHandlerWithMsg
    (ReportUserProfile <$> mquery (findUser userId))
    (Action.reportUser userId)
    "Das Nutzerprofil wurde der Moderation gemeldet."
