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

import Action
import Data.Avatar
import Frontend.Fragment.IdeaList
import Frontend.Fragment.Note
import Frontend.Prelude hiding ((</>), (<.>))
import Frontend.Validation
import LifeCycle
import Persistent.Api
    ( SetUserEmail(SetUserEmail)
    , SetUserPass(SetUserPass)
    , SetUserProfileDesc(SetUserProfileDesc)
    , SetUserProfile(SetUserProfile)
    )
import Persistent (findUser, findIdeasByUserId, getListInfoForIdea)

import qualified Frontend.Path as U
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Types as DF
import qualified Text.Digestive.Lucid.Html5 as DF


-- * page

-- | 9. User settings
data PageUserSettings = PageUserSettings User
  deriving (Eq, Show, Read)

instance Page PageUserSettings

-- | 8.1 User profile: Created ideas
data PageUserProfileCreatedIdeas = PageUserProfileCreatedIdeas RenderContext UserView ListItemIdeas
  deriving (Eq, Show, Read)

instance Page PageUserProfileCreatedIdeas

-- | 8.2 User profile: Delegated votes
data PageUserProfileDelegatedVotes = PageUserProfileDelegatedVotes RenderContext UserView [Delegation]
  deriving (Eq, Show, Read)

instance Page PageUserProfileDelegatedVotes

-- | 8.X User profile: Editing the public profile
data EditUserProfile = EditUserProfile RenderContext User
  deriving (Eq, Show, Read)

instance Page EditUserProfile

-- | 8.X Report user profile
data ReportUserProfile = ReportUserProfile User
  deriving (Eq, Show, Read)

instance Page ReportUserProfile

-- * templates

-- ** User Settings

data UserSettingData = UserSettingData
    { profileEmail    :: Maybe EmailAddress
    , profileOldPass  :: Maybe ST
    , profileNewPass1 :: Maybe ST
    , profileNewPass2 :: Maybe ST
    }
    deriving (Eq, Show)

checkUserPassword :: (ActionM m) => UserSettingData -> m (DF.Result (Html ()) UserSettingData)
checkUserPassword u@(UserSettingData _      Nothing    _        _       ) = pure (pure u)
checkUserPassword u@(UserSettingData _email (Just pwd) _newpwd1 _newpwd2) =
    userPassElim checkInitialPwd checkEncryptedPwd passwordError
        . _userSettingsPassword
        . _userSettings
    <$> currentUser
  where
    passwordError = DF.Error "Das alte Passwort ist nicht korrekt"

    checkInitialPwd (InitialPassword p)
      | p == pwd  = pure u
      | otherwise = passwordError

    checkEncryptedPwd (FakeEncryptedPassword p)
      | p == cs pwd = pure u
      | otherwise   = passwordError

instance FormPage PageUserSettings where
    type FormPagePayload PageUserSettings = UserSettingData

    formAction _ = U.UserSettings
    redirectOf _ _ = U.UserSettings

    makeForm (PageUserSettings user) =
          DF.validateM checkUserPassword
        . DF.validate (checkPwdAllOrNothing <=< checkNewPassword)
        $ UserSettingData
            <$> ("email"         .:
                    emailField "Email" (user ^. userEmail))
            <*> ("old-password"  .:
                    -- no need to validate the current password
                    DF.optionalText Nothing)
            <*> ("new-password1" .:
                    validateOptional "neues Passwort" passwordV (DF.optionalText Nothing))
            <*> ("new-password2" .:
                    validateOptional "neues Passwort (Wiederholung)" passwordV (DF.optionalText Nothing))
      where
        checkPwdAllOrNothing u@(UserSettingData _ Nothing  Nothing  Nothing)  = pure u
        checkPwdAllOrNothing u@(UserSettingData _ (Just _) (Just _) (Just _)) = pure u
        checkPwdAllOrNothing _ = DF.Error "Passwort-Felder sind nur teilweise ausgefüllt."

        checkNewPassword u
          | profileNewPass1 u == profileNewPass2 u = pure u
          | otherwise = DF.Error "Die neuen Passwörter passen nicht (Tippfehler?)"

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
        (update . SetUserPass uid . FakeEncryptedPassword . cs) `mapM_` mnewPass1

userHeaderDiv :: (Monad m) => RenderContext -> UserView -> HtmlT m ()
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
                when True $ do  -- FIXME: `CanVoteIdea `elem` caps`, but caps need to be refactored for that.
                                -- FIXME: (in the process, we should merge CanVote{Idea,Topic}.  it's the same thing.)
                    btn U.Broken "Klassenweit beauftragen"
                    btn U.Broken "Schulweit beauftragen"
                btn (U.reportUser user) "melden"
                let caps = ctx ^. renderContextUser . userRole . to userCapabilities  -- TODO: this name introduction may have become unnecessary.
                eliminate caps CanEditUser
                    editProfileBtn
                    "[[[something grayed out.]]]"

-- | TODO: better name for this function.
eliminate :: (Eq cap, Monad m) => [Clickable cap] -> cap -> HtmlT m () -> HtmlT m () -> HtmlT m ()
eliminate caps cap clickable grayedout = do
    when (Clickable cap `elem` caps) clickable
    when (GrayedOut cap `elem` caps) grayedout


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
    ctx <- renderContext
    equery (do
        user  <- makeUserView <$> (maybe404 =<< findUser userId)
        ideas <- ListItemIdeas ctx IdeaInUserProfile
                    (IdeaLocationSpace SchoolSpace) emptyIdeasQuery
              <$> (findIdeasByUserId userId >>= mapM getListInfoForIdea)
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
    <$> renderContext
    <*> (makeUserView <$> mquery (findUser userId))
    <*> pure [] -- FIXME


-- ** User Profile: Edit profile

isOwnProfile :: RenderContext -> User -> Bool
isOwnProfile ctx user = ctx ^. renderContextUser . _Id == user ^. _Id

instance FormPage EditUserProfile where
    type FormPagePayload EditUserProfile = UserProfile

    formAction (EditUserProfile _ctx u) = U.editUserProfile u
    redirectOf (EditUserProfile _ctx u) _ = U.viewUserProfile u

    makeForm (EditUserProfile _ctx user) =
        UserProfile
        <$> ("avatar" .: (cs <$$> DF.file))
        <*> ("desc"   .: validate "Beschreibung" markdownV (dfTextField user userDesc _Markdown))

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
    (EditUserProfile <$> renderContext <*> mquery (findUser uid))
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
