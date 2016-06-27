{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.User
where

import Access
import Action
import Codec.Picture (DynamicImage)
import Frontend.Fragment.DelegationTab
import Frontend.Fragment.IdeaList
import Frontend.Fragment.Note
import Frontend.Prelude hiding ((</>), (<.>))
import Frontend.Validation
import Persistent.Api
    ( SetUserEmail(SetUserEmail)
    , SetUserPass(SetUserPass)
    , SetUserDesc(SetUserDesc)
    )
import Persistent
    ( DelegateeListsMap(..)
    , DelegateeLists(..)
    , userDelegateeListsMap
    , findUser
    , findIdeasByUserId
    , getIdeaStats
    )

import qualified Data.Set as Set
import qualified Frontend.Path as U
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Types as DF (Result(..))
import qualified Text.Digestive.Lucid.Html5 as DF


-- * misc

setProfileContext :: User -> CapCtx -> CapCtx
setProfileContext user =
    set capCtxUserProfile (Just user) .
    set capCtxDelegateTo  (Just user)


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
data PageUserProfileCreatedIdeas =
        PageUserProfileCreatedIdeas CapCtx UserView ListItemIdeas DelegateeListsMap
  deriving (Eq, Show, Read)

instance Page PageUserProfileCreatedIdeas where
    isAuthorized = userPage -- Are profiles public?

-- | 8.2 User profile: Delegated votes
data PageUserProfileDelegatedVotes =
        PageUserProfileDelegatedVotes CapCtx UserView DelegateeListsMap
  deriving (Eq, Show, Read)

instance Page PageUserProfileDelegatedVotes where
    isAuthorized = userPage -- Are profiles public?

-- | 8.X User profile: Editing the public profile
data EditUserProfile = EditUserProfile { _eupCapCtx :: CapCtx, _eupUser :: User }
  deriving (Eq, Show, Read)

makeLenses ''EditUserProfile

instance Page EditUserProfile where
    isAuthorized = authNeedCaps [CanEditUser] eupCapCtx

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
verifyUserPassIfExists :: ActionM m => Maybe ST -> m Bool
verifyUserPassIfExists Nothing    = pure True
verifyUserPassIfExists (Just pwd) = verifyUserPass pwd . view userPassword <$> currentUser

instance FormPage PageUserSettings where
    type FormPagePayload PageUserSettings = UserSettingData

    formAction _ = U.userSettings
    redirectOf (PageUserSettings u) _
        | has (Frontend.Prelude.userSettings . userSettingsPassword . _UserPassInitial) u = U.listSpaces
        | otherwise = U.userSettings

    makeForm (PageUserSettings user) =
          DF.check "Die neuen Passwörter passen nicht (Tippfehler?)" checkNewPassword
        . DF.check "Passwort-Felder sind nur teilweise ausgefüllt."  checkPwdAllOrNothing
        $ UserSettingData
            <$> ("email"         .:
                    optionalEmailField "Email" (user ^. userEmail))
            <*> ("old-password"  .:
                    -- while we need to check that the old password is the correct
                    -- one, we do not need to validate it against the rules for new passwords.
                    -- (the user could provide nothing, but 'checkPwdAllOrNothing' will catch that.)
                    DF.checkM "Das alte Passwort ist nicht korrekt" verifyUserPassIfExists
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
        semanticDiv' [class_ "container-main container-narrow popup-page"] p $ do
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

userHeaderDiv :: (Monad m) => CapCtx -> Either User (User, DelegateeListsMap) -> HtmlT m ()
userHeaderDiv _ (Left user) =
    div_ $ do
        h1_ [class_ "main-heading"] $ user ^. userLogin . _UserLogin . html
        p_ "Dieser Nutzer ist gelöscht"

userHeaderDiv ctx (Right (user, delegations)) =
    div_ $ do
        div_ [class_ "heroic-avatar"] $ user ^. userAvatarImg avatarDefaultSize
        h1_ [class_ "main-heading"] $ user ^. userLogin . _UserLogin . html
        ul_ [class_ "role-badges"] $ do
            forM_ (user ^. userRoleSet . to Set.toList) $ \(r :: Role) ->
                li_ [class_ "badge"] $ r ^. uilabeled
        div_ [class_ "sub-header"] $ user ^. userDesc . html

        let btn lnk = a_ [class_ "btn-cta heroic-cta", href_ lnk]
            editProfileBtn = btn (U.editUserProfile user) "+ Profil bearbeiten"

        div_ [class_ "heroic-btn-group"] $ do
            let caps = capabilities ctx
            when (CanDelegateInClass `elem` caps || CanDelegateInSchool `elem` caps) $ do
                delegationButtons ctx user delegations
            btn (U.reportUser user) "melden"
            when (CanEditUser `elem` caps) $ do
                editProfileBtn

-- | NOTE: reflexive delegation is a thing!  the reasons are part didactic and part
-- philosophical, but it doesn't really matter: users can delegate to themselves
-- just like to anybody else, and the graph will look different if they do.
delegationButtons :: Monad m => CapCtx -> User -> DelegateeListsMap -> HtmlT m ()
delegationButtons (view capCtxUser -> delegatee)
                  delegate
                  (delegatedDScopes delegatee -> dscopes) = do
    let but = postButton_ [class_ "btn-cta heroic-cta", jsReloadOnClick]
    forM_ (commonSchoolClasses delegatee delegate) $ \clss -> do
        if DScopeIdeaSpace (ClassSpace clss) `elem` dscopes
            then do
                but (U.withdrawDelegationOnClassSpace delegate clss)
                    ("Beauftragung für Klasse " <> uilabel clss <> " entziehen")
            else do
                but (U.delegateVoteOnClassSpace delegate clss)
                    ("Für Klasse " <> uilabel clss <> " beauftragen")
    if DScopeIdeaSpace SchoolSpace `elem` dscopes
        then do
            but (U.withdrawDelegationOnSchoolSpace delegate)
                "Schulweite beauftragung entziehen"
        else do
            but (U.delegateVoteOnSchoolSpace delegate)
                "Schulweit beauftragen"

-- | All 'DScopes' in which user watching the profile has delegated to the profile owner.
delegatedDScopes :: User -> DelegateeListsMap -> [DScope]
delegatedDScopes delegatee (DelegateeListsMap xs) = fullDScopeToDScope . fst <$> filter pr xs
  where
    pr :: (a, DelegateeLists) -> Bool
    pr (_, DelegateeLists ys) = delegatee `elem` (fst <$> ys)


-- ** User Profile: Created Ideas

instance ToHtml PageUserProfileCreatedIdeas where
    toHtmlRaw = toHtml
    toHtml p@(PageUserProfileCreatedIdeas ctx (DeletedUser user) _ideas _delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx (Left user)
    toHtml p@(PageUserProfileCreatedIdeas ctx (ActiveUser user) ideas delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx (Right (user, delegations))
            -- Tab selection
            div_ [class_ "heroic-tabs"] $ do
                span_ [class_ "heroic-tab-item m-active"]
                    "Erstellte Ideen"
                a_ [class_ "heroic-tab-item", href_ (U.userDelegations user)]
                    "Erhaltene Stimmen"
        -- List of ideas
        div_ [class_ "m-shadow"] $ do
            div_ [class_ "grid"] $ toHtml ideas

-- | List all the created ideas for the given user.
createdIdeas :: (ActionPersist m, ActionUserHandler m)
    => AUID User -> IdeasQuery -> m PageUserProfileCreatedIdeas
createdIdeas userId ideasQuery = do
    ctx <- currentUserCapCtx
    let visibleByCurrentUser idea =
            case idea ^. ideaLocation . ideaLocationSpace of
                SchoolSpace  -> True
                ClassSpace c ->
                    case ctx ^. capCtxUser . userRoleScope of
                        SchoolScope      -> True
                        ClassesScope cls -> c `Set.member` cls
    equery (do
        user  <- maybe404 =<< findUser userId
        ideas <- ListItemIdeas ctx (IdeaInUserProfile user) ideasQuery
              <$> (applyFilter ideasQuery <$>
                    (mapM getIdeaStats
                     =<< filter visibleByCurrentUser
                         <$> findIdeasByUserId userId))
        delegatees <- userDelegateeListsMap userId
        pure $ PageUserProfileCreatedIdeas
            (setProfileContext user ctx)
            (makeUserView user)
            ideas
            delegatees)


-- ** User Profile: Delegated Votes

instance ToHtml PageUserProfileDelegatedVotes where
    toHtmlRaw = toHtml
    toHtml p@(PageUserProfileDelegatedVotes ctx (DeletedUser user) _delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx (Left user)
    toHtml p@(PageUserProfileDelegatedVotes ctx (ActiveUser user) delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx (Right (user, delegations))
            div_ [class_ "heroic-tabs"] $ do
                a_ [class_ "heroic-tab-item", href_ (U.viewUserProfile user)]
                    "Erstellte Ideen"
                span_ [class_ "heroic-tab-item  m-active"]
                    "Erhaltene Stimmen"
        div_ [class_ "m-shadow"] $ do
            div_ [class_ "grid"] $ do
                div_ [class_ "container-narrow"] $ do
                    renderDelegations True delegations

delegatedVotes :: (ActionPersist m, ActionUserHandler m)
      => AUID User -> m PageUserProfileDelegatedVotes
delegatedVotes userId = do
    ctx <- currentUserCapCtx
    equery $ do
        user <- maybe404 =<< findUser userId
        PageUserProfileDelegatedVotes
            (setProfileContext user ctx)
            (makeUserView user)
            <$> userDelegateeListsMap userId


-- ** User Profile: Edit profile

data UserProfileUpdate = UserProfileUpdate
    { _profileAvatar :: Maybe DynamicImage
    , _profileDesc   :: Document
    }

makeLenses ''UserProfileUpdate

instance FormPage EditUserProfile where
    type FormPagePayload EditUserProfile = UserProfileUpdate

    formAction (EditUserProfile _ctx u) = U.editUserProfile u
    redirectOf (EditUserProfile _ctx u) _ = U.viewUserProfile u

    makeForm (EditUserProfile _ctx user) =
        UserProfileUpdate
        <$> ("avatar" .: DF.validateM validateImageFile DF.file)
        <*> ("desc"   .: validate "Beschreibung" markdownV (DF.text . Just . unMarkdown $ user ^. userDesc))

    formPage v form p@(EditUserProfile ctx user) = do
        semanticDiv' [class_ "container-main container-narrow popup-page"] p $ do
            div_ [class_ "heroic-avatar"] $ user ^. userAvatarImg avatarDefaultSize
            h1_ [class_ "main-heading"] $ user ^. userLogin . _UserLogin . html
            ul_ [class_ "role-badges"] $ do
                forM_ (user ^. userRoleSet . to Set.toList) $ \(r :: Role) ->
                    li_ [class_ "badge"] $ r ^. uilabeled
            h1_ [class_ "main-heading"] .
                toHtml $ if isOwnProfile (ctx ^. capCtxUser) user
                    then "Eigenes Nutzerprofil bearbeiten"
                    else "Nutzerprofil von " <> user ^. userLogin . unUserLogin <> " bearbeiten"
            -- FIXME: merge this with 'userHeaderDiv'.

            form $ do
                label_ $ do
                    span_ [class_ "label-text"] "Avatar"
                    div_ $ do
                        "Einige wichtige Hinweise zum Hochladen von Bildern."
                        ul_ $ do
                            li_ $ do
                                "Das alte Bild wird beim hochladen überschrieben.  Ziehe dir bitte "
                                "jetzt zuerst eine Sicherheitskopie, wenn du es später noch brauchst."
                            li_ $ do
                                "Nach dem hochladen wird das neue Bild in Kreisform geschnitten. "
                                "Es sollte also nicht zu lang oder hoch sein und nichts wichtiges "
                                "in den Ecken zeigen."
                            li_ $ do
                                let dim = fromString . show . maximum $ avatarDefaultSize : avatarExtraSizes
                                "Das neue Bild sollte für optimale Qualität mindestens "
                                dim >> "x" >> dim >> " "
                                "Pixel haben."
                            li_ $ do
                                "Es darf nicht größer sein als " >> avatarMaxByteSize ^. html >> "."
                    DF.inputFile "avatar" v
                label_ $ do
                    span_ [class_ "label-text"] "Beschreibung"
                    inputTextArea_ [placeholder_ "..."] Nothing Nothing "desc" v
                footer_ [class_ "form-footer"] $ do
                    DF.inputSubmit "Änderungen speichern"
                    cancelButton p

validateImageFile :: ActionM m => Maybe FilePath -> m (DF.Result (Html ()) (Maybe DynamicImage))
validateImageFile = \case
    Nothing   -> pure $ DF.Success Nothing
    Just file -> do
        img <- readImageFile (cs file)
        pure $ case img of
            -- FIXME: what are the accepted formats?  be more specific and more accurate!
            Left _    -> DF.Error   "Die ausgewählte Datei ist kein Bild (jpg, png, gif, ...)"
            Right pic -> DF.Success $ Just pic


editUserProfile :: ActionM m => AUID User -> FormPageHandler m EditUserProfile
editUserProfile uid = formPageHandlerWithMsg
    (do user <- mquery (findUser uid)
        ctx  <- setProfileContext user <$> currentUserCapCtx
        pure $ EditUserProfile ctx user
    )
    (\up -> do
        case up ^. profileAvatar of
            Nothing ->
                -- FIXME: this should not be impossible
                throwError500 "IMPOSSIBLE: editUserProfile"
                -- update . SetUserDesc uid $ up ^. profileDesc
            Just pic -> do
                update . SetUserDesc uid $ up ^. profileDesc
                saveAvatar uid pic
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
