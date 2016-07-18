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
    ( EQuery
    , DelegationListsMap(..)
    , DelegateeLists(..)
    , userDelegationListsMap
    , userDelegateListsMap
    , findUser
    , findIdeasByUserId
    , getIdeaStats
    , delegateInScope
    )

import qualified Data.Set as Set
import qualified Frontend.Path as U
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Types as DF (Result(..))
import qualified Text.Digestive.Lucid.Html5 as DF


-- * misc

profileContext :: (ActionPersist m, ActionUserHandler m) => User -> m CapCtx
profileContext user = set capCtxUserProfile (Just user)
                    . set capCtxDelegateTo  (Just user)
                  <$> currentUserCapCtx


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
        PageUserProfileCreatedIdeas CapCtx UserView ListItemIdeas [Delegation]
  deriving (Eq, Show, Read)

instance Page PageUserProfileCreatedIdeas where
    isAuthorized = userPage -- profiles are public.  (see #695)

-- | 8.2 User profile: Votes from delegatees
data PageUserProfileUserAsDelegate =
        PageUserProfileUserAsDelegate CapCtx UserView DelegationListsMap [Delegation]
  deriving (Eq, Show, Read)

instance Page PageUserProfileUserAsDelegate where
    isAuthorized = userPage -- profiles are public.  (see #695)

-- | 8.X User profile: Votes to delegates
data PageUserProfileUserAsDelegatee =
        PageUserProfileUserAsDelegatee CapCtx UserView DelegationListsMap [Delegation]
  deriving (Eq, Show, Read)

instance Page PageUserProfileUserAsDelegatee where
    isAuthorized = userPage -- profiles are public.  (see #695)

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

userHeaderDiv :: (Monad m) => CapCtx -> Either User (User, [Delegation]) -> HtmlT m ()
userHeaderDiv _ (Left user) =
    div_ $ do
        h1_ [class_ "main-heading"] $ user ^. userLogin . _UserLogin . html
        p_ "Dieser Nutzer ist gelöscht"

userHeaderDiv ctx (Right (user, delegations)) =
    div_ $ do
        userHeaderDivCore user
        div_ [class_ "sub-header"] $ user ^. userDesc . html

        let btn lnk = a_ [class_ "btn-cta heroic-cta", href_ lnk]
            editProfileBtn = btn (U.editUserProfile user) "+ Profil bearbeiten"

        div_ [class_ "heroic-btn-group"] $ do
            let caps = capabilities ctx
            when (any (`elem` caps) [CanDelegateInClass, CanDelegateInSchool]) $ do
                delegationButtons (ctx ^. capCtxUser) user delegations
            unless (isOwnProfile (ctx ^. capCtxUser) user) $
                btn (U.reportUser user) "melden"
            when (CanEditUser `elem` caps) $ do
                editProfileBtn

userHeaderDivCore :: User -> Monad m => HtmlT m ()
userHeaderDivCore user = do
    div_ [class_ "heroic-avatar"] $ user ^. userAvatarImg avatarDefaultSize
    h1_ [class_ "main-heading"] $ user ^. userLogin . _UserLogin . html
    ul_ [class_ "role-badges"] $ do
        forM_ (user ^. userRoleSet . to Set.toList) $ \(r :: Role) ->
            li_ [class_ "badge"] $ r ^. uilabeled

commonIdeaSpaceDelegations :: User -> User -> EQuery [Delegation]
commonIdeaSpaceDelegations delegatee delegate = do
    let delegateeId = delegatee ^. _Id
    schoolDelegation <- delegateInScope delegateeId (DScopeIdeaSpace SchoolSpace)
    classDelegations <- forM (Set.toList $ commonSchoolClasses delegatee delegate)
        (delegateInScope delegateeId . DScopeIdeaSpace . ClassSpace)
    pure $ catMaybes (schoolDelegation:classDelegations)

-- | Delegation buttons works in a different way if the user opens
-- his/her own profile, or other users profile.
--
-- For the own profile clicking on the delegation button, the delegate selection
-- page is opened.
--
-- For other's profile, clicking on the delegation buttons mark the owner of
-- the profile as the delegate of the current user.
delegationButtons :: Monad m => User -> User -> [Delegation] -> HtmlT m ()
delegationButtons delegatee delegate delegations = do
    let ownProfile = isOwnProfile delegatee delegate
        isActiveDelegation dscope =
            if ownProfile
                then any (\d -> d ^. delegationScope == dscope &&
                                d ^. delegationFrom == delegatee ^. _Id) delegations
                else Delegation dscope (delegatee ^. _Id) (delegate ^. _Id)
                     `elem`
                     delegations
        butGet path = a_ [class_ "btn-cta heroic-cta", href_ path]
        butPost = postButton_ [class_ "btn-cta heroic-cta", jsReloadOnClick]
    forM_ (commonSchoolClasses delegatee delegate) $ \clss -> do
        let classScope = DScopeIdeaSpace (ClassSpace clss)
        if isActiveDelegation classScope
            then do
                if ownProfile
                    then butGet (U.createDelegation classScope)
                           ("Deine Beauftragung für Klasse " <> uilabel clss)
                    else butPost (U.withdrawDelegationOnClassSpace delegate clss)
                           ("Beauftragung für Klasse " <> uilabel clss <> " entziehen")
            else do
                if ownProfile
                    then butGet (U.createDelegation classScope)
                           ("Deine Beauftragung für Klasse " <> uilabel clss)
                    else butPost (U.delegateVoteOnClassSpace delegate clss)
                           ("Für Klasse " <> uilabel clss <> " beauftragen")
    let schoolScope = DScopeIdeaSpace SchoolSpace
    if isActiveDelegation schoolScope
        then do
            (if ownProfile
                then butGet (U.createDelegation schoolScope)
                else butPost (U.withdrawDelegationOnSchoolSpace delegate))
                "Schulweite beauftragung entziehen"
        else do
            (if ownProfile
                then butGet (U.createDelegation schoolScope)
                else butPost (U.delegateVoteOnSchoolSpace delegate))
                "Schulweit beauftragen"

-- | All 'DScopes' in which user watching the profile has delegated to the profile owner.
delegatedDScopes :: User -> DelegationListsMap -> [DScope]
delegatedDScopes delegatee (DelegationListsMap xs) = fullDScopeToDScope . fst <$> filter pr xs
  where
    pr :: (a, DelegateeLists) -> Bool
    pr (_, DelegateeLists ys) = delegatee `elem` (fst <$> ys)


-- ** User Profile: Created Ideas

data UserProfileTab
    = UserIdeasTab
    | UserDelegateesTab
    | UserDelegatesTab
  deriving (Eq)

-- | FUTUREWORK: 'Frontend.Page.Topic.tabLink' shares some non-trivial code with this function that
-- could be factored out.
userProfileTab :: Monad m => UserProfileTab -> User -> HtmlT m ()
userProfileTab activeTab user = do
    div_ [class_ "heroic-tabs is-responsive"] $ allTabs False
    select_ [class_ "heroic-tabs-dropdown", onchange_ "window.location = this.value"] $ allTabs True
  where
    allTabs dd = do
        tabLink dd UserIdeasTab      (U.viewUserProfile user)     "Erstellte Ideen"
        tabLink dd UserDelegateesTab (U.userDelegationsTo user)   "Wer stimmt für mich ab?"
        tabLink dd UserDelegatesTab  (U.userDelegationsFrom user) "Für wen stimme ich ab?"

    tabLink True  = tabLinkDropdown
    tabLink False = tabLinkDiv

    tabLinkDropdown t path
        = option_ $ [selected_ "true" | t == activeTab] <> [value_ . absoluteUriPath . U.relPath $ path]

    tabLinkDiv t path
        | t == activeTab = span_ [class_ "heroic-tab-item m-active"]
        | otherwise      = a_    [class_ "heroic-tab-item", href_ path]

instance ToHtml PageUserProfileCreatedIdeas where
    toHtmlRaw = toHtml
    toHtml p@(PageUserProfileCreatedIdeas ctx (DeletedUser user) _ideas _delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx (Left user)
    toHtml p@(PageUserProfileCreatedIdeas ctx (ActiveUser user) ideas delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx (Right (user, delegations))
            userProfileTab UserIdeasTab user
        -- List of ideas
        div_ [class_ "m-shadow"] $ do
            div_ [class_ "grid"] $ toHtml ideas

-- | List all the created ideas for the given user.
createdIdeas :: (ActionPersist m, ActionUserHandler m)
    => AUID User -> IdeasQuery -> m PageUserProfileCreatedIdeas
createdIdeas userId ideasQuery = do
    user <- mquery $ findUser userId
    ctx  <- profileContext user
    let visibleByCurrentUser idea =
            case idea ^. ideaLocation . ideaLocationSpace of
                SchoolSpace  -> True
                ClassSpace c ->
                    case ctx ^. capCtxUser . userRoleScope of
                        SchoolScope      -> True
                        ClassesScope cls -> c `Set.member` cls
    cUser <- currentUser
    equery (do
        ideas <- ListItemIdeas ctx (IdeaInUserProfile user) ideasQuery
              <$> (applyFilter ideasQuery <$>
                    (mapM getIdeaStats
                     =<< filter visibleByCurrentUser
                         <$> findIdeasByUserId userId))
        ds <- commonIdeaSpaceDelegations cUser user
        pure $ PageUserProfileCreatedIdeas
            ctx
            (makeUserView user)
            ideas
            ds)


-- ** User Profile: User As Delegate

instance ToHtml PageUserProfileUserAsDelegate where
    toHtmlRaw = toHtml
    toHtml p@(PageUserProfileUserAsDelegate ctx (DeletedUser user) _delegationListsMap _delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx (Left user)
    toHtml p@(PageUserProfileUserAsDelegate ctx (ActiveUser user) delegationListsMap delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx (Right (user, delegations))
            userProfileTab UserDelegateesTab user
        div_ [class_ "m-shadow"] $ do
            div_ [class_ "grid"] $ do
                div_ [class_ "container-narrow"] $ do
                    renderDelegations True delegationListsMap

userProfileUserDelegation
    :: (ActionPersist m, ActionUserHandler m)
    => (CapCtx -> UserView -> DelegationListsMap -> [Delegation] -> page)
    -> (AUID User -> EQuery DelegationListsMap)
    -> AUID User -> m page
userProfileUserDelegation pageConstructor userDelegationsMap userId = do
    user <- mquery $ findUser userId
    ctx  <- profileContext user
    cUser <- currentUser
    equery $ do
        pageConstructor
            ctx
            (makeUserView user)
            <$> userDelegationsMap userId
            <*> commonIdeaSpaceDelegations cUser user

userProfileUserAsDelegate :: (ActionPersist m, ActionUserHandler m)
      => AUID User -> m PageUserProfileUserAsDelegate
userProfileUserAsDelegate =
    userProfileUserDelegation
        PageUserProfileUserAsDelegate
        userDelegationListsMap


-- ** User Profile: User as a delegatee

instance ToHtml PageUserProfileUserAsDelegatee where
    toHtmlRaw = toHtml
    toHtml p@(PageUserProfileUserAsDelegatee ctx (DeletedUser user) _delegationListsMap _delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx (Left user)
    toHtml p@(PageUserProfileUserAsDelegatee ctx (ActiveUser user) delegationListsMap delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv ctx (Right (user, delegations))
            userProfileTab UserDelegatesTab user
        div_ [class_ "m-shadow"] $ do
            div_ [class_ "grid"] $ do
                div_ [class_ "container-narrow"] $ do
                    renderDelegations True delegationListsMap

userProfileUserAsDelegatee :: (ActionPersist m, ActionUserHandler m)
      => AUID User -> m PageUserProfileUserAsDelegatee
userProfileUserAsDelegatee =
    userProfileUserDelegation
        PageUserProfileUserAsDelegatee
        userDelegateListsMap


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
        semanticDiv' [class_ "container-main container-narrow"] p $ do
            div_ [class_ "hero-unit"] $ do
                userHeaderDivCore user
                h2_ [class_ "sub-heading"] .
                    toHtml $ if isOwnProfile (ctx ^. capCtxUser) user
                        then "Eigenes Nutzerprofil bearbeiten"
                        else "Nutzerprofil von " <> user ^. userLogin . unUserLogin <> " bearbeiten"

            form $ do
                div_ $ do
                    p_ [class_ "label-text"] "Einige wichtige Hinweise zum Hochladen von Bildern."
                    div_ [class_ "info-text"] $ do
                        p_ $ do
                            "Das alte Bild wird beim hochladen überschrieben.  Ziehe dir bitte "
                            "jetzt zuerst eine Sicherheitskopie, wenn du es später noch brauchst."
                        p_ $ do
                            "Nach dem hochladen wird das neue Bild in Kreisform geschnitten. "
                            "Es sollte also nicht zu lang oder hoch sein und nichts wichtiges "
                            "in den Ecken zeigen."
                        p_ $ do
                            let dim = fromString . show . maximum $ avatarDefaultSize : avatarExtraSizes
                            "Das neue Bild sollte für optimale Qualität mindestens "
                            dim >> "x" >> dim >> " "
                            "Pixel haben."
                        p_ $ do
                            "Es darf nicht größer sein als " >> avatarMaxByteSize ^. html >> "."
                    label_ $ do
                        span_ [class_ "label-text"] "Avatar"
                        DF.inputFile "avatar" v
                        br_ nil
                        br_ nil
                    label_ $ do
                        span_ [class_ "label-text"] "Beschreibung"
                        inputTextArea_ [placeholder_ "..."] Nothing Nothing "desc" v
                    footer_ [class_ "form-footer"] $ do
                        DF.inputSubmit "Änderungen speichern"
                        cancelButton p

validateImageFile :: (Monad n, ActionM m) => Maybe FilePath -> m (DF.Result (HtmlT n ()) (Maybe DynamicImage))
validateImageFile = \case
    Nothing   -> pure $ DF.Success Nothing
    Just file -> do
        img <- readImageFile (cs file)
        pure $ case img of
            Nothing          -> DF.Success Nothing  -- FIXME: get rid of this double-'Maybe'; see
                                                    -- documentation of 'readImageFile'
            Just (Right pic) -> DF.Success (Just pic)
            Just (Left _)    -> DF.Error "Die ausgewählte Datei ist kein Bild (jpg, png, gif, ...)"
                                -- (everything that juicy-pixles can read is allowed here)


editUserProfile :: ActionM m => AUID User -> FormPageHandler m EditUserProfile
editUserProfile uid = formPageHandlerWithMsg
    (do user <- mquery (findUser uid)
        ctx  <- profileContext user
        pure $ EditUserProfile ctx user
    )
    (\up -> do
        update . SetUserDesc uid $ up ^. profileDesc
        saveAvatar uid `mapM_` (up ^. profileAvatar)
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
