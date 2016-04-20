{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.User
where

import Action
import Frontend.Fragment.IdeaList
import Frontend.Prelude
import Persistent.Api
import qualified Frontend.Path as P

import qualified Frontend.Path as U
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF


-- * page

-- | 9. User settings
data PageUserSettings = PageUserSettings User
  deriving (Eq, Show, Read)

instance Page PageUserSettings

-- | 8.1 User profile: Created ideas
data PageUserProfileCreatedIdeas = PageUserProfileCreatedIdeas RenderContext User ListItemIdeas
  deriving (Eq, Show, Read)

instance Page PageUserProfileCreatedIdeas

-- | 8.2 User profile: Delegated votes
data PageUserProfileDelegatedVotes = PageUserProfileDelegatedVotes User [Delegation]
  deriving (Eq, Show, Read)

instance Page PageUserProfileDelegatedVotes

-- | 8.X User profile: Editing the public profile
data EditUserProfile = EditUserProfile User
  deriving (Eq, Show, Read)

instance Page EditUserProfile


-- * templates

-- ** User Settings

data UserSettingData = UserSettingData
    { profileEmail    :: Maybe EmailAddress
    , profileOldPass  :: Maybe ST
    , profileNewPass1 :: Maybe ST
    , profileNewPass2 :: Maybe ST
    }
    deriving (Eq, Show)

instance FormPage PageUserSettings where
    type FormPagePayload PageUserSettings = UserSettingData

    formAction _ = U.UserSettings

    -- Redirect to ourselves, so the user can review the changes.  FUTUREWORK: It would be nice to
    -- have a messaging device that prints a line "your changes have been saved" at the top of the
    -- form; without that, UX is still a bit confusing.
    redirectOf _ _ = U.UserSettings

    makeForm (PageUserSettings user) =
        UserSettingData
        <$> ("email"         .: emailField)
        <*> ("old-password"  .: DF.optionalText Nothing)
        <*> ("new-password1" .: DF.optionalText Nothing)
        <*> ("new-password2" .: DF.optionalText Nothing)

      where
        email = user ^. userEmail
        emailField =
            {-  Since not all texts values are valid email addresses, emailAddress is a @Prism@
                from texts to @EmailAddress@. Here we want to traverse the text of an email address
                thus one needs to reverse this prisms. While Prisms cannot be reversed in full
                generality we could expect a weaker form which also traversals, this would look
                like that:

                email & rev emailAddress %%~ DF.optionalText

                Instead we have the code below which extracts the text of the email address if
                there is such an email address, optionalText gets a @Maybe ST@, finally the
                result of optionalText is processed with a pure function from @Maybe ST@ to
                @Maybe EmailAddress@ where only a valid text representation of an email gets
                mapped to @Just@  of an @EmailAddress@.
            -}
            (>>= preview emailAddress) <$> DF.optionalText (email ^? _Just . re emailAddress)

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
                        h2_ [class_ "label-header"] "Passwort andern"
                        label_ $ do
                            span_ [class_ "label-text"] "aktualles Passwort"
                            inputPassword_ [class_ "m-small"]
                                "old-password" v
                        label_ $ do
                            span_ [class_ "label-text"] "neues Passwort"
                            inputPassword_ [class_ "m-small"]
                                "new-password1" v
                        label_ $ do
                            span_ [class_ "label-text"] "neues Passwort bestatigen"
                            inputPassword_ [class_ "m-small"]
                                "new-password2" v
                        footer_ [class_ "form-footer"] $ do
                            DF.inputSubmit "Änderungen speichern"


userSettings :: forall m . ActionM m => FormPageHandler m PageUserSettings
userSettings = FormPageHandler (PageUserSettings <$> currentUser) changeUser
  where
    changeUser :: UserSettingData -> m ()
    changeUser (UserSettingData memail oldPass newPass1 newPass2) = do
        uid <- currentUserId
        maybe (pure ()) (update . SetUserEmail uid) memail
        update $ SetUserPass uid oldPass newPass1 newPass2
        pure ()

userHeaderDiv :: (Monad m) => User -> HtmlT m ()
userHeaderDiv user =
    div_ $ do
        div_ [class_ "heroic-avatar"] $ user ^. userAvatar . to avatarImgFromMaybeURL
        h1_ [class_ "main-heading"] $ user ^. userLogin . _UserLogin . html
        span_ [class_ "post-title"] $ user ^. userRole . roleSchoolClass . to showSchoolClass . html
        p_ [class_ "sub-header"] $ user ^. userDesc . html
        div_ [class_ "heroic-btn-group"] $ do
            button_ [class_ "heroic-cta btn-cta", value_ ""] $ do
                i_ [class_ "icon-bullhorn"] nil
                "Klassenweit beauftragen" --FIXME
            button_ [class_ "heroic-cta btn-cta", value_ ""] $ do
                i_ [class_ "icon-bullhorn"] nil
                "Schulweit beauftragen" -- FIXME


-- ** User Profile: Created Ideas

instance ToHtml PageUserProfileCreatedIdeas where
    toHtmlRaw = toHtml
    toHtml p@(PageUserProfileCreatedIdeas ctx user ideas) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            when (ctx ^. renderContextUser . _Id == user ^. _Id) $ do
                nav_ [class_ "pop-menu m-dots detail-header-menu"] $ do
                    ul_ [class_ "pop-menu-list"] $ do
                        li_ [class_ "pop-menu-list-item"] $ do
                            a_ [href_ U.UserProfile] $ do
                                i_ [class_ "icon-pencil"] nil
                                "bearbeiten"
            userHeaderDiv user
            -- Tab selection
            div_ [class_ "heroic-tabs"] $ do
                span_ [class_ "heroic-tab-item m-active"]
                    "Erstellte Ideen"
                a_ [class_ "heroic-tab-item", href_ (P.User (user ^. _Id) P.UserDelegations)]
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
        user  <- maybe404 =<< findUser userId
        ideas <- ListItemIdeas ctx IdeaInUserProfile
                    (IdeaLocationSpace SchoolSpace) (Nothing, Nothing)

                    -- TODO:
                    -- * there are no queries for this page.  but we probably want that.
                    -- * idea location is used to calculate the urls for the filter and sort links.
                    --   for users, we need something slightly different.  shouldn't be too hard to
                    --   refactor that, though.

              <$> (findIdeasByUserId userId >>= mapM getListInfoForIdea)
        pure $ PageUserProfileCreatedIdeas ctx user ideas)


-- ** User Profile: Delegated Votes

instance ToHtml PageUserProfileDelegatedVotes where
    toHtmlRaw = toHtml
    toHtml p@(PageUserProfileDelegatedVotes user delegations) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv user
            div_ [class_ "heroic-tabs"] $ do
                a_ [class_ "heroic-tab-item", href_ (P.User (user ^. _Id) P.UserIdeas)]
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

delegatedVotes :: ActionPersist m => AUID User -> m PageUserProfileDelegatedVotes
delegatedVotes userId = do
    let dv = []  -- FIXME
    user :: User <- mquery $ findUser userId
    pure $ PageUserProfileDelegatedVotes user dv


-- ** User Profile: Edit profile

instance FormPage EditUserProfile where
    type FormPagePayload EditUserProfile = EditUserData

    formAction EditUserProfile{} = U.UserProfile

    redirectOf (EditUserProfile u) _ = U.viewUser u

    makeForm (EditUserProfile user) =
        EditUserData
        <$> ("firstname" .: field userFirstName _UserFirstName)
        <*> ("lastname"  .: field userLastName  _UserLastName)
        <*> ("desc"      .: field userDesc      _Markdown)

      where
        -- FIXME: use me elsewhere
        field :: Monad m => Getter User a -> Traversal' a ST -> DF.Form (Html ()) m a
        field l p = user ^. l & p %%~ DF.text . Just

    formPage v form p = do
        semanticDiv p $ do
            div_ [class_ "container-main popup-page"] $ do
                div_ [class_ "container-narrow"] $ do
                    h1_ [class_ "main-heading"] "User profile" -- FIXME english
                    form $ do
                        label_ $ do
                            span_ [class_ "label-text"] "Firstname" -- FIXME english
                            inputText_ [class_ "m-small"] "firstname" v
                        label_ $ do
                            span_ [class_ "label-text"] "Lastname" -- FIXME english
                            inputText_ [class_ "m-small"] "lastname" v
                        label_ $ do
                            span_ [class_ "label-text"] "Desc" -- FIXME english
                            inputTextArea_ [placeholder_ "..."] Nothing Nothing "desc" v
                        footer_ [class_ "form-footer"] $ do
                            DF.inputSubmit "Änderungen speichern"

editUserProfile :: ActionM m => FormPageHandler m EditUserProfile
editUserProfile = FormPageHandler
    { _formGetPage   = EditUserProfile <$> currentUser
    , _formProcessor = \up -> update . (`EditUser` up) =<< currentUserId }
