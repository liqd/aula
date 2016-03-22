{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.User
where

import Action
import qualified Frontend.Path as P
import Frontend.Prelude

import qualified Frontend.Path as U
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF


-- * page

-- | 9. User settings
data PageUserSettings = PageUserSettings User
  deriving (Eq, Show, Read)

instance Page PageUserSettings

-- | 8.1 User profile: Created ideas
data PageUserProfileCreatedIdeas = PageUserProfileCreatedIdeas User [(Idea, Int)]
  deriving (Eq, Show, Read)

instance Page PageUserProfileCreatedIdeas

-- | 8.2 User profile: Delegated votes
data PageUserProfileDelegatedVotes = PageUserProfileDelegatedVotes User [Delegation]
  deriving (Eq, Show, Read)

instance Page PageUserProfileDelegatedVotes


-- * templates

-- ** User Settings

data UserSettingData = UserSettingData
    { profileEmail    :: Maybe UserEmail
    , profileOldPass  :: Maybe ST
    , profileNewPass1 :: Maybe ST
    , profileNewPass2 :: Maybe ST
    }
    deriving (Eq, Show)

instance FormPage PageUserSettings where
    type FormPageResult PageUserSettings = UserSettingData

    formAction _ = relPath U.UserSettings
    redirectOf _ = relPath U.ListSpaces  -- FIXME: Redirect to the right place

    makeForm (PageUserSettings user) =
        UserSettingData
        <$> ("email"         .: (fmap UserEmail <$> DF.optionalText
                                        (fmap fromUserEmail $ user ^. userEmail)))
        <*> ("old-password"  .: DF.optionalText Nothing)
        <*> ("new-password1" .: DF.optionalText Nothing)
        <*> ("new-password2" .: DF.optionalText Nothing)

    formPage v fa p = do
        semanticDiv p $ do
            DF.form v fa $ do
                h1_ "Einstellungen"
                p_ "E-mailadresse (optional)"
                DF.inputText "email" v >> br_ []
                h3_ "Passwort andern"
                p_ "aktualles Passwort"
                DF.inputText "old-password" v >> br_ []
                p_ "neues Passwort"
                DF.inputText "new-password1" v >> br_ []
                p_ "neues Passwort bestatigen"
                DF.inputText "new-password2" v >> br_ []
                DF.inputSubmit "ANDERUNGEN SPEICHERN"


userSettings :: (ActionM r action) => ServerT (FormHandler PageUserSettings) action
userSettings = redirectFormHandler (PageUserSettings <$> currentUser) changeUser
  where
    -- FIXME: Set the password
    changeUser (UserSettingData email _oldPass _newPass1 _newPass2) = do
        modifyCurrentUser (maybe id (\ e -> userEmail .~ Just e) email)

userHeaderDiv :: (Monad m) => User -> HtmlT m ()
userHeaderDiv _user =
    div_ $ do
        div_ [class_ "heroic-avatar"] "Avatar"
        h1_ [class_ "main-heading"] "Username"
        span_ [class_ "post-title"] "Klasse"
        p_ [class_ "sub-header"] "Ein kleiner freier Beschreibungstext ..."  -- FIXME: get this from 'User' value!
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
    toHtml p@(PageUserProfileCreatedIdeas user ideas) = semanticDiv p $ do
        div_ [class_ "hero-unit"] $ do
            userHeaderDiv user
            -- Tab selection
            div_ [class_ "heroic-tabs"] $ do
                span_ [class_ "heroic-tab-item m-active"]
                    "Erstellte Ideen"
                a_ [class_ "heroic-tab-item", href_ (P.User (user ^. _Id) P.UserDelegations)]
                    "Erhaltene Stimmen"
        -- List of ideas
        div_ [class_ "m-shadow"] $ do
            div_ [class_ "grid"] $ do
                -- Settings button
                div_ [class_ "ideas-list"] $ do
                    div_ [class_ "btn-settings pop-menu"] $ do
                        i_ [class_ "icon-sort", title_ "Settings"] nil
                        ul_ [class_ "pop-menu-list"] $ do
                            li_ [class_ "pop-menu-list-item"] $ do
                                a_ [href_ U.Broken] "popularity"
                            li_ [class_ "pop-menu-list-item"] $ do
                                a_ [href_ U.Broken] "date"
                    for_ ideas $ \(idea, numVoters) ->
                        ListItemIdea False Nothing numVoters idea ^. html

-- | List all the created ideas for the given user.
-- Using @join . persistent $ do ... return $ makeFrame@ will
-- go only once to the database and query everything in one transaction,
-- that ensures data consistency, as other persistent computations
-- can interleave if the compute partial results in more than
-- one round. Same applies here like 'STM' and 'IO'.
createdIdeas :: (ActionPersist r m, ActionUserHandler m, MonadError ActionExcept m)
    => AUID User -> m (Frame PageUserProfileCreatedIdeas)
createdIdeas userId = join . persistent $ do
    -- FIXME: 404
    Just user <- findInById dbUsers userId
    ideasAndNumVoters <- findIdeasByUserId userId >>= mapM getNumVotersForIdea
    return . makeFrame $ PageUserProfileCreatedIdeas user ideasAndNumVoters

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
                div_ [class_ "small-avatar-list-image"] nil -- FIXME Make a real image a child here
            div_ [class_ "col-11-12"] $ do
                h3_ "UserName"
                p_ $ do
                    "5 Stimmen von "
                    strong_ $ do
                        a_ [href_ U.Broken] "UserName, "
                        a_ [href_ U.Broken] "UserName, "
                        a_ [href_ U.Broken] "UserName"

delegatedVotes :: (ActionPersist r m, ActionUserHandler m, MonadError ActionExcept m)
    => AUID User -> m (Frame PageUserProfileDelegatedVotes)
delegatedVotes userId = join . persistent $ do
    -- FIXME: 404
    Just user <- findInById dbUsers userId
    return $ makeFrame (PageUserProfileDelegatedVotes user []) -- FIXME: Delegated votes
