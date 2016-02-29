{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.User
where

import Action
import qualified Frontend.Path as P
import Frontend.Prelude

import qualified Frontend.Path as U
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

----------------------------------------------------------------------
-- page

-- | 9. User settings
data PageUserSettings = PageUserSettings User
  deriving (Eq, Show, Read)

instance Page PageUserSettings where
    isPrivatePage _ = True

-- | 8.1 User profile: Created ideas
data PageUserProfileCreatedIdeas = PageUserProfileCreatedIdeas User [Idea]
  deriving (Eq, Show, Read)

instance Page PageUserProfileCreatedIdeas where
    isPrivatePage _ = True

-- | 8.2 User profile: Delegated votes
data PageUserProfileDelegatedVotes = PageUserProfileDelegatedVotes User [Delegation]
  deriving (Eq, Show, Read)

instance Page PageUserProfileDelegatedVotes where
    isPrivatePage _ = True


----------------------------------------------------------------------
-- templates

-- * User Settings

data UserSettingData = UserSettingData
    { profileEmail    :: Maybe Email
    , profileOldPass  :: Maybe ST
    , profileNewPass1 :: Maybe ST
    , profileNewPass2 :: Maybe ST
    }
    deriving (Eq, Show)

instance FormPageView PageUserSettings where
    type FormPageResult PageUserSettings = UserSettingData

    formAction _ = relPath (U.User U.UserSettings)

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

-- FIXME: Redirect to the right place
instance RedirectOf PageUserSettings where
    redirectOf _ = relPath U.ListSpaces

userSettings :: (ActionM action) => ServerT (FormHandler PageUserSettings ST) action
userSettings = redirectFormHandler (PageUserSettings <$> currentUser) changeUser
  where
    -- FIXME: Set the password
    changeUser (UserSettingData email _oldPass _newPass1 _newPass2) = do
        modifyCurrentUser (maybe id (\ e -> userEmail .~ Just e) email)

userHeaderDiv :: (Monad m) => User -> HtmlT m ()
userHeaderDiv _user =
    div_ $ do
        "Avatar" >> br_ []
        "Username" >> br_ []
        "Klasse" >> br_ []
        "Ein kliener frier Beschreibungstext ..." >> br_ []
        button_ [value_ ""] "KLASSENWEIT BEAUFTRAGEN" >> br_ [] --FIXME
        button_ [value_ ""] "SCHULWEIT BEUFTRAGEN" >> br_ [] -- FIXME

-- * User Profile: Created Ideas

instance ToHtml PageUserProfileCreatedIdeas where
    toHtmlRaw = toHtml
    toHtml p@(PageUserProfileCreatedIdeas user ideas) = semanticDiv p $ do
        userHeaderDiv user
        -- Tab selection
        div_ $ do
             ul_ [] $ do
                li_ $ a_ [href_ (P.User P.UserDelegations)] "Erhaltene Stimmen"
                li_ $ span_ "Erstellte Ideen"
        -- Settings button
        div_ $ do
            button_ [value_ ""] "Some kind of settings on the right"
        -- List of ideas
        div_ [id_ "ideas"] . for_ ideas $ \idea ->
            ListItemIdea False Nothing idea ^. html

-- | List all the created ideas for the given user.
-- Using @join . persistent $ do ... return $ makeFrame@ will
-- go only once to the database and query everything in one transaction,
-- that ensures data consistency, as other persistent computations
-- can interleave if the compute partial results in more than
-- one round. Same applies here like 'STM' and 'IO'.
createdIdeas :: (ActionPersist m, ActionUserHandler m) => AUID User -> m (Frame PageUserProfileCreatedIdeas)
createdIdeas userId = join . persistent $ do
    -- FIXME: 404
    Just user <- findInById dbUsers userId
    ideas <- findIdeasByUserId userId
    return $ makeFrame (PageUserProfileCreatedIdeas user ideas)

-- * User Profile: Delegated Votes

instance ToHtml PageUserProfileDelegatedVotes where
    toHtmlRaw = toHtml
    toHtml p@(PageUserProfileDelegatedVotes user _delegation) = semanticDiv p $ do
        userHeaderDiv user
        div_ $ do
             ul_ [] $ do
                li_ $ span_ "Erhaltene Stimmen"
                li_ $ a_ [href_ (P.User P.UserIdeas)] "Erstellte Ideen"
        -- School / Class select buttons
        div_ $ do
            button_ [value_ ""] "Schulweit"
            button_ [value_ ""] "Klassenweit"
        div_ $ do
            p_ "FIXME: Delegated votes"

delegatedVotes :: (ActionPersist m, ActionUserHandler m) => AUID User -> m (Frame PageUserProfileDelegatedVotes)
delegatedVotes userId = join . persistent $ do
    -- FIXME: 404
    Just user <- findInById dbUsers userId
    return $ makeFrame (PageUserProfileDelegatedVotes user []) -- FIXME: Delegated votes
