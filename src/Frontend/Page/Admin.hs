{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Admin
where

import Control.Arrow ((&&&))
import Data.Maybe (mapMaybe)

import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF
import qualified Generics.SOP as SOP

import Action (ActionM, ActionPersist(..))
import Frontend.Prelude

import qualified Frontend.Path as U


----------------------------------------------------------------------
-- types

-- | 11.1 Admin settings: Durations
data PageAdminSettingsDurations =
    PageAdminSettingsDurations Durations
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsDurations where
    isPrivatePage _ = True

-- | 11.2 Admin settings: Quorum
data PageAdminSettingsQuorum =
    PageAdminSettingsQuorum Quorums
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsQuorum where
    isPrivatePage _ = True

-- | 11.3 Admin settings: Manage groups & permissions
data PageAdminSettingsGaPUsersView = PageAdminSettingsGaPUsersView [User]
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsGaPUsersView where
    isPrivatePage _ = True

data PageAdminSettingsGaPUsersCreate = PageAdminSettingsGaPUsersCreate
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsGaPUsersCreate where
    isPrivatePage _ = True

data PageAdminSettingsGaPUsersEdit = PageAdminSettingsGaPUsersEdit User [SchoolClass]
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsGaPUsersEdit where
    isPrivatePage _ = True

data PageAdminSettingsGaPClassesView = PageAdminSettingsGaPClassesView [SchoolClass]
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsGaPClassesView where
    isPrivatePage _ = True

data PageAdminSettingsGaPClassesCreate = PageAdminSettingsGaPClassesCreate
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsGaPClassesCreate where
    isPrivatePage _ = True

-- | 11.4 Admin settings: Events protocol
data PageAdminSettingsEventsProtocol =
    PageAdminSettingsEventsProtocol [IdeaSpace]
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsEventsProtocol where
    isPrivatePage _ = True

-- | Elaboration and Voting phase durations
data Durations = Durations
    { elaborationPhase :: DurationDays
    , votingPhase      :: DurationDays
    }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic Durations

data Quorums = Quorums
    { schoolQuorumPercentage :: Int
    , classQuorumPercentage  :: Int
    }
  deriving (Eq, Show, Read, Generic)

instance SOP.Generic Quorums


----------------------------------------------------------------------
-- constants

defaultElaborationPeriod, defaultVotingPeriod :: DurationDays
defaultElaborationPeriod = 21
defaultVotingPeriod = 21

defaultSchoolQuorum, defaultClassQuorum :: Int
defaultSchoolQuorum = 30
defaultClassQuorum = 3


----------------------------------------------------------------------
-- tabs

data MenuItem
    = MenuItemDurations
    | MenuItemQuorum
    | MenuItemGroupsAndPermissions (Maybe PermissionContext)
    | MenuItemEventsProtocol
  deriving (Eq, Show)

class ToMenuItem t where
    toMenuItem :: t -> MenuItem

-- | 11.1 Admin settings: Durations
instance ToMenuItem PageAdminSettingsDurations where
    toMenuItem _ = MenuItemDurations

-- | 11.2 Admin settings: Quorum
instance ToMenuItem PageAdminSettingsQuorum where
    toMenuItem _ = MenuItemQuorum

-- | 11.3 Admin settings: Manage groups & permissions
instance ToMenuItem PageAdminSettingsGaPUsersView where
    toMenuItem _ = MenuItemGroupsAndPermissions (Just PermUserView)

instance ToMenuItem PageAdminSettingsGaPUsersCreate where
    toMenuItem _ = MenuItemGroupsAndPermissions (Just PermUserCreate)

instance ToMenuItem PageAdminSettingsGaPUsersEdit where
    toMenuItem _ = MenuItemGroupsAndPermissions (Just PermUserView)

instance ToMenuItem PageAdminSettingsGaPClassesView where
    toMenuItem _ = MenuItemGroupsAndPermissions (Just PermClassView)

instance ToMenuItem PageAdminSettingsGaPClassesCreate where
    toMenuItem _ = MenuItemGroupsAndPermissions (Just PermClassCreate)

-- | 11.4 Admin settings: Events protocol
instance ToMenuItem PageAdminSettingsEventsProtocol where
    toMenuItem _ = MenuItemEventsProtocol


----------------------------------------------------------------------
-- templates

-- * Duration

adminFrame :: (Monad m, ToMenuItem tab) => tab -> HtmlT m () -> HtmlT m ()
adminFrame t bdy = do
    div_ [id_ "tabs"] . ul_ [] $ do
        li_ [] $ menulink tab MenuItemDurations
        li_ [] $ menulink tab MenuItemQuorum
        if isPermissionsMenuItem tab
            then do
                li_ [] $ span_ "Gruppen & Nutzer"
                li_ [] $ menulink tab (MenuItemGroupsAndPermissions (Just PermUserView))
                li_ [] $ menulink tab (MenuItemGroupsAndPermissions (Just PermClassView))
            else do
                li_ [] $ menulink tab (MenuItemGroupsAndPermissions Nothing)
        li_ [] $ menulink tab MenuItemEventsProtocol
    div_ bdy
  where
    tab = toMenuItem t
    isPermissionsMenuItem (MenuItemGroupsAndPermissions _) = True
    isPermissionsMenuItem _ = False

data MenuLink = MenuLink ST U.AdminPs ST
  deriving (Show)

menulink :: Monad m => MenuItem -> MenuItem -> HtmlT m ()
menulink curMenuItem targetMenuItem = case menulink' targetMenuItem of
    MenuLink ident uri body ->
        a_ [ id_ ident
           , href_ $ U.Admin uri
           , class_ $ tabSelected curMenuItem targetMenuItem
           ]
          $ toHtml body

menulink' :: MenuItem -> MenuLink
menulink' targetMenuItem =
  case targetMenuItem of
    MenuItemDurations
        -> MenuLink "tab-duration" U.AdminDuration "Dauer der Phasen"
    MenuItemQuorum
        -> MenuLink "tab-qourum" U.AdminQuorum "Quorum"
    MenuItemGroupsAndPermissions (Just PermUserView)
        -> MenuLink "tab-groups-perms-user"  (U.AdminAccess PermUserView) "Nutzer"
    MenuItemGroupsAndPermissions (Just PermUserCreate)
        -> MenuLink "tab-groups-perms-user"  (U.AdminAccess PermUserView) "Nutzer"
    MenuItemGroupsAndPermissions (Just PermClassView)
        -> MenuLink "tab-groups-perms-class" (U.AdminAccess PermClassView) "Klasse"
    MenuItemGroupsAndPermissions (Just PermClassCreate)
        -> MenuLink "tab-groups-perms-class" (U.AdminAccess PermClassView) "Klasse"
    MenuItemGroupsAndPermissions Nothing
        -> MenuLink "tab-groups-perms"       (U.AdminAccess PermUserView) "Gruppen & Nutzer"
    MenuItemEventsProtocol
        -> MenuLink "tab-events"             U.AdminEvent "Beauftragen Stimmen"

instance FormPageView PageAdminSettingsDurations where
    type FormPageResult PageAdminSettingsDurations = Durations

    formAction _ = relPath $ U.Admin U.AdminDuration

    -- FIXME: Do we redirect to the same page???
    redirectOf _ = relPath $ U.Admin U.AdminDuration

    makeForm (PageAdminSettingsDurations dur) =
        mkDurations
            ("elab-duration" .: readPeriod defaultElaborationPeriod (elaborationPhase dur))
            ("vote-duration" .: readPeriod defaultVotingPeriod (votingPhase dur))
      where
        mkDurations e v =
            Durations <$> (DurationDays <$> e) <*> (DurationDays <$> v)
        readPeriod (DurationDays d) (DurationDays v) =
            fromMaybe d . readMaybe <$> DF.string (Just (show v))

    formPage v fa p = adminFrame p $ do
        semanticDiv p $ do
            DF.form v fa $ do
                div_ $ do
                    "Wie viele Tage soll die Ausarbeitungphase dauern?" >> br_ []
                    DF.inputText "elab-duration" v >> "Tage" >> br_ []
                div_ $ do
                    "Wie viele Tage soll die Abstimmungphase dauren?" >> br_ []
                    DF.inputText "vote-duration" v >> "Tage" >> br_ []
                DF.inputSubmit "AENDERUNGEN SPIECHERN"

adminDurations :: ActionM r m => ServerT (FormHandler PageAdminSettingsDurations ST) m
adminDurations = redirectFormHandler (PageAdminSettingsDurations <$> durations) saveDurations
  where
    saveDurations :: ActionM r m => Durations -> m ()
    saveDurations (Durations elab vote) = persistent $ do
        modifyDb dbElaborationDuration (const elab)
        modifyDb dbVoteDuration        (const vote)

    durations :: ActionM r m => m Durations
    durations = persistent $
        Durations <$> getDb dbElaborationDuration
                  <*> getDb dbVoteDuration

-- * Quorum

instance FormPageView PageAdminSettingsQuorum where
    type FormPageResult PageAdminSettingsQuorum = Quorums

    formAction _ = relPath $ U.Admin U.AdminQuorum
    redirectOf _ = relPath $ U.Admin U.AdminQuorum

    makeForm (PageAdminSettingsQuorum q) =
        Quorums
        <$> ("school-quorum" .: readPercentage defaultSchoolQuorum (schoolQuorumPercentage q))
        <*> ("class-quorum"  .: readPercentage defaultClassQuorum (classQuorumPercentage q))
      where
        readPercentage d v = fromMaybe d . readMaybe <$> DF.string (Just (show v))

    formPage v fa p = adminFrame p $ do
        semanticDiv p $ do
            DF.form v fa $ do
                div_ $ do
                    "Wie hoch soll das Quorum schulweit sein?" >> br_ []
                    DF.inputText "school-quorum" v >> "% aller Schulerinnen der Schule" >> br_ []
                div_ $ do
                    "Wie hoch soll das Quorum klassenweit sein?" >> br_ []
                    DF.inputText "class-quorum" v >> "% aller Schulerinnen der Klasse" >> br_ []
                DF.inputSubmit "AENDERUNGEN SPIECHERN"

adminQuorum :: ActionM r m => ServerT (FormHandler PageAdminSettingsQuorum ST) m
adminQuorum = redirectFormHandler (PageAdminSettingsQuorum <$> quorum) saveQuorum
  where
    saveQuorum (Quorums school clss) = persistent $ do
        modifyDb dbSchoolQuorum (const school)
        modifyDb dbClassQuorum (const clss)

    quorum = persistent $
        Quorums <$> getDb dbSchoolQuorum
                <*> getDb dbClassQuorum


-- * Groups and permisisons

instance ToHtml PageAdminSettingsGaPUsersView where
    toHtml = toHtmlRaw
    toHtmlRaw p@(PageAdminSettingsGaPUsersView users) =
        adminFrame p . semanticDiv p $ do
            table_ $ do
                thead_ . tr_ $ do
                    th_ "AVATAR"
                    th_ "NAME"
                    th_ "KLASSE"
                    th_ "ROLE SELECTION"
                    th_ $ button_ [onclick_ . U.Admin . U.AdminAccess $ PermUserCreate] "NUTZER ANLEGEN"
                    th_ $ input_ [value_ "NUTZERSUCHE"]
                tbody_ . forM_ users $ \user -> tr_ $ do
                    td_ $ img_ [src_ . U.TopStatic . fromString . cs $ user ^. userAvatar]
                    td_ . toHtml $ user ^. userLogin . fromUserLogin
                    td_ "Klasse ????" -- FIXME: Fetch the user's class if exists
                    td_ "Role ???" -- FIXME: Fetch the user's role
                    td_ "" -- THIS SHOULD LEFT EMPTY
                    td_ $ a_ [href_ . U.Admin . U.AdminEditUser $ user ^. _Id] "bearbeiten"


instance ToHtml PageAdminSettingsGaPUsersCreate where
    toHtml = toHtmlRaw
    toHtmlRaw p@PageAdminSettingsGaPUsersCreate =
        adminFrame p . semanticDiv p $ do
            toHtml (show p)

instance ToHtml PageAdminSettingsGaPClassesView where
    toHtml = toHtmlRaw
    toHtmlRaw p@(PageAdminSettingsGaPClassesView classes) =
        adminFrame p . semanticDiv p $ do
            table_ $ do
                thead_ . tr_ $ do
                    th_ "KLASSE"
                    th_ $ button_ [onclick_ U.Broken] "KLASSE ANLEGEN"
                    th_ $ input_ [value_ "Klassensuche"]
                tbody_ . forM_ classes $ \clss -> tr_ $ do
                    th_ . toHtml $ clss ^. className
                    th_ ""
                    th_ $ a_ [href_ U.Broken] "bearbeiten"


instance ToHtml PageAdminSettingsGaPClassesCreate where
    toHtml = toHtmlRaw
    toHtmlRaw p@PageAdminSettingsGaPClassesCreate =
        adminFrame p . semanticDiv p $ do
            toHtml (show p)

data Role
    = RoleStudent
    | RoleGuest
  deriving (Eq, Generic, Show)

instance SOP.Generic Role

data EditUser = EditUser
    { editUserRole  :: Role
    , editUserClass :: SchoolClass
    }
  deriving (Eq, Generic, Show)

instance SOP.Generic EditUser

roleValues :: IsString s => [(Role, s)]
roleValues = [ (RoleStudent, "Sculer")
             , (RoleGuest, "Gueste :)")
             ]

instance FormPageView PageAdminSettingsGaPUsersEdit where
    type FormPageResult PageAdminSettingsGaPUsersEdit = EditUser

    formAction (PageAdminSettingsGaPUsersEdit user _classes) =
        relPath . U.Admin . U.AdminEditUser $ user ^. _Id

    redirectOf (PageAdminSettingsGaPUsersEdit _user _classes) =
        relPath . U.Admin . U.AdminAccess $ PermUserView

    -- FIXME: Show the user's role and class as default in the selections.
    makeForm (PageAdminSettingsGaPUsersEdit _user classes) =
        EditUser
        <$> ("user-role"  .: DF.choice roleValues  Nothing)
        <*> ("user-class" .: DF.choice classValues Nothing)
      where
        classValues = (id &&& toHtml . view className) <$> classes

    formPage v fa p@(PageAdminSettingsGaPUsersEdit user _classes) = adminFrame p $ do
        semanticDiv p $ do
            DF.form v fa $ do
                div_ $ do
                    p_ "Avatar"
                    toHtml (user ^. userLogin . fromUserLogin) >> br_ []
                div_ $ do
                    "Nutzerrolle"
                    DF.inputSelect   "user-role" v >> br_ []
                    "Klasse"
                    DF.inputSelect   "user-class" v >> br_ []
                div_ $ do
                    button_ [onclick_ U.Broken, class_ "btn-cta"] "Passwort Zurucksetzen" >> br_ []
                    button_ [onclick_ U.Broken, class_ "btn-cta"] "Nutzer loschen" >> br_ []
                DF.inputSubmit "AENDERUNGEN SPIECHERN"

-- FIXME: Fetch limited number of users ("pagination").

adminSettingsGaPUsersView :: ActionM r m => m (Frame PageAdminSettingsGaPUsersView)
adminSettingsGaPUsersView =
    makeFrame =<< PageAdminSettingsGaPUsersView <$> persistent getUsers

adminSettingsGaPUsersCreate :: ActionM r m => m (Frame PageAdminSettingsGaPUsersCreate)
adminSettingsGaPUsersCreate =
    makeFrame PageAdminSettingsGaPUsersCreate

adminSettingsGaPClassesView :: ActionM r m => m (Frame PageAdminSettingsGaPClassesView)
adminSettingsGaPClassesView =
    makeFrame =<< PageAdminSettingsGaPClassesView <$> persistent getSchoolClasses

adminSettingsGaPClassesCreate :: ActionM r m => m (Frame PageAdminSettingsGaPClassesCreate)
adminSettingsGaPClassesCreate =
    makeFrame PageAdminSettingsGaPClassesCreate

adminSettingsGaPUserEdit :: ActionM r m => AUID User -> ServerT (FormHandler PageAdminSettingsGaPUsersEdit ST) m
adminSettingsGaPUserEdit uid = redirectFormHandler editUserPage editUser
  where
    editUserPage = persistent $
        PageAdminSettingsGaPUsersEdit
        <$> ((\(Just u) -> u) <$> findUser uid) -- FIXME: Error handling
        <*> getSchoolClasses

    editUser (EditUser role clss) = do
        let isForClass (Student clss')    = clss == clss'
            isForClass (ClassGuest clss') = clss == clss'
            isForClass _                  = False

        let addGroup gs = case role of
                RoleStudent -> Student clss : gs
                RoleGuest   -> ClassGuest clss : gs

        persistent $ modifyUser uid (userGroups %~ addGroup . filter (not . isForClass))

getSchoolClasses :: PersistM m => m [SchoolClass]
getSchoolClasses = mapMaybe toClass <$> getSpaces
  where
    toClass (ClassSpace clss) = Just clss
    toClass _                 = Nothing

-- * Events protocol

instance ToHtml PageAdminSettingsEventsProtocol where
    toHtml = toHtmlRaw
    toHtmlRaw p@(PageAdminSettingsEventsProtocol ideaSpaces) = adminFrame p . semanticDiv p $ do
        div_ $ do
            p_ "Hier konnen Sie das Event-Protokoll als CSV-Datei herunterladen"
            -- FIXME: Clientside JavaScript. Change the download button link
            -- If the value of the selection is changes.
            select_ [name_ "idea"] . forM_ ideaSpaces $ \idea ->
                option_ [value_ (makeValue idea)] (makeText idea)
        div_ $ do
            p_ "Event-Protokoll"
            -- FIXME: Link to the correct page.
            button_ [onclick_ U.Broken] "DOWNLOAD"
            p_ "Das Event-Protokoll beinhaltet alle Aktivieren der Nutzerlennen auf Aula"
      where
        makeValue :: IdeaSpace -> ST
        makeValue SchoolSpace = "idea-schoolspace"
        makeValue (ClassSpace (SchoolClass year name))
            = cs $ mconcat ["idea-class-", show year, "-", show name]

        makeText SchoolSpace = "Schule"
        makeText (ClassSpace (SchoolClass _year name)) = toHtml name

adminEventsProtocol :: ActionM r m => m (Frame PageAdminSettingsEventsProtocol)
adminEventsProtocol = makeFrame =<< (PageAdminSettingsEventsProtocol <$> persistent getSpaces)
