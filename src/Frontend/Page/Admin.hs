{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Admin
where

import Data.Maybe (mapMaybe)

import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

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
data PageAdminSettingsGroupsAndPermissions
  = PageAdminSettingsGPUsers   [User]
  | PageAdminSettingsGPClasses [SchoolClass]
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsGroupsAndPermissions where
    isPrivatePage _ = True

-- | 11.3 Admin settings: User creation & user import
data PageAdminSettingsUserCreateAndImport =
    PageAdminSettingsUserCreateAndImport
  deriving (Eq, Show, Read)

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
  deriving (Eq, Show, Read)

data Quorums = Quorums
    { schoolQuorumPercentage :: Int
    , classQuorumPercentage  :: Int
    }
  deriving (Eq, Show, Read)

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

data Tab
    = TabDurations
    | TabQuorum
    | TabGroupsAndPermissions (Maybe PermissionContext)
    | TabEventsProtocol
  deriving (Eq, Show)

class ToTab t where
    toTab :: t -> Tab

-- | 11.1 Admin settings: Durations
instance ToTab PageAdminSettingsDurations where
    toTab _ = TabDurations

-- | 11.2 Admin settings: Quorum
instance ToTab PageAdminSettingsQuorum where
    toTab _ = TabQuorum

-- | 11.3 Admin settings: Manage groups & permissions
instance ToTab PageAdminSettingsGroupsAndPermissions where
    toTab (PageAdminSettingsGPUsers _users) = TabGroupsAndPermissions (Just PermUser)
    toTab (PageAdminSettingsGPClasses _classes) = TabGroupsAndPermissions (Just PermClass)

-- | 11.4 Admin settings: Events protocol
instance ToTab PageAdminSettingsEventsProtocol where
    toTab _ = TabEventsProtocol

----------------------------------------------------------------------
-- templates

-- * Duration

adminFrame :: (Monad m, ToTab tab) => tab -> HtmlT m () -> HtmlT m ()
adminFrame t bdy = do
    div_ [id_ "tabs"] . ul_ [] $ do
        li_ [] $ tabLink tab TabDurations
        li_ [] $ tabLink tab TabQuorum
        if isPermissionsTab tab
            then do
                li_ [] $ span_ "Gruppen & Nutzer"
                li_ [] $ tabLink tab (TabGroupsAndPermissions (Just PermUser))
                li_ [] $ tabLink tab (TabGroupsAndPermissions (Just PermClass))
            else do
                li_ [] $ tabLink tab (TabGroupsAndPermissions Nothing)
        li_ [] $ tabLink tab TabEventsProtocol
    div_ bdy
  where
    tab = toTab t
    isPermissionsTab (TabGroupsAndPermissions _) = True
    isPermissionsTab _ = False

tabLink :: Monad m => Tab -> Tab -> HtmlT m ()
tabLink curTab targetTab =
  case targetTab of
    TabDurations -> go "tab-duration"           U.AdminDuration "Dauer der Phasen"
    TabQuorum    -> go "tab-qourum"             U.AdminQuorum "Quorum"
    TabGroupsAndPermissions Nothing
                 -> go "tab-groups-perms"       (U.AdminAccess PermUser) "Gruppen & Nutzer"
    TabGroupsAndPermissions (Just PermUser)
                 -> go "tab-groups-perms-user"  (U.AdminAccess PermUser) "Nutzer"
    TabGroupsAndPermissions (Just PermClass)
                 -> go "tab-groups-perms-class" (U.AdminAccess PermClass) "Klasse"
    TabEventsProtocol
                 -> go "tab-events"             U.AdminEvent "Beauftragen Stimmen"
  where
    go ident uri =
        a_ [ id_ ident
           , href_ $ U.Admin uri
           , class_ $ tabSelected curTab targetTab
           ]

instance FormPageView PageAdminSettingsDurations where
    type FormPageResult PageAdminSettingsDurations = Durations

    -- | The form action used in form generation
    formAction _ = relPath $ U.Admin U.AdminDuration

    -- | Calculates a redirect address from the given page
    -- FIXME: Do we redirecto to the same page???
    redirectOf _ = relPath $ U.Admin U.AdminDuration

    -- | Generates a Html view from the given page
    makeForm (PageAdminSettingsDurations dur) =
        mkDurations
            ("elab-duration" .: readPeriod defaultElaborationPeriod (elaborationPhase dur))
            (("vote-duration" .: readPeriod defaultVotingPeriod (votingPhase dur)))
      where
        mkDurations e v =
            Durations <$> (DurationDays <$> e) <*> (DurationDays <$> v)
        readPeriod (DurationDays d) (DurationDays v) =
            fromMaybe d . readMaybe <$> DF.string (Just (show v))

    -- | Generates a Html snippet from the given view, form action, and the @p@ page
    -- formPage :: (Monad m) => View (HtmlT m ()) -> ST -> p -> HtmlT m ()
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

adminDurations :: (ActionM m) => ServerT (FormHandler PageAdminSettingsDurations ST) m
adminDurations = redirectFormHandler (PageAdminSettingsDurations <$> durations) saveDurations
  where
    saveDurations :: ActionM m => Durations -> m ()
    saveDurations (Durations elab vote) = persistent $ do
        modifyDb dbElaborationDuration (const elab)
        modifyDb dbVoteDuration        (const vote)

    durations :: ActionM m => m Durations
    durations = persistent $
        Durations <$> getDb dbElaborationDuration
                  <*> getDb dbVoteDuration

-- * Quorom

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

adminQuorum :: ActionM m => ServerT (FormHandler PageAdminSettingsQuorum ST) m
adminQuorum = redirectFormHandler (PageAdminSettingsQuorum <$> quorum) saveQuorum
  where
    saveQuorum (Quorums school clss) = persistent $ do
        modifyDb dbSchoolQuorum (const school)
        modifyDb dbClassQuorum (const clss)

    quorum = persistent $
        Quorums <$> getDb dbSchoolQuorum
                <*> getDb dbClassQuorum

-- * Groups and permisisons

instance ToHtml PageAdminSettingsGroupsAndPermissions where
    toHtml = toHtmlRaw

    toHtmlRaw p@(PageAdminSettingsGPUsers users) =
        adminFrame p . semanticDiv p $ do
            table_ $ do
                thead_ . tr_ $ do
                    th_ "AVATAR"
                    th_ "NAME"
                    th_ "KLASSE"
                    th_ "ROLE SELECTION"
                    th_ $ button_ [onclick_ U.ListSpaces] "NUTZER ANLEGEN"
                    th_ $ input_ [value_ "NUTZERSUCHE"]
                -- FIXME: Make the table fetch some users with AJAX
                tbody_ . forM_ users $ \user -> tr_ $ do
                    td_ $ img_ [src_ . U.TopStatic . fromString . cs $ user ^. userAvatar]
                    td_ . toHtml $ user ^. userLogin
                    td_ "Klasse ????" -- FIXME: Fetch the user's class if exists
                    td_ "Role ???" -- FIXME: Fetch the user's role
                    td_ "" -- THIS SHOULD LEFT EMPTY
                    td_ $ a_ [href_ U.ListSpaces] "bearbeiten"

    toHtmlRaw p@(PageAdminSettingsGPClasses classes) =
        adminFrame p . semanticDiv p $ do
            button_ [onclick_ U.ListSpaces] "KLASSE ANLEGEN"
            table_ $ do
                thead_ . tr_ $ do
                    th_ "KLASSE"
                    th_ $ button_ [onclick_ U.ListSpaces] "KLASSE ANLEGEN"
                    th_ $ input_ [value_ "Klassensuche"]
                tbody_ . forM_ classes $ \clss -> tr_ $ do
                    th_ . toHtml $ clss ^. className
                    th_ ""
                    th_ $ a_ [href_ U.ListSpaces] "bearbeiten"


adminSettingsGroupsAndPermissions
    :: ActionM m => PermissionContext -> m (Frame PageAdminSettingsGroupsAndPermissions)

adminSettingsGroupsAndPermissions PermUser =
    -- FIXME: Fetch limited number of users.
    makeFrame =<< PageAdminSettingsGPUsers <$> persistent getUsers

adminSettingsGroupsAndPermissions PermClass =
    -- FIXME: Store the classes in different table?
    makeFrame =<< PageAdminSettingsGPClasses . mapMaybe toClass <$> persistent getSpaces
  where
    toClass (ClassSpace clss) = Just clss
    toClass _                 = Nothing


-- * User create and import

adminSettingsUserCreateAndImport
    :: (ActionM m) => ServerT (FormHandler PageAdminSettingsUserCreateAndImport ST) m
adminSettingsUserCreateAndImport = error "adminSettingsUserCreateAndImport"

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
            button_ [onclick_ U.ListSpaces] "DOWNLOAD"
            p_ "Das Event-Protokoll beinhaltet alle Aktivieren der Nutzerlennen auf Aula"
      where
        makeValue :: IdeaSpace -> ST
        makeValue SchoolSpace = "idea-schoolspace"
        makeValue (ClassSpace (SchoolClass year name))
            = cs $ mconcat ["idea-class-", show year, "-", show name]

        makeText SchoolSpace = "Schule"
        makeText (ClassSpace (SchoolClass _year name)) = toHtml name

adminEventsProtocol :: ActionM m => m (Frame PageAdminSettingsEventsProtocol)
adminEventsProtocol = makeFrame =<< (PageAdminSettingsEventsProtocol <$> persistent getSpaces)
