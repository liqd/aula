{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Admin
where

import Control.Arrow ((&&&))
import Data.Maybe (mapMaybe)

import qualified Data.Csv as Csv
import qualified Data.Text as ST
import qualified Generics.SOP as SOP
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

import Action
import Persistent.Api
import Frontend.Prelude

import qualified Frontend.Path as U


-- * types

-- | 11.1 Admin settings: Durations
data PageAdminSettingsDurations =
    PageAdminSettingsDurations Durations
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsDurations

-- | 11.2 Admin settings: Quorum
data PageAdminSettingsQuorum =
    PageAdminSettingsQuorum Quorums
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsQuorum

-- | 11.3 Admin settings: Manage groups & permissions
data PageAdminSettingsGaPUsersView = PageAdminSettingsGaPUsersView [User]
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsGaPUsersView

data PageAdminSettingsGaPUsersCreate = PageAdminSettingsGaPUsersCreate
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsGaPUsersCreate

data PageAdminSettingsGaPUsersEdit = PageAdminSettingsGaPUsersEdit User [SchoolClass]
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsGaPUsersEdit

data PageAdminSettingsGaPClassesView = PageAdminSettingsGaPClassesView [SchoolClass]
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsGaPClassesView

data PageAdminSettingsGaPClassesCreate = PageAdminSettingsGaPClassesCreate
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsGaPClassesCreate

data PageAdminSettingsGaPClassesEdit = PageAdminSettingsGaPClassesEdit SchoolClass [User]
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsGaPClassesEdit

-- | 11.4 Admin settings: Events protocol
data PageAdminSettingsEventsProtocol =
    PageAdminSettingsEventsProtocol [IdeaSpace]
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsEventsProtocol


-- * tabs

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
    toMenuItem _ = MenuItemGroupsAndPermissions (Just PermUserView)

instance ToMenuItem PageAdminSettingsGaPUsersEdit where
    toMenuItem _ = MenuItemGroupsAndPermissions (Just PermUserView)

instance ToMenuItem PageAdminSettingsGaPClassesView where
    toMenuItem _ = MenuItemGroupsAndPermissions (Just PermClassView)

instance ToMenuItem PageAdminSettingsGaPClassesCreate where
    toMenuItem _ = MenuItemGroupsAndPermissions (Just PermClassView)

instance ToMenuItem PageAdminSettingsGaPClassesEdit where
    toMenuItem _ = MenuItemGroupsAndPermissions (Just PermClassView)

-- | 11.4 Admin settings: Events protocol
instance ToMenuItem PageAdminSettingsEventsProtocol where
    toMenuItem _ = MenuItemEventsProtocol


-- * templates

-- ** Duration

adminFrame :: (Monad m, ToMenuItem tab) => tab -> HtmlT m () -> HtmlT m ()
adminFrame t bdy = do
    div_ [class_ "col-2-12"] $ do
        nav_ [class_ "admin-menu"] $ do
            h2_ [class_ "admin-menu-header"] "Prozessverwaltung"
            ul_ [] $ do
                li_ [] $ menulink tab MenuItemDurations
                li_ [] $ menulink tab MenuItemQuorum
                if isPermissionsMenuItem tab
                    then do
                        li_ [] $ do
                            "Gruppen & Nutzer"
                            ul_ $ do
                                li_ [] $ menulink tab (MenuItemGroupsAndPermissions (Just PermUserView))
                                li_ [] $ menulink tab (MenuItemGroupsAndPermissions (Just PermClassView))
                    else do
                        li_ [] $ menulink tab (MenuItemGroupsAndPermissions Nothing)
                li_ [] $ menulink tab MenuItemEventsProtocol
    div_ [class_ "col-10-12 admin-body"] bdy
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
        -> MenuLink "tab-events"             U.AdminEvent "Protokolle"

instance FormPage PageAdminSettingsDurations where
    type FormPagePayload PageAdminSettingsDurations = Durations

    formAction _ = U.Admin U.AdminDuration

    -- FIXME: Do we redirect to the same page???
    redirectOf _ _ = U.Admin U.AdminDuration

    makeForm (PageAdminSettingsDurations dur) =
        Durations <$> ("elab-duration" .: getPeriod elaborationPhase)
                  <*> ("vote-duration" .: getPeriod votingPhase)
      where
        defaultDurations = defaultSettings ^. durations
        readPeriod (DurationDays d) (DurationDays v) =
            fromMaybe d . readMaybe <$> DF.string (Just (show v))
        getPeriod l = DurationDays <$> readPeriod (defaultDurations ^. l) (dur ^. l)

    formPage v form p = adminFrame p . semanticDiv p . form $ do
        -- FIXME these should be "number" fields
        label_ [class_ "input-append"] $ do
            span_ [class_ "label-text"] "Wie viele Tage soll die Ausarbeitungphase dauern?"
            inputText_ [class_ "input-number input-appendee"] "elab-duration" v
            span_ [class_ "input-helper"] "Tage"
        label_ [class_ "input-append"] $ do
            span_ [class_ "label-text"] "Wie viele Tage soll die Abstimmungphase dauren?"
            inputText_  [class_ "input-number input-appendee"] "vote-duration" v
            span_ [class_ "input-helper"] "Tage"
        DF.inputSubmit "Änderungen speichern"

adminDurations :: ActionM m => ServerT (FormHandler PageAdminSettingsDurations) m
adminDurations = redirectFormHandler (PageAdminSettingsDurations <$> query (view dbDurations))
                                     (update . SaveDurations)


-- ** Quorum

instance FormPage PageAdminSettingsQuorum where
    type FormPagePayload PageAdminSettingsQuorum = Quorums

    formAction _   = U.Admin U.AdminQuorum
    redirectOf _ _ = U.Admin U.AdminQuorum

    makeForm (PageAdminSettingsQuorum q) =
        Quorums
        <$> ("school-quorum" .: getPercentage schoolQuorumPercentage)
        <*> ("class-quorum"  .: getPercentage classQuorumPercentage)
      where
        readPercentage d v = fromMaybe d . readMaybe <$> DF.string (Just (show v))
        getPercentage l = readPercentage (defaultSettings ^. quorums . l) (q ^. l)

    formPage v form p = adminFrame p . semanticDiv p . form $ do
        label_ [class_ "input-append"] $ do
            span_ [class_ "label-text"] "Wie hoch soll das Quorum schulweit sein?"
            inputText_ [class_ "input-number input-appendee"] "school-quorum" v
            span_ [class_ "input-helper"] "% aller Schulerinnen der Schule"
        label_ [class_ "input-append"] $ do
            span_ [class_ "label-text"] "Wie hoch soll das Quorum klassenweit sein?"
            inputText_ [class_ "input-number input-appendee"] "class-quorum" v
            span_ [class_ "input-helper"] "% aller Schulerinnen der Klasse"
        DF.inputSubmit "Änderungen speichern"

adminQuorum :: ActionM m => ServerT (FormHandler PageAdminSettingsQuorum) m
adminQuorum = redirectFormHandler (PageAdminSettingsQuorum <$> query (view dbQuorums))
                                  (update . SaveQuorums)


-- ** roles and permisisons

instance ToHtml PageAdminSettingsGaPUsersView where
    toHtml = toHtmlRaw
    toHtmlRaw p@(PageAdminSettingsGaPUsersView users) =
        adminFrame p . semanticDiv p $ do
            table_ [class_ "admin-table"] $ do
                thead_ . tr_ $ do
                    th_ nil
                    th_ "Name"
                    th_ "Klasse"
                    th_ "Rolle [<>]"
                    th_ $ button_ [class_ "btn-cta", onclick_ . U.Admin . U.AdminAccess $ PermUserCreate] "Nutzer anlegen"
                    th_ $ do
                        div_ [class_ "inline-search-container"] $ do
                            input_ [type_ "text", class_ "inline-search-input", value_ "Nutzersuche"] -- FIXME Placeholder not value
                            a_ [href_ U.Broken, class_ "inline-search-button"] $ i_ [class_ "icon-search"] nil -- FIXME dummy

                let renderUserRow :: forall m. (Monad m) => User -> HtmlT m ()
                    renderUserRow user = tr_ $ do
                        td_ . span_ [class_ "img-container"] $ avatarImgFromMaybeURL (user ^. userAvatar)
                        td_ $ user ^. userLogin . fromUserLogin . html
                        td_ (case user ^. userRole of
                                Student cl    -> toHtml $ showSchoolClass cl
                                ClassGuest cl -> toHtml $ showSchoolClass cl
                                _             -> nil)
                        td_ $ roleLabel (user ^. userRole)
                        td_ (toHtmlRaw nbsp)
                        td_ $ a_ [href_ . U.Admin . U.AdminEditUser $ user ^. _Id] "bearbeiten"

                tbody_ $ renderUserRow `mapM_` users


instance ToHtml PageAdminSettingsGaPUsersCreate where
    toHtml = toHtmlRaw
    toHtmlRaw p@PageAdminSettingsGaPUsersCreate =
        adminFrame p . semanticDiv p $ do
            div_ [class_ "admin-container"] $ do
                form_ $ do -- FIXME
                    div_ [class_ "col-3-12"] $ do
                        div_ [class_ "upload-avatar"] $ do
                            a_ [href_ U.Broken] $ do
                                i_ [class_ "upload-avatar-icon icon-camera"] nil
                    div_ [class_ "col-9-12"] $ do
                        h1_ [class_ "admin-main-heading"] $ do
                            "UserName" -- FIXME
                        label_ [class_ "col-6-12"] $ do
                            span_ [class_ "label-text"] "Nutzerrolle"
                            -- FIXME inputSelect_ [class_ "m-stretch"] "user-role" v
                            select_ [class_ "m-stretch"] nil
                        label_ [class_ "col-6-12"] $ do
                            span_ [class_ "label-text"] "Klasse"
                            -- FIXME inputSelect_ [class_ "m-stretch"]  "user-class" v
                            select_ [class_ "m-stretch"] nil
                        a_ [href_ U.Broken, class_ "btn forgotten-password"] "Passwort zurücksetzen"
                        div_ [class_ "admin-buttons"] $ do
                            DF.inputSubmit "Speichern"

instance ToHtml PageAdminSettingsGaPClassesView where
    toHtml = toHtmlRaw
    toHtmlRaw p@(PageAdminSettingsGaPClassesView classes) =
        adminFrame p . semanticDiv p $ do
            table_ [class_ "admin-table"] $ do
                thead_ . tr_ $ do
                    th_ "Klasse"
                    th_ $ button_
                            [ class_ "btn-cta"
                            , onclick_ . U.Admin $ U.AdminAccess PermClassCreate
                            ]
                            "Klasse anlegen"
                    th_ $ do
                        div_ [class_ "inline-search-container"] $ do
                            input_ [type_ "text", class_ "inline-search-input", value_ "Klassensuche"] -- FIXME Placeholder not value
                            a_ [href_ U.Broken, class_ "inline-search-button"] $ i_ [class_ "icon-search"] nil -- FIXME dummy
                tbody_ . forM_ classes $ \clss -> tr_ $ do
                    td_ . toHtml $ clss ^. className
                    td_ (toHtmlRaw nbsp)
                    td_ $ a_ [href_ . U.Admin $ U.AdminEditClass clss] "bearbeiten"

-- | FIXME: re-visit application logic.  we should really be able to change everybody into every
-- role, and the class field should be hidden / displayed as appropriate.  see issue #197.
data RoleSelection
    = RoleStudent
    | RoleGuest
  deriving (Eq, Generic, Show)

instance SOP.Generic RoleSelection

data EditUserPayload = EditUserPayload
    { editUserRole  :: RoleSelection
    , editUserClass :: SchoolClass
    }
  deriving (Eq, Generic, Show)

instance SOP.Generic EditUserPayload

roleSelectionChoices :: IsString s => [(RoleSelection, s)]
roleSelectionChoices =
             [ (RoleStudent, "Schüler")
             , (RoleGuest, "Gast")
             ]

instance FormPage PageAdminSettingsGaPUsersEdit where
    type FormPagePayload PageAdminSettingsGaPUsersEdit = EditUserPayload

    formAction (PageAdminSettingsGaPUsersEdit user _classes) =
        U.Admin . U.AdminEditUser $ user ^. _Id

    redirectOf (PageAdminSettingsGaPUsersEdit _user _classes) _ =
        U.Admin . U.AdminAccess $ PermUserView

    -- FIXME: Show the user's role and class as default in the selections.
    makeForm (PageAdminSettingsGaPUsersEdit user classes) =
        EditUserPayload
            <$> ("user-role"  .: DF.choice roleSelectionChoices role)
            <*> ("user-class" .: DF.choice classValues clval)
      where
        classValues = (id &&& toHtml . view className) <$> classes

        role = case user ^. userRole of
            Student _    -> Just RoleStudent
            ClassGuest _ -> Just RoleGuest
            _            -> Nothing  -- FIXME: see RoleSelection

        clval = case user ^. userRole of
            Student cl    -> Just cl
            ClassGuest cl -> Just cl
            _             -> Nothing  -- FIXME: see RoleSelection

    formPage v form p@(PageAdminSettingsGaPUsersEdit user _classes) =
        adminFrame p . semanticDiv p . div_ [class_ "admin-container"] . form $ do
            div_ [class_ "col-3-12"] $ do
                div_ [class_ "upload-avatar"] $ do
                    a_ [href_ U.Broken] $ do
                        i_ [class_ "upload-avatar-icon icon-camera"] nil
                        avatarImgFromHasMeta user
            div_ [class_ "col-9-12"] $ do
                h1_ [class_ "admin-main-heading"] $ do
                    toHtml (user ^. userLogin . fromUserLogin)
                label_ [class_ "col-6-12"] $ do
                    span_ [class_ "label-text"] "Nutzerrolle"
                    inputSelect_ [class_ "m-stretch"] "user-role" v
                label_ [class_ "col-6-12"] $ do
                    span_ [class_ "label-text"] "Klasse"
                    inputSelect_ [class_ "m-stretch"]  "user-class" v
                a_ [href_ U.Broken, class_ "btn forgotten-password"] "Passwort zurücksetzen"
                div_ [class_ "admin-buttons"] $ do
                    a_ [href_ U.Broken, class_ "btn-cta"] "Nutzer löschen"
                    DF.inputSubmit "Änderungen speichern"

instance ToHtml PageAdminSettingsGaPClassesEdit where
    toHtml = toHtmlRaw
    toHtmlRaw p@(PageAdminSettingsGaPClassesEdit clss users) =
        adminFrame p . semanticDiv p $ do
            -- FIXME: Make appropiate design
            div_ . h1_ . toHtml $ clss ^. className
            table_ [class_ "admin-table"] $ do
                thead_ . tr_ $ do
                    th_ nil
                    th_ "Name"
                    th_ nil
                tbody_ . forM_ users $ \user -> tr_ $ do
                    td_ . span_ [class_ "img-container"] $ img_ [src_ U.Broken]
                    td_ . toHtml $ user ^. userLogin . fromUserLogin
                    td_ $ a_ [href_ . U.Admin . U.AdminEditUser $ user ^. _Id] "bearbeiten"

-- FIXME: Fetch limited number of users ("pagination").

adminSettingsGaPUsersView :: ActionM m => m (Frame PageAdminSettingsGaPUsersView)
adminSettingsGaPUsersView =
    makeFrame =<< PageAdminSettingsGaPUsersView <$> query getUsers

adminSettingsGaPUsersCreate :: ActionM m => m (Frame PageAdminSettingsGaPUsersCreate)
adminSettingsGaPUsersCreate =
    makeFrame PageAdminSettingsGaPUsersCreate

adminSettingsGaPClassesView :: ActionM m => m (Frame PageAdminSettingsGaPClassesView)
adminSettingsGaPClassesView =
    makeFrame =<< PageAdminSettingsGaPClassesView <$> query getSchoolClasses

adminSettingsGaPUserEdit :: ActionM m => AUID User -> ServerT (FormHandler PageAdminSettingsGaPUsersEdit) m
adminSettingsGaPUserEdit uid = redirectFormHandler editUserPage editUser
  where
    editUserPage = query $
        PageAdminSettingsGaPUsersEdit
        <$> ((\(Just u) -> u) <$> findUser uid) -- FIXME: Error handling (404?)
        <*> getSchoolClasses

    editUser = update . SetUserRole uid . payloadToUserRole

payloadToUserRole :: EditUserPayload -> Role
payloadToUserRole (EditUserPayload RoleStudent clss) = Student clss
payloadToUserRole (EditUserPayload RoleGuest   clss) = ClassGuest clss

isClassInRole :: SchoolClass -> Role -> Bool
isClassInRole clss (Student clss')    = clss == clss'
isClassInRole clss (ClassGuest clss') = clss == clss'
isClassInRole _    _                  = False

getSchoolClasses :: Query [SchoolClass]
getSchoolClasses = mapMaybe toClass <$> getSpaces
  where
    toClass (ClassSpace clss) = Just clss
    toClass _                 = Nothing

adminSettingsGaPClassesEdit :: ActionM m => SchoolClass -> m (Frame PageAdminSettingsGaPClassesEdit)
adminSettingsGaPClassesEdit clss =
    makeFrame =<< PageAdminSettingsGaPClassesEdit clss <$> usersInClass
  where
    -- FIXME: the following two lines should be happening in "Persistent.Api".
    usersInClass = filter isUserInClass <$> query getUsers
    isUserInClass = isClassInRole clss . view userRole


-- ** Events protocol

instance ToHtml PageAdminSettingsEventsProtocol where
    toHtml = toHtmlRaw
    toHtmlRaw p@(PageAdminSettingsEventsProtocol ideaSpaces) = adminFrame p . semanticDiv p $ do
        label_ $ do
            span_ [class_ "label-text"] "Hier konnen Sie das Event-Protokoll als CSV-Datei herunterladen"
        -- FIXME: Clientside JavaScript. Change the download button link
        -- If the value of the selection is changes.
            select_ [name_ "idea"] . forM_ ideaSpaces $ \idea ->
                option_ [value_ (makeValue idea)] (makeText idea)
        div_ [class_ "download-box"] $ do
            header_ [class_ "download-box-header"] $ do
                "Event-Protokoll"
            -- FIXME: Link to the correct page.
                button_ [class_ "btn-cta download-box-button", onclick_ U.Broken] "Download"
            p_ [class_ "download-box-body"] "Das Event-Protokoll beinhaltet alle Aktivieren der Nutzerlennen auf Aula"
      where
        makeValue :: IdeaSpace -> ST
        makeValue SchoolSpace = "idea-schoolspace"
        makeValue (ClassSpace (SchoolClass year name))
            = cs $ mconcat ["idea-class-", show year, "-", show name]

        makeText SchoolSpace = "Schule"
        makeText (ClassSpace (SchoolClass _year name)) = toHtml name

adminEventsProtocol :: ActionM m => m (Frame PageAdminSettingsEventsProtocol)
adminEventsProtocol = makeFrame =<< (PageAdminSettingsEventsProtocol <$> query getSpaces)


-- * Classes Create

data BatchCreateUsersFormData = BatchCreateUsersFormData ST (Maybe FilePath)
  deriving (Eq, Show, Generic)

instance SOP.Generic BatchCreateUsersFormData

instance FormPage PageAdminSettingsGaPClassesCreate where
    type FormPagePayload PageAdminSettingsGaPClassesCreate = BatchCreateUsersFormData

    formAction PageAdminSettingsGaPClassesCreate =
        U.Admin $ U.AdminAccess PermClassCreate

    redirectOf PageAdminSettingsGaPClassesCreate _ =
        U.Admin $ U.AdminAccess PermClassView

    makeForm PageAdminSettingsGaPClassesCreate = BatchCreateUsersFormData
        <$> ("classname" DF..: DF.text Nothing)  -- FIXME: validate
        <*> ("file"      DF..: DF.file)

    formPage v form p@PageAdminSettingsGaPClassesCreate = adminFrame p . semanticDiv p $ do
        h3_ "Klasse anlegen"
        a_ [href_ $ U.TopStatic "templates/student_upload.csv"] "Vorlage herunterladen."
        form $ do
            div_ $ do
                p_ "Klasse"
                DF.inputText "classname" v
            div_ $ do
                p_ "CSV-Datei"
                DF.inputFile "file" v
            DF.inputSubmit "upload!"

theOnlySchoolYearHack :: Int
theOnlySchoolYearHack = 2016

data CsvUserRecord = CsvUserRecord
    { _csvUserRecordFirst       :: UserFirstName
    , _csvUserRecordLast        :: UserLastName
    , _csvUserRecordEmail       :: Maybe EmailAddress
    , _csvUserRecordLogin       :: Maybe UserLogin
    }
  deriving (Eq, Show)

instance Csv.FromRecord CsvUserRecord where
    parseRecord (fmap (ST.strip . cs) . toList -> (v :: [ST])) = CsvUserRecord
        <$> (UserFirstName <$> parseName 50 0)
        <*> (UserLastName <$> parseName 50 1)
        <*> parseMEmail 2
        <*> pure (parseMLogin 3)
      where
        parseName :: (Monad m) => Int -> Int -> m ST
        parseName maxLength i
            | length v < i + 1
                = fail $ "user record too short: " <> show v
            | ST.length (v !! i) > maxLength
                = fail $ "user record with overly long column " <> show i <> ": " <> show v
            | otherwise
                = pure $ v !! i

        parseMEmail :: (Monad m) => Int -> m (Maybe EmailAddress)
        parseMEmail i
            | length v < i + 1 = pure Nothing
            | v !! i == ""     = pure Nothing
            | otherwise        = case v ^? ix i . emailAddress of
                Nothing    -> fail $ "user record with bad email address: " <> show v
                Just email -> pure . Just $ email

        parseMLogin :: Int -> Maybe UserLogin
        parseMLogin i
            | length v < i + 1 = Nothing
            | v !! i == ""     = Nothing
            | otherwise        = Just . UserLogin $ v !! i

adminSettingsGaPClassesCreate :: forall m. (ActionTempCsvFiles m, ActionM m)
                              => ServerT (FormHandler PageAdminSettingsGaPClassesCreate) m
adminSettingsGaPClassesCreate = redirectFormHandler (pure PageAdminSettingsGaPClassesCreate) q
  where
    q :: BatchCreateUsersFormData -> m ()
    q (BatchCreateUsersFormData _clname Nothing) =
        throwError500 "upload FAILED: no file!"  -- FIXME: status code?
    q (BatchCreateUsersFormData clname (Just file)) = do
        let schoolcl = SchoolClass theOnlySchoolYearHack clname
        eCsv :: Either String [CsvUserRecord] <- popTempCsvFile file
        case eCsv of
            Left msg      -> throwError500 $ "csv parsing FAILED: " <> cs msg
                                             -- FIXME: status code?
            Right records -> mapM_ (p schoolcl) records

    p :: SchoolClass -> CsvUserRecord -> m ()
    p schoolcl (CsvUserRecord firstName lastName mEmail mLogin) = do
      void $ do
        update . AddIdeaSpaceIfNotExists $ ClassSpace schoolcl
        pwd <- mkRandomPassword
        currentUserAddDb (AddUser pwd) ProtoUser
            { _protoUserLogin     = mLogin
            , _protoUserFirstName = firstName
            , _protoUserLastName  = lastName
            , _protoUserRole      = Student schoolcl
            , _protoUserPassword  = Nothing
            , _protoUserEmail     = mEmail
            }
