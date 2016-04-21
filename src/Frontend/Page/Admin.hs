{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Werror #-}

module Frontend.Page.Admin
where

import Control.Arrow ((&&&))
import Network.HTTP.Media ((//))
import Servant

import qualified Data.Csv as Csv
import qualified Data.Text as ST
import qualified Generics.SOP as SOP
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

import Action
import EventLog
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

-- FIXME: the following names are a little ridiculous.  s/PageAdminSettingsGaPUsersView/GroupPermUserView/?9

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

data PageAdminSettingsGaPUserDelete = PageAdminSettingsGaPUserDelete User
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsGaPUserDelete

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

instance ToMenuItem PageAdminSettingsGaPUserDelete where
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

adminDurations :: ActionM m => FormPageHandler m PageAdminSettingsDurations
adminDurations =
    FormPageHandler
        (PageAdminSettingsDurations <$> query (view dbDurations))
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
            span_ [class_ "input-helper"] "% aller Schülerinnen der Schule"
        label_ [class_ "input-append"] $ do
            span_ [class_ "label-text"] "Wie hoch soll das Quorum klassenweit sein?"
            inputText_ [class_ "input-number input-appendee"] "class-quorum" v
            span_ [class_ "input-helper"] "% aller Schülerinnen der Klasse"
        DF.inputSubmit "Änderungen speichern"

adminQuorum :: ActionM m => FormPageHandler m PageAdminSettingsQuorum
adminQuorum =
    FormPageHandler
        (PageAdminSettingsQuorum <$> query (view dbQuorums))
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
                        td_ $ user ^. userLogin . unUserLogin . html
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
                    toHtml (user ^. userLogin . unUserLogin)
                label_ [class_ "col-6-12"] $ do
                    span_ [class_ "label-text"] "Nutzerrolle"
                    inputSelect_ [class_ "m-stretch"] "user-role" v
                label_ [class_ "col-6-12"] $ do
                    span_ [class_ "label-text"] "Klasse"
                    inputSelect_ [class_ "m-stretch"]  "user-class" v
                a_ [href_ U.Broken, class_ "btn forgotten-password"] "Passwort zurücksetzen"
                div_ [class_ "admin-buttons"] $ do
                    a_ [href_ . U.Admin $ U.AdminDeleteUser (user ^. _Id), class_ "btn-cta"] "Nutzer löschen"
                    DF.inputSubmit "Änderungen speichern"

instance ToHtml PageAdminSettingsGaPClassesEdit where
    toHtml = toHtmlRaw
    toHtmlRaw p@(PageAdminSettingsGaPClassesEdit schoolClss users) =
        adminFrame p . semanticDiv p $ do
            div_ . h1_ [class_ "admin-main-heading"] . toHtml $ schoolClss ^. className
            div_ $ a_ [class_ "admin-buttons", href_ . U.Admin . U.AdminDlPass $ schoolClss]
                "Passwort-Liste"
            table_ [class_ "admin-table"] $ do
                thead_ . tr_ $ do
                    th_ nil
                    th_ "Name"
                    th_ nil
                tbody_ . forM_ users $ \user -> tr_ $ do
                    td_ . span_ [class_ "img-container"] $ avatarImgFromMaybeURL (user ^. userAvatar)
                    td_ . toHtml $ user ^. userLogin . unUserLogin
                    td_ $ a_ [href_ . U.Admin . U.AdminEditUser $ user ^. _Id] "bearbeiten"


adminSettingsGaPUsersView :: ActionPersist m => m PageAdminSettingsGaPUsersView
adminSettingsGaPUsersView = PageAdminSettingsGaPUsersView <$> query getUsers

adminSettingsGaPUsersCreate :: Applicative m => m PageAdminSettingsGaPUsersCreate
adminSettingsGaPUsersCreate = pure PageAdminSettingsGaPUsersCreate

adminSettingsGaPClassesView :: ActionPersist m => m PageAdminSettingsGaPClassesView
adminSettingsGaPClassesView = PageAdminSettingsGaPClassesView <$> query getSchoolClasses

adminSettingsGaPUserEdit :: ActionM m => AUID User -> FormPageHandler m PageAdminSettingsGaPUsersEdit
adminSettingsGaPUserEdit uid = FormPageHandler editUserPage changeUser
  where
    editUserPage = equery $
        PageAdminSettingsGaPUsersEdit
        <$> (maybe404 =<< findUser uid)
        <*> getSchoolClasses

    changeUser = update . SetUserRole uid . payloadToUserRole

payloadToUserRole :: EditUserPayload -> Role
payloadToUserRole (EditUserPayload RoleStudent clss) = Student clss
payloadToUserRole (EditUserPayload RoleGuest   clss) = ClassGuest clss

adminSettingsGaPClassesEdit :: ActionPersist m => SchoolClass -> m PageAdminSettingsGaPClassesEdit
adminSettingsGaPClassesEdit clss =
    PageAdminSettingsGaPClassesEdit clss <$> query (getUsersInClass clss)

instance FormPage PageAdminSettingsGaPUserDelete where
    type FormPagePayload PageAdminSettingsGaPUserDelete = ()

    type FormPageResult PageAdminSettingsGaPUserDelete = ()

    formAction (PageAdminSettingsGaPUserDelete user) = U.Admin $ U.AdminDeleteUser (user ^. _Id)

    redirectOf _ _ = U.Admin $ U.AdminAccess PermUserView

    makeForm _ = pure ()

    formPage _v form p@(PageAdminSettingsGaPUserDelete user) =
        adminFrame p . semanticDiv p . form $ do
            p_ "Are you sure deleting this user???"
            DF.inputSubmit "Nutzer löschen"
            a_ [href_ . U.Admin $ U.AdminEditUser (user ^. _Id), class_ "btn-cta"] "Cancel"

    guardPage _ = pure Nothing

adminSettingsGaPUserDelete :: forall m. (ActionM m)
                           => AUID User -> FormPageHandler m PageAdminSettingsGaPUserDelete
adminSettingsGaPUserDelete uid =
    FormPageHandler
        (PageAdminSettingsGaPUserDelete <$> equery (maybe404 =<< findUser uid))
        (const $ Action.deleteUser uid)

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

adminEventsProtocol :: ActionPersist m => m PageAdminSettingsEventsProtocol
adminEventsProtocol = PageAdminSettingsEventsProtocol <$> query getSpaces


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

adminSettingsGaPClassesCreate :: forall m. (ActionTempCsvFiles m, ActionM m)
                              => FormPageHandler m PageAdminSettingsGaPClassesCreate
adminSettingsGaPClassesCreate = FormPageHandler (pure PageAdminSettingsGaPClassesCreate) q
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
    p _        (CsvUserRecord _ _ _ _                          (Just _)) = do
        throwError500 "upload FAILED: internal error!"
    p schoolcl (CsvUserRecord firstName lastName mEmail mLogin Nothing) = do
      void $ do
        update . AddIdeaSpaceIfNotExists $ ClassSpace schoolcl
        pwd <- mkRandomPassword
        currentUserAddDb AddUser ProtoUser
            { _protoUserLogin     = mLogin
            , _protoUserFirstName = firstName
            , _protoUserLastName  = lastName
            , _protoUserRole      = Student schoolcl
            , _protoUserPassword  = pwd
            , _protoUserEmail     = mEmail
            , _protoUserDesc      = Markdown nil
            }


-- * csv file handling

data CsvUserRecord = CsvUserRecord
    { _csvUserRecordFirst       :: UserFirstName
    , _csvUserRecordLast        :: UserLastName
    , _csvUserRecordEmail       :: Maybe EmailAddress
    , _csvUserRecordLogin       :: Maybe UserLogin
    , _csvUserRecordInitialPass :: Maybe ST
    }
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic CsvUserRecord

data InitialPasswordsCsv = InitialPasswordsCsv [CsvUserRecord]
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic InitialPasswordsCsv

type InitialPasswordsCsvH = Headers '[HeaderListEntry__] InitialPasswordsCsv
type HeaderListEntry__ = Header "Content-Disposition" String  -- appease hlint v1.9.22


-- | NOTE: If there are any passwords in the csv input file, they are silently ignored.  (This can
-- be easily changed, if we want the admins / moderators / ... to make up passwords instead.)
instance Csv.FromRecord CsvUserRecord where
    parseRecord (fmap (ST.strip . cs) . toList -> (v :: [ST])) = CsvUserRecord
        <$> (UserFirstName <$> parseName 50 0)
        <*> (UserLastName <$> parseName 50 1)
        <*> parseMEmail 2
        <*> pure (parseMLogin 3)
        <*> pure Nothing
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

instance Csv.ToRecord CsvUserRecord where
    toRecord (CsvUserRecord fn ln em li pw) = Csv.toRecord
        [ fn ^. _UserFirstName
        , ln ^. _UserLastName
        , em ^. _Just . re emailAddress
        , li ^. _Just . _UserLogin
        , pw ^. _Just
        ]


data CSV

instance Accept CSV where
    contentType Proxy = "text" // "csv"

instance MimeRender CSV InitialPasswordsCsv where
    mimeRender Proxy (InitialPasswordsCsv rows) =
        cs (intercalate "," csvUserRecordHeaders <> "\n")
        <> Csv.encode rows

instance MimeRender CSV EventLog where
    mimeRender Proxy (EventLog _ []) = "[Keine Daten]"
    mimeRender Proxy (EventLog domainUrl rows) =
        cs (intercalate "," eventLogItemCsvHeaders <> "\n")
        <> Csv.encode (URLEventLogItem domainUrl <$> rows)


csvUserRecordHeaders :: [String]
csvUserRecordHeaders = ["Vorname", "Nachname", "email", "login", "Passwort (falls initial)"]

instance MimeRender CSV InitialPasswordsCsvH where
    mimeRender proxy (Headers v _) = mimeRender proxy v


adminInitialPasswordsCsv :: ActionM m => SchoolClass -> m InitialPasswordsCsvH
adminInitialPasswordsCsv clss =
    addHeader ("attachment; filename=" <> showSchoolClass clss <> ".csv") .
    InitialPasswordsCsv . catMaybes . fmap mk <$> query (getUsersInClass clss)
  where
    mk u = case u ^. userPassword of
        UserPassInitial ps -> Just $ CsvUserRecord
              (u ^. userFirstName)
              (u ^. userLastName)
              (u ^. userEmail)
              (Just $ u ^. userLogin)
              (Just ps)
        _ -> Nothing
