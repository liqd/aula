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

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page.Admin
where

import Control.Arrow ((&&&))
import Servant

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
data AdminViewUsers = AdminViewUsers [User]
  deriving (Eq, Show, Read)

instance Page AdminViewUsers

data AdminCreateUser = AdminCreateUser
  deriving (Eq, Show, Read)

instance Page AdminCreateUser

data AdminEditUser = AdminEditUser User [SchoolClass]
  deriving (Eq, Show, Read)

instance Page AdminEditUser

data AdminDeleteUser = AdminDeleteUser User
  deriving (Eq, Show, Read)

instance Page AdminDeleteUser

data AdminViewClasses = AdminViewClasses [SchoolClass]
  deriving (Eq, Show, Read)

instance Page AdminViewClasses

data AdminCreateClass = AdminCreateClass
  deriving (Eq, Show, Read)

instance Page AdminCreateClass

data AdminEditClass = AdminEditClass SchoolClass [User]
  deriving (Eq, Show, Read)

instance Page AdminEditClass

-- | 11.4 Admin settings: Events protocol
data PageAdminSettingsEventsProtocol =
    PageAdminSettingsEventsProtocol [IdeaSpace]
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsEventsProtocol


-- * tabs

data MenuItem
    = MenuItemDurations
    | MenuItemQuorum
    | MenuItemClasses
    | MenuItemUsers
    | MenuItemClassesAndUsers
    | MenuItemEventsProtocol
  deriving (Eq, Show)

class ToMenuItem t where
    toMenuItem :: proxy t -> MenuItem

-- | 11.1 Admin settings: Durations
instance ToMenuItem PageAdminSettingsDurations where
    toMenuItem _ = MenuItemDurations

-- | 11.2 Admin settings: Quorum
instance ToMenuItem PageAdminSettingsQuorum where
    toMenuItem _ = MenuItemQuorum

-- | 11.3 Admin settings: Manage groups & permissions
instance ToMenuItem AdminViewUsers where
    toMenuItem _ = MenuItemUsers

instance ToMenuItem AdminCreateUser where
    toMenuItem _ = MenuItemUsers

instance ToMenuItem AdminEditUser where
    toMenuItem _ = MenuItemUsers

instance ToMenuItem AdminDeleteUser where
    toMenuItem _ = MenuItemUsers

instance ToMenuItem AdminViewClasses where
    toMenuItem _ = MenuItemClasses

instance ToMenuItem AdminCreateClass where
    toMenuItem _ = MenuItemClasses

instance ToMenuItem AdminEditClass where
    toMenuItem _ = MenuItemClasses

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
                if tab `elem` [MenuItemUsers, MenuItemClasses]
                    then do
                        li_ [] $ do
                            "Gruppen & Nutzer"
                            ul_ $ do
                                li_ [] $ menulink tab MenuItemUsers
                                li_ [] $ menulink tab MenuItemClasses
                    else do
                        li_ [] $ menulink tab MenuItemClassesAndUsers
                li_ [] $ menulink tab MenuItemEventsProtocol
    div_ [class_ "col-10-12 admin-body"] bdy
  where
    tab = toMenuItem [t]

data MenuLink = MenuLink ST U.AdminMode ST
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
    MenuItemUsers
        -> MenuLink "tab-groups-perms-user"  U.AdminViewUsers "Nutzer"
    MenuItemClasses
        -> MenuLink "tab-groups-perms-class" U.AdminViewClasses "Klasse"
    MenuItemClassesAndUsers
        -> MenuLink "tab-groups-perms"       U.AdminViewUsers "Gruppen & Nutzer"
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

instance ToHtml AdminViewUsers where
    toHtml = toHtmlRaw
    toHtmlRaw p@(AdminViewUsers users) =
        adminFrame p . semanticDiv p $ do
            table_ [class_ "admin-table"] $ do
                thead_ . tr_ $ do
                    th_ nil
                    th_ "Name"
                    th_ "Klasse"
                    th_ "Rolle [<>]"
                    th_ $ button_ [class_ "btn-cta", onclick_ $ U.Admin U.AdminCreateUser] "Nutzer anlegen"
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


instance ToHtml AdminCreateUser where
    toHtml = toHtmlRaw
    toHtmlRaw p =
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
                            -- FIXME inputSelect_ [class_ "m-stretch"] "role" v
                            select_ [class_ "m-stretch"] nil
                        label_ [class_ "col-6-12"] $ do
                            span_ [class_ "label-text"] "Klasse"
                            -- FIXME inputSelect_ [class_ "m-stretch"]  "class" v
                            select_ [class_ "m-stretch"] nil
                        a_ [href_ U.Broken, class_ "btn forgotten-password"] "Passwort zurücksetzen"
                        div_ [class_ "admin-buttons"] $ do
                            DF.inputSubmit "Speichern"

instance ToHtml AdminViewClasses where
    toHtml = toHtmlRaw
    toHtmlRaw p@(AdminViewClasses classes) =
        adminFrame p . semanticDiv p $ do
            table_ [class_ "admin-table"] $ do
                thead_ . tr_ $ do
                    th_ "Klasse"
                    th_ $ button_
                            [ class_ "btn-cta"
                            , onclick_ $ U.Admin U.AdminCreateClass
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

roleSelectionChoices :: IsString s => [(RoleSelection, s)]
roleSelectionChoices =
             [ (RoleStudent, "Schüler")
             , (RoleGuest, "Gast")
             ]

roleSelection :: Getter Role (Maybe RoleSelection)
roleSelection = to $ \case
    Student{}    -> Just RoleStudent
    ClassGuest{} -> Just RoleGuest
    _            -> Nothing


chooseRole :: Maybe Role -> Monad m => DF.Form (Html ()) m RoleSelection
chooseRole mr = DF.choice roleSelectionChoices (selectRole =<< mr)
  where
    selectRole = \case
        Student _    -> Just RoleStudent
        ClassGuest _ -> Just RoleGuest
        _            -> Nothing  -- FIXME: see RoleSelection

chooseClass :: [SchoolClass] -> Maybe SchoolClass -> DfForm SchoolClass
chooseClass classes = DF.choice classValues
  where
    classValues = (id &&& toHtml . view className) <$> classes

roleForm :: Maybe Role -> Maybe SchoolClass -> [SchoolClass] -> DfForm Role
roleForm mrole mclass classes =
    fromRoleSelection
        <$> ("role"  .: chooseRole mrole)
        <*> ("class" .: chooseClass classes mclass)

instance FormPage AdminEditUser where
    type FormPagePayload AdminEditUser = Role

    formAction (AdminEditUser user _classes) =
        U.Admin . U.AdminEditUser $ user ^. _Id

    redirectOf _ _ = U.Admin U.AdminViewUsers

    -- FIXME: Show the user's role and class as default in the selections.
    makeForm (AdminEditUser user classes) =
        roleForm (user ^? userRole) (user ^? userRole . roleSchoolClass) classes

    formPage v form p@(AdminEditUser user _classes) =
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
                    inputSelect_ [class_ "m-stretch"] "role" v
                label_ [class_ "col-6-12"] $ do
                    span_ [class_ "label-text"] "Klasse"
                    inputSelect_ [class_ "m-stretch"]  "class" v
                a_ [href_ U.Broken, class_ "btn forgotten-password"] "Passwort zurücksetzen"
                div_ [class_ "admin-buttons"] $ do
                    a_ [href_ . U.Admin $ U.AdminDeleteUser (user ^. _Id), class_ "btn-cta"] "Nutzer löschen"
                    DF.inputSubmit "Änderungen speichern"

instance ToHtml AdminEditClass where
    toHtml = toHtmlRaw
    toHtmlRaw p@(AdminEditClass schoolClss users) =
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


adminViewUsers :: ActionPersist m => m AdminViewUsers
adminViewUsers = AdminViewUsers <$> query getUsers

adminCreateUser :: Applicative m => m AdminCreateUser
adminCreateUser = pure AdminCreateUser

adminViewClasses :: ActionPersist m => m AdminViewClasses
adminViewClasses = AdminViewClasses <$> query getSchoolClasses

adminEditUser :: ActionM m => AUID User -> FormPageHandler m AdminEditUser
adminEditUser uid = FormPageHandler
    { _formGetPage   = equery $ AdminEditUser <$> (maybe404 =<< findUser uid) <*> getSchoolClasses
    , _formProcessor = update . SetUserRole uid
    }

fromRoleSelection :: RoleSelection -> SchoolClass -> Role
fromRoleSelection RoleStudent = Student
fromRoleSelection RoleGuest   = ClassGuest

adminEditClass :: ActionPersist m => SchoolClass -> m AdminEditClass
adminEditClass clss =
    AdminEditClass clss <$> query (getUsersInClass clss)

instance FormPage AdminDeleteUser where
    type FormPagePayload AdminDeleteUser = ()

    formAction (AdminDeleteUser user) = U.Admin $ U.AdminDeleteUser (user ^. _Id)
    redirectOf _ _ = U.Admin U.AdminViewUsers

    makeForm _ = pure ()

    formPage _v form p@(AdminDeleteUser user) =
        adminFrame p . semanticDiv p . form $ do
            div_ [class_ "container-confirm"] $ do
                h1_ "Nutzer löschen"
                p_ $ "Wollen Sie " >> toHtml (userLongName user) >> " wirklich loschen?"
                div_ [class_ "admin-buttons"] $ do
                    DF.inputSubmit "Nutzer löschen"
                    a_ [href_ . U.Admin $ U.AdminEditUser (user ^. _Id), class_ "btn-cta"] "Zurück"

adminDeleteUser :: ActionM m => AUID User -> FormPageHandler m AdminDeleteUser
adminDeleteUser uid =
    FormPageHandler
        (AdminDeleteUser <$> equery (maybe404 =<< findUser uid))
        (const $ Action.deleteUser uid)


-- ** Events protocol

data EventsProtocolFilter = EventsProtocolFilter (Maybe IdeaSpace)
  deriving (Eq, Ord, Show, Read)

instance FormPage PageAdminSettingsEventsProtocol where
    type FormPagePayload PageAdminSettingsEventsProtocol = EventsProtocolFilter
    type FormPageResult PageAdminSettingsEventsProtocol = EventsProtocolFilter

    formAction (PageAdminSettingsEventsProtocol _) = U.Admin U.AdminEvent
    redirectOf _ (EventsProtocolFilter Nothing)      = U.Admin U.AdminDlEvents
    redirectOf _ (EventsProtocolFilter (Just space)) = U.Admin (U.AdminDlEventsF space)

    makeForm (PageAdminSettingsEventsProtocol spaces) = EventsProtocolFilter <$> ("space" .: DF.choice vs Nothing)
      where
        vs :: [(Maybe IdeaSpace, Html ())]
        vs = (Nothing, "(Alle Ideenräume)") : ((Just &&& toHtml . showIdeaSpaceUI) <$> spaces)

    formPage v form p@(PageAdminSettingsEventsProtocol _) = adminFrame p . semanticDiv p . form $ do
        label_ $ do
            span_ [class_ "label-text"] "Hier konnen Sie das Event-Protokoll als CSV-Datei herunterladen"
            inputSelect_ [class_ "m-stretch"] "space" v
        div_ [class_ "download-box"] $ do
            header_ [class_ "download-box-header"] $ do
                "Event-Protokoll"
                button_ [class_ "btn-cta download-box-button", onclick_ U.Broken] "Download"
            p_ [class_ "download-box-body"] "Das Event-Protokoll enthält alle Aktivitäten der NutzerInnen auf Aula"

adminEventsProtocol :: (ActionM m) => FormPageHandler m PageAdminSettingsEventsProtocol
adminEventsProtocol = FormPageHandler (PageAdminSettingsEventsProtocol <$> query getSpaces) pure


-- * Classes Create

data BatchCreateUsersFormData = BatchCreateUsersFormData ST (Maybe FilePath)
  deriving (Eq, Show, Generic)

instance SOP.Generic BatchCreateUsersFormData

instance FormPage AdminCreateClass where
    type FormPagePayload AdminCreateClass = BatchCreateUsersFormData

    formAction _   = U.Admin U.AdminCreateClass
    redirectOf _ _ = U.Admin U.AdminViewClasses

    makeForm _ = BatchCreateUsersFormData
        <$> ("classname" .: DF.text Nothing)  -- FIXME: validate
        <*> ("file"      .: DF.file)

    formPage v form p = adminFrame p . semanticDiv p $ do
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

adminCreateClass :: forall m. (ActionTempCsvFiles m, ActionM m)
                              => FormPageHandler m AdminCreateClass
adminCreateClass = FormPageHandler (pure AdminCreateClass) q
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


instance MimeRender CSV InitialPasswordsCsv where  -- FIXME: handle null case like with 'EventLog'?
    mimeRender Proxy (InitialPasswordsCsv rows) =
        cs (intercalate "," csvUserRecordHeaders <> "\n")
        <> Csv.encode rows

csvUserRecordHeaders :: [String]
csvUserRecordHeaders = ["Vorname", "Nachname", "email", "login", "Passwort (falls initial)"]

adminInitialPasswordsCsv :: ActionM m => SchoolClass -> m (CsvHeaders InitialPasswordsCsv)
adminInitialPasswordsCsv clss =
    csvHeaders ("Passwortliste " <> showSchoolClass clss) .
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
