{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
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
import qualified Text.Digestive.Types as DF

import Action
import Persistent.Api
import Frontend.Prelude
import Frontend.Validation hiding (tab, spaces)

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

-- | Admin settings: Freeze
data PageAdminSettingsFreeze =
    PageAdminSettingsFreeze Freeze
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsFreeze

-- | 11.3 Admin settings: Manage groups & permissions
data AdminViewUsers = AdminViewUsers UsersQuery [UserView]
  deriving (Eq, Show, Read)

instance Page AdminViewUsers

data AdminCreateUser = AdminCreateUser [SchoolClass]
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

data AdminEditClass = AdminEditClass SchoolClass [UserView]
  deriving (Eq, Show, Read)

instance Page AdminEditClass

data AdminPhaseChange = AdminPhaseChange
  deriving (Eq, Show, Read)

instance Page AdminPhaseChange

-- | 11.4 Admin settings: Events protocol
data PageAdminSettingsEventsProtocol =
    PageAdminSettingsEventsProtocol [IdeaSpace]
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsEventsProtocol

data CreateUserPayload = CreateUserPayload
    { _createUserFirstName :: UserFirstName
    , _createUserLastName  :: UserLastName
    , _createUserLogin     :: Maybe UserLogin
    , _createUserEmail     :: Maybe EmailAddress
    , _createUserRole      :: Role
    }
  deriving (Eq, Generic, Show)

instance SOP.Generic CreateUserPayload

makeLenses ''CreateUserPayload


-- * tabs

data MenuItem
    = MenuItemDurations
    | MenuItemQuorum
    | MenuItemFreeze
    | MenuItemClasses
    | MenuItemUsers
    | MenuItemClassesAndUsers
    | MenuItemEventsProtocol
    | MenuItemPhaseChange
  deriving (Eq, Show)

class ToMenuItem t where
    toMenuItem :: proxy t -> MenuItem

-- | 11.1 Admin settings: Durations
instance ToMenuItem PageAdminSettingsDurations where
    toMenuItem _ = MenuItemDurations

-- | 11.2 Admin settings: Quorum
instance ToMenuItem PageAdminSettingsQuorum where
    toMenuItem _ = MenuItemQuorum

-- | Admin settings: Freeze
instance ToMenuItem PageAdminSettingsFreeze where
    toMenuItem _ = MenuItemFreeze

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

instance ToMenuItem AdminPhaseChange where
    toMenuItem _ = MenuItemPhaseChange

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
                li_ [] $ menulink tab MenuItemFreeze
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
                li_ [] $ menulink tab MenuItemPhaseChange
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
    MenuItemFreeze
        -> MenuLink "tab-freeze" U.AdminFreeze "Ferienmodus"
    MenuItemUsers
        -> MenuLink "tab-groups-perms-user"  U.adminViewUsers "Nutzer"
    MenuItemClasses
        -> MenuLink "tab-groups-perms-class" U.AdminViewClasses "Klasse"
    MenuItemClassesAndUsers
        -> MenuLink "tab-groups-perms"       U.adminViewUsers "Gruppen & Nutzer"
    MenuItemEventsProtocol
        -> MenuLink "tab-events"             U.AdminEvent "Protokolle"
    MenuItemPhaseChange
        -> MenuLink "tab-phase-change" U.AdminChangePhase "Phasen verschieben"

instance FormPage PageAdminSettingsDurations where
    type FormPagePayload PageAdminSettingsDurations = Durations

    formAction _ = U.Admin U.AdminDuration
    redirectOf _ _ = U.Admin U.AdminDuration

    makeForm (PageAdminSettingsDurations dur) =
        Durations <$> ("elab-duration" .: period (pNam PhaseRefinement) elaborationPhase)
                  <*> ("vote-duration" .: period (pNam PhaseVoting)     votingPhase)
      where
        period name getter = validate
            name
            (DurationDays <$> inRange 1 366)
            (DF.string (Just (show . unDurationDays $ dur ^. getter)))
        pNam ph = uilabel $ ph (error "PageAdminSettingsDurations: impossible")

    formPage v form p = adminFrame p . semanticDiv p . form $ do
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
    formPageHandlerWithMsg
        (PageAdminSettingsDurations <$> query (view dbDurations))
        (update . SaveDurations)
        "Alle Änderungen wurden gespeichert."


-- ** Quorum

instance FormPage PageAdminSettingsQuorum where
    type FormPagePayload PageAdminSettingsQuorum = Quorums

    formAction _   = U.Admin U.AdminQuorum
    redirectOf _ _ = U.Admin U.AdminQuorum

    makeForm (PageAdminSettingsQuorum q) =
        Quorums
        <$> ("school-quorum" .: percentage "Schulquorum"    schoolQuorumPercentage)
        <*> ("class-quorum"  .: percentage "Klassenquorum"  classQuorumPercentage)
      where
        percentage name getter = validate
            name
            (inRange 1 100)
            (DF.string (Just (show (q ^. getter))))

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
    formPageHandlerWithMsg
        (PageAdminSettingsQuorum <$> query (view dbQuorums))
        (update . SaveQuorums)
        "Die neuen Werte wurden gespeichert."


-- ** Freeze

instance FormPage PageAdminSettingsFreeze where
    type FormPagePayload PageAdminSettingsFreeze = Freeze

    formAction _   = U.Admin U.AdminFreeze
    redirectOf _ _ = U.Admin U.AdminFreeze

    makeForm (PageAdminSettingsFreeze current) =
        "freeze" .: DF.choice ((id &&& showOption) <$> [minBound..]) (Just current)
      where
        showOption NotFrozen = "Normalbetrieb (aufgetaut)"
        showOption Frozen    = "Ferienbetrieb (eingefroren)"

    formPage v form p = adminFrame p . semanticDiv p . form $ do
        p_ "Im Ferienbetrieb sind die folgenden Änderungen zu beachten:"
        ul_ $ do
            li_ "Die Zeit bis zum Ablauf von Ausarbeitungsphase und Abstimmungsphase wird angehalten."
            li_ "In der wilde-Ideen-Phase kann nicht mehr gewählt werden."
            li_ "In der Abstimmugnsphase kann nicht mehr gewählt werden."
            li_ "Es kann nicht mehr auf Kommentare abgestimmt werden."

        label_ [class_ "input-append"] $ do
            span_ [class_ "label-text"] "Aktueller Status"
            DF.inputSelect "freeze" v
        DF.inputSubmit "Status setzen!"

adminFreeze :: ActionM m => FormPageHandler m PageAdminSettingsFreeze
adminFreeze =
    formPageHandlerCalcMsg
        (PageAdminSettingsFreeze <$> query (view dbFreeze))
        (\payload -> do
             now <- getCurrentTimestamp
             update $ SaveAndEnactFreeze now payload)
        (let msgFrozen   :: ST = "Das System wurde re-aktiviert (Normalbetrieb)."
             msgUnfrozen :: ST = "Das System wurde eingefroren (Ferienbetrieb)."
         in const $ const . freezeElim msgFrozen msgUnfrozen)


-- ** roles and permisisons

instance ToHtml AdminViewUsers where
    toHtml = toHtmlRaw
    toHtmlRaw p@(AdminViewUsers filters users) =
        adminFrame p . semanticDiv p $ do
            div_ [class_ "clearfix"] $ do
                div_ [class_ "btn-settings pop-menu"] $ do
                    i_ [class_ "icon-sort", title_ "Sortieren nach"] nil
                    ul_ [class_ "pop-menu-list"] $ do
                        sequence_
                            [ li_ [class_ "pop-menu-list-item"] $
                                a_ [href_ . U.Admin . U.AdminViewUsers . Just $
                                        filters & usersQueryS .~ by]
                                    (uilabel by)
                            | by <- [minBound..] ]
            table_ [class_ "admin-table"] $ do
                thead_ . tr_ $ do
                    th_ nil
                    th_ "Name"
                    th_ "Klasse"
                    th_ "Rolle"
                    th_ $ button_ [class_ "btn-cta", onclick_ $ U.Admin U.AdminCreateUser] "Nutzer anlegen"
                    th_ $ do
                        div_ [class_ "inline-search-container"] $ do
                            input_ [type_ "text", class_ "inline-search-input", value_ "Nutzersuche"] -- FIXME Placeholder not value
                            a_ [href_ U.Broken, class_ "inline-search-button"] $ i_ [class_ "icon-search"] nil -- FIXME dummy

                let renderUserInfoRow :: forall m. (Monad m) => User -> HtmlT m ()
                    renderUserInfoRow user = do
                        td_ $ user ^. userLogin . unUserLogin . html
                        td_ $ user ^. userRole . roleSchoolClass . to showSchoolClass . html
                        td_ $ user ^. userRole . uilabeled
                        td_ $ toHtmlRaw nbsp

                let renderUserRow :: forall m. (Monad m) => UserView -> HtmlT m ()
                    renderUserRow (DeletedUser user) = tr_ $ do
                        td_ nil
                        renderUserInfoRow user
                        td_ "[gelöscht]"

                    renderUserRow (ActiveUser user) = tr_ $ do
                        td_ . span_ [class_ "img-container"] $ avatarImgFromMaybeURL (user ^. userAvatar)
                        renderUserInfoRow user
                        td_ $ a_ [href_ . U.Admin . U.AdminEditUser $ user ^. _Id] "bearbeiten"

                tbody_ $ renderUserRow `mapM_` applyFilter filters users

instance FormPage AdminCreateUser where
    type FormPagePayload AdminCreateUser = CreateUserPayload

    formAction _   = U.Admin U.AdminCreateUser
    redirectOf _ _ = U.Admin U.adminViewUsers

    -- FIXME: Show the user's role and class as default in the selections.
    makeForm (AdminCreateUser classes) =
        CreateUserPayload
            <$> ("firstname"  .: firstName (DF.string Nothing))
            <*> ("lastname"   .: lastName  (DF.string Nothing))
            <*> ("login"      .: loginName (DF.optionalString Nothing))
            <*> emailField "Email" Nothing
            <*> roleForm Nothing Nothing classes
        where
            -- FIXME: Users with more than one name?
            firstName = validate "Vorname"  (UserFirstName . cs <$> many1 letter <??> "nur Buchstaben")
            lastName  = validate "Nachname" (UserLastName  . cs <$> many1 letter <??> "nur Buchstaben")
            loginName = validateOptional "Login" (UserLogin <$> username)

    formPage v form p =
        adminFrame p . semanticDiv p . div_ [class_ "admin-container"] . form $ do
            div_ [class_ "col-9-12"] $ do
                h1_ [class_ "admin-main-heading"] $ do
                    label_ [class_ "input-append"] $ do
                        span_ [class_ "label-text col-6-12"] "Vorname:"
                        inputText_ [class_ "m-small col-6-12"] "firstname" v
                    label_ [class_ "input-append"] $ do
                        span_ [class_ "label-text col-6-12"] "Nachname:"
                        inputText_ [class_ "m-small col-6-12"] "lastname" v
                    label_ [class_ "input-append"] $ do
                        span_ [class_ "label-text col-6-12"] "Login:"
                        inputText_ [class_ "m-small col-6-12"] "login" v
                    label_ [class_ "col-6-12"] $ do
                        span_ [class_ "label-text"] "Nutzerrolle"
                        inputSelect_ [class_ "m-stretch"] "role" v
                    label_ [class_ "col-6-12"] $ do
                        span_ [class_ "label-text"] "Klasse"  -- FIXME: see FIXME in AdminEditUser below.
                        inputSelect_ [class_ "m-stretch"] "class" v
                div_ [class_ "admin-buttons"] $ do
                    DF.inputSubmit "Änderungen speichern"

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
                    td_ $ clss ^. className . html
                    td_ $ toHtmlRaw nbsp
                    td_ $ a_ [href_ . U.Admin $ U.AdminEditClass clss] "bearbeiten"

-- | FIXME: re-visit application logic.  we should really be able to change everybody into every
-- role, and the class field should be hidden / displayed as appropriate.  see issue #197.
data RoleSelection
    = RoleSelStudent
    | RoleSelClassGuest
    | RoleSelSchoolGuest
    | RoleSelModerator
    | RoleSelPrincipal
    | RoleSelAdmin
  deriving (Eq, Generic, Enum, Bounded, Show)

instance SOP.Generic RoleSelection

instance HasUILabel RoleSelection where
    uilabel = \case
        RoleSelStudent     -> uilabel $ Student (SchoolClass 0 nil)
        RoleSelClassGuest  -> uilabel $ ClassGuest (SchoolClass 0 nil)
        RoleSelSchoolGuest -> uilabel SchoolGuest
        RoleSelModerator   -> uilabel Moderator
        RoleSelPrincipal   -> uilabel Principal
        RoleSelAdmin       -> uilabel Admin

roleSelectionChoices :: IsString s => [(RoleSelection, s)]
roleSelectionChoices = (id &&& uilabel) <$> [minBound..]

roleSelection :: Getter Role RoleSelection
roleSelection = to $ \case
    Student{}    -> RoleSelStudent
    ClassGuest{} -> RoleSelClassGuest
    SchoolGuest  -> RoleSelSchoolGuest
    Moderator    -> RoleSelModerator
    Principal    -> RoleSelPrincipal
    Admin        -> RoleSelAdmin

chooseRole :: Maybe Role -> Monad m => DF.Form (Html ()) m RoleSelection
chooseRole mr = DF.choice roleSelectionChoices (mr ^? _Just . roleSelection)

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
    type FormPagePayload AdminEditUser = (Maybe UserLogin, Role)

    formAction (AdminEditUser user _classes) =
        U.Admin . U.AdminEditUser $ user ^. _Id

    redirectOf _ _ = U.Admin U.adminViewUsers

    makeForm (AdminEditUser user classes) =
        (,) <$> ("login" .: validateUserLogin)
            <*> roleForm (user ^? userRole) (user ^? userRole . roleSchoolClass) classes
      where
        validateUserLogin :: ActionM m => DF.Form (Html ()) m (Maybe UserLogin)
        validateUserLogin = DF.validateM go $ dfTextField user userLogin _UserLogin

        go :: forall m. ActionM m => UserLogin -> m (DF.Result (Html ()) (Maybe UserLogin))
        go "" = pure $ DF.Error "login darf nicht leer sein"
        go lgin = if lgin == user ^. userLogin
            then pure (DF.Success Nothing)
            else do
                yes <- query $ loginIsAvailable lgin
                if yes then pure . DF.Success $ Just lgin
                       else pure . DF.Error   $ "login ist bereits vergeben"


    formPage v form p@(AdminEditUser user _classes) =
        adminFrame p . semanticDiv p . div_ [class_ "admin-container"] . form $ do
            div_ [class_ "col-9-12"] $ do
                h1_ [class_ "admin-main-heading"] $ do
                    span_ [class_ "label-text"] "Login"
                    inputText_ [class_ "m-stretch"] "login" v
                label_ [class_ "col-6-12"] $ do
                    span_ [class_ "label-text"] "Nutzerrolle"
                    inputSelect_ [class_ "m-stretch"] "role" v
                label_ [class_ "col-6-12"] $ do  -- FIXME: we need a js hook that checks the value
                                                 -- of the role field, and if that's not one of the
                                                 -- first two, the "school class" field here should
                                                 -- be hidden.  (nothing needs to be done to the
                                                 -- form logic, this is a pure UI task.)
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
            div_ . h1_ [class_ "admin-main-heading"] $ schoolClss ^. className . html
            div_ $ a_ [class_ "admin-buttons", href_ . U.Admin . U.AdminDlPass $ schoolClss]
                "Passwort-Liste"
            table_ [class_ "admin-table"] $ do
                thead_ . tr_ $ do
                    th_ nil
                    th_ "Name"
                    th_ nil
                tbody_ . forM_ (activeUsers users) $ \user -> tr_ $ do
                    td_ . span_ [class_ "img-container"] $ avatarImgFromMaybeURL (user ^. userAvatar)
                    td_ $ user ^. userLogin . unUserLogin . html
                    td_ $ a_ [href_ . U.Admin . U.AdminEditUser $ user ^. _Id] "bearbeiten"


adminViewUsers :: ActionPersist m => Maybe SortUsersBy -> m AdminViewUsers
adminViewUsers qs = AdminViewUsers (mkUsersQuery qs) <$> query getUserViews

adminCreateUser :: (ActionPersist m, ActionUserHandler m, ActionRandomPassword m,
                    ActionCurrentTimestamp m) => FormPageHandler m AdminCreateUser
adminCreateUser = formPageHandlerCalcMsg
    (AdminCreateUser <$> query getSchoolClasses)
    (\up -> do
        forM_ (up ^? createUserRole . roleSchoolClass) $
            update . AddIdeaSpaceIfNotExists . ClassSpace
        pwd <- mkRandomPassword
        currentUserAddDb_ AddUser ProtoUser
            { _protoUserLogin     = up ^. createUserLogin
            , _protoUserFirstName = up ^. createUserFirstName
            , _protoUserLastName  = up ^. createUserLastName
            , _protoUserRole      = up ^. createUserRole
            , _protoUserPassword  = pwd
            , _protoUserEmail     = up ^. createUserEmail
            , _protoUserDesc      = Markdown nil
            }
    )
    (\_ u _ -> unwords ["Nutzer", createUserFullName u, "wurde angelegt."])
  where
    -- This is a clone of 'userFullName' for the type we need here.
    createUserFullName :: CreateUserPayload -> String
    createUserFullName u = unwords $ cs <$>
        [u ^. createUserFirstName . _UserFirstName, u ^. createUserLastName . _UserLastName]

adminViewClasses :: ActionPersist m => m AdminViewClasses
adminViewClasses = AdminViewClasses <$> query getSchoolClasses

adminEditUser :: ActionM m => AUID User -> FormPageHandler m AdminEditUser
adminEditUser uid = formPageHandlerCalcMsg
    (equery $ AdminEditUser <$> (maybe404 =<< findActiveUser uid) <*> getSchoolClasses)
    (update . uncurry (SetUserLoginAndRole uid))
    (\(AdminEditUser u _) _ _ -> unwords ["Nutzer", userFullName u, "wurde geändert."])

fromRoleSelection :: RoleSelection -> SchoolClass -> Role
fromRoleSelection RoleSelStudent     = Student
fromRoleSelection RoleSelClassGuest  = ClassGuest
fromRoleSelection RoleSelSchoolGuest = const SchoolGuest
fromRoleSelection RoleSelModerator   = const Moderator
fromRoleSelection RoleSelPrincipal   = const Principal
fromRoleSelection RoleSelAdmin       = const Admin

adminEditClass :: ActionPersist m => SchoolClass -> m AdminEditClass
adminEditClass clss =
    AdminEditClass clss
    <$> (makeUserView <$$> query (getUsersInClass clss))

instance FormPage AdminDeleteUser where
    type FormPagePayload AdminDeleteUser = ()

    formAction (AdminDeleteUser user) = U.Admin $ U.AdminDeleteUser (user ^. _Id)
    redirectOf _ _ = U.Admin U.adminViewUsers

    makeForm _ = pure ()

    formPage _v form p@(AdminDeleteUser user) =
        adminFrame p . semanticDiv p . form $ do
            div_ [class_ "container-confirm"] $ do
                h1_ "Nutzer löschen"
                p_ $ "Wollen Sie " >> toHtml (dangerousUserLongName user) >> " wirklich loschen?"
                div_ [class_ "admin-buttons"] $ do
                    DF.inputSubmit "Nutzer löschen"
                    a_ [href_ . U.Admin $ U.AdminEditUser (user ^. _Id), class_ "btn-cta"] "Zurück"

adminDeleteUser :: ActionM m => AUID User -> FormPageHandler m AdminDeleteUser
adminDeleteUser uid =
    formPageHandlerCalcMsg
        (AdminDeleteUser <$> mquery (findActiveUser uid))
        (const $ Action.deleteUser uid)
        (\(AdminDeleteUser u) _ _ -> unwords ["Nutzer", userFullName u, "wurde gelöscht"])


-- ** Events protocol

data EventsProtocolFilter = EventsProtocolFilter (Maybe IdeaSpace)
  deriving (Eq, Ord, Show, Read)

instance FormPage PageAdminSettingsEventsProtocol where
    type FormPagePayload PageAdminSettingsEventsProtocol = EventsProtocolFilter
    type FormPageResult PageAdminSettingsEventsProtocol = EventsProtocolFilter

    formAction (PageAdminSettingsEventsProtocol _) = U.Admin U.AdminEvent
    redirectOf _ (EventsProtocolFilter mspace)     = U.Admin (U.AdminDlEvents mspace)

    makeForm (PageAdminSettingsEventsProtocol spaces) = EventsProtocolFilter <$> ("space" .: DF.choice vs Nothing)
      where
        vs :: [(Maybe IdeaSpace, Html ())]
        vs = (Nothing, "(Alle Ideenräume)") : ((Just &&& toHtml . toUrlPiece) <$> spaces)

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
adminEventsProtocol = formPageHandler (PageAdminSettingsEventsProtocol <$> query getSpaces) pure


-- * Classes Create

data BatchCreateUsersFormData = BatchCreateUsersFormData ST (Maybe FilePath)
  deriving (Eq, Show, Generic)

instance SOP.Generic BatchCreateUsersFormData

instance FormPage AdminCreateClass where
    type FormPagePayload AdminCreateClass = BatchCreateUsersFormData

    formAction _   = U.Admin U.AdminCreateClass
    redirectOf _ _ = U.Admin U.AdminViewClasses

    makeForm _ = BatchCreateUsersFormData
        <$> ("classname" .: classname (DF.string Nothing))
        <*> ("file"      .: DF.file)
      where
        classname = validate
            "Name der Klasse"
            (cs <$> many1 anyChar <??> "nicht leer")

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

adminCreateClass :: forall m. (ReadTempFile m, ActionAddDb m, ActionRandomPassword m)
                              => FormPageHandler m AdminCreateClass
adminCreateClass = formPageHandlerWithMsg (pure AdminCreateClass) q msgOk
  where
    q :: BatchCreateUsersFormData -> m ()
    q (BatchCreateUsersFormData _clname Nothing) =
        throwError500 "upload FAILED: no file!"  -- FIXME: status code?
    q (BatchCreateUsersFormData clname (Just file)) = do
        eCsv :: Either String [CsvUserRecord] <- readTempCsvFile file
        case eCsv of
            Left msg      -> throwError500 $ "csv parsing FAILED: " <> cs msg
                                             -- FIXME: status code?
            Right records -> do
                let schoolcl = SchoolClass theOnlySchoolYearHack clname
                update . AddIdeaSpaceIfNotExists $ ClassSpace schoolcl
                forM_ records . p $ Student schoolcl

    p :: Role -> CsvUserRecord -> m ()
    p _        (CsvUserRecord _ _ _ _                          (Just _)) = do
        throwError500 "upload FAILED: internal error!"
    p role (CsvUserRecord firstName lastName mEmail mLogin Nothing) = do
      void $ do
        pwd <- mkRandomPassword
        currentUserAddDb AddUser ProtoUser
            { _protoUserLogin     = mLogin
            , _protoUserFirstName = firstName
            , _protoUserLastName  = lastName
            , _protoUserRole      = role
            , _protoUserPassword  = pwd
            , _protoUserEmail     = mEmail
            , _protoUserDesc      = Markdown nil
            }

    msgOk :: ST
    msgOk = "Die Klasse wurde angelegt."

adminPhaseChange
    :: forall m . (ActionM m)
    => FormPageHandler m AdminPhaseChange
adminPhaseChange =
    formPageHandlerCalcMsgM
        (pure AdminPhaseChange)
        (\(AdminPhaseChangeForTopicData tid dir) -> do
            case dir of
                Forward -> Action.topicForceNextPhase tid
                Backward -> Action.topicInVotingResetToJury tid
        )
        (\_ (AdminPhaseChangeForTopicData tid _) _ -> do
            topic <- Action.mquery $ findTopic tid
            return $ unwords
                [ "Das Thema wurde in Phase"
                , topic ^. topicPhase . to show
                , "verschoben."
                ]
        )


data PhaseChangeDir = Forward | Backward
  deriving (Eq, Show, Generic)

instance SOP.Generic PhaseChangeDir

phaseChangeDirText :: PhaseChangeDir -> ST
phaseChangeDirText Forward  = "vorwärts"
phaseChangeDirText Backward = "zurück"

instance ToHtml PhaseChangeDir where
    toHtmlRaw = toHtml
    toHtml    = toHtml . phaseChangeDirText

data AdminPhaseChangeForTopicData = AdminPhaseChangeForTopicData (AUID Topic) PhaseChangeDir
  deriving (Eq, Show)

-- FIXME: if we keep this, there needs to be some sort of feedback to the admin what happened with
-- the phase change.  we could redirect to a page showing a message of the form "topic with title
-- ... and id ... changed from phase ... to phase ...".  or we could add a message queue to the
-- session state that gets flushed and appended to the digestive functors errors implicitly whenever
-- we show a form.
instance FormPage AdminPhaseChange where
    type FormPagePayload AdminPhaseChange = AdminPhaseChangeForTopicData

    formAction _   = U.Admin U.AdminChangePhase
    redirectOf _ _ = U.Admin U.AdminChangePhase

    -- | Generates a Html view from the given page
    makeForm _ =
        AdminPhaseChangeForTopicData
            <$> ("topic-id" .: topicId (DF.string Nothing))
            <*> ("dir"      .: DF.choice choices Nothing)
      where
        choices = map (id &&& toHtml) [Forward, Backward]
        topicId = validate "ID des Themas"
            (AUID . read . cs <$> many1 digit <??> "Ziffern von 0-9")

    formPage v form p = adminFrame p . semanticDiv p $ do
        h3_ "Phasen verschieben"
        form $ do
            div_ $ do
                p_ "ID-Nummer der Themas aus der URL"
                DF.inputText "topic-id" v
            div_ $ do
                p_ "Vorwärts/Rückwärts"
                DF.inputSelect "dir" v
            DF.inputSubmit "Verschieben!"


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
