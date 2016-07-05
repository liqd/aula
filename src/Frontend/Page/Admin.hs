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
import Data.Set (Set)
import Servant

import qualified Data.Csv as Csv
import qualified Data.Set as Set
import qualified Data.Text as ST
import qualified Generics.SOP as SOP
import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF
import qualified Text.Digestive.Types as DF

import Access
import Action
import Logger.EventLog
import Persistent.Api
    ( SaveDurations(SaveDurations)
    , SaveQuorums(SaveQuorums)
    , SaveAndEnactFreeze(SaveAndEnactFreeze)
    , AddIdeaSpaceIfNotExists(AddIdeaSpaceIfNotExists)
    , AddUser(AddUser)
    , SetUserLogin(SetUserLogin)
    , AddUserRole(AddUserRole)
    , RemUserRole(RemUserRole)
    )
import Persistent
    ( dbDurations, dbQuorums, dbFreeze, loginIsAvailable, getUserViews, getSchoolClasses
    , findActiveUser, getUsersInClass, findActiveUser, getSpaces, getUsersInClass
    )
import Frontend.Prelude
import Frontend.Validation hiding (tab, spaces)

import qualified Frontend.Constant as Constant
import qualified Frontend.Path as U


-- * types

-- | 11.1 Admin settings: Durations
data PageAdminSettingsDurations =
    PageAdminSettingsDurations Durations
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsDurations where
    isAuthorized = adminPage
    isResponsive _ = False

-- | 11.2 Admin settings: Quorum
data PageAdminSettingsQuorum =
    PageAdminSettingsQuorum Quorums
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsQuorum where
    isAuthorized = adminPage
    isResponsive _ = False

-- | Admin settings: Freeze
data PageAdminSettingsFreeze =
    PageAdminSettingsFreeze Freeze
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsFreeze where
    isAuthorized = adminPage
    isResponsive _ = False

-- | 11.3 Admin settings: Manage groups & permissions
data AdminViewUsers = AdminViewUsers UsersQuery [UserView]
  deriving (Eq, Show, Read)

instance Page AdminViewUsers where
    isAuthorized   = adminPage
    isResponsive _ = False

data AdminCreateUser = AdminCreateUser [SchoolClass]
  deriving (Eq, Show, Read)

instance Page AdminCreateUser where
    isAuthorized   = adminPage
    isResponsive _ = False

data AdminAddRole = AdminAddRole User [SchoolClass]
  deriving (Eq, Show, Read)

instance Page AdminAddRole where
    isAuthorized   = adminPage
    isResponsive _ = False

data AdminEditUser = AdminEditUser User
  deriving (Eq, Show, Read)

instance Page AdminEditUser where
    isAuthorized   = adminPage
    isResponsive _ = False

data AdminDeleteUser = AdminDeleteUser User
  deriving (Eq, Show, Read)

instance Page AdminDeleteUser where
    isAuthorized   = adminPage
    isResponsive _ = False

data AdminViewClasses = AdminViewClasses ClassesFilterQuery [SchoolClass]
  deriving (Eq, Show, Read)

instance Page AdminViewClasses where
    isAuthorized   = adminPage
    isResponsive _ = False

data AdminCreateClass = AdminCreateClass
  deriving (Eq, Show, Read)

instance Page AdminCreateClass where
    isAuthorized   = adminPage
    isResponsive _ = False

data AdminEditClass = AdminEditClass SchoolClass [UserView]
  deriving (Eq, Show, Read)

instance Page AdminEditClass where
    isAuthorized   = adminPage
    isResponsive _ = False

data AdminPhaseChange = AdminPhaseChange
  deriving (Eq, Show, Read)

instance Page AdminPhaseChange where
    isAuthorized   = adminPage
    isResponsive _ = False

-- | 11.4 Admin settings: Events protocol
data PageAdminSettingsEventsProtocol =
    PageAdminSettingsEventsProtocol [IdeaSpace]
  deriving (Eq, Show, Read)

instance Page PageAdminSettingsEventsProtocol where
    isAuthorized   = adminPage
    isResponsive _ = False

data PageAdminResetPassword =
    PageAdminResetPassword User InitialPassword
  deriving (Eq, Show, Read)

instance Page PageAdminResetPassword where
    isAuthorized   = adminPage
    isResponsive _ = False

data CreateUserPayload = CreateUserPayload
    { _createUserFirstName :: UserFirstName
    , _createUserLastName  :: UserLastName
    , _createUserLogin     :: Maybe UserLogin
    , _createUserEmail     :: Maybe EmailAddress
    , _createUserRoleSet   :: Set Role
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

instance IsTab MenuItem

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

instance ToMenuItem AdminAddRole where
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

instance ToMenuItem PageAdminResetPassword where
    toMenuItem _ = MenuItemUsers


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

data MenuLink = MenuLink ST (U.Main 'U.AllowGetPost) ST
  deriving (Show)

menulink :: Monad m => MenuItem -> MenuItem -> HtmlT m ()
menulink curMenuItem targetMenuItem = case menulink' targetMenuItem of
    MenuLink ident path body ->
        a_ ([ id_ ident
            , href_ path
            ] <> tabSelected Desktop curMenuItem targetMenuItem)
          $ toHtml body

menulink' :: MenuItem -> MenuLink
menulink' targetMenuItem =
  case targetMenuItem of
    MenuItemDurations
        -> MenuLink "tab-duration" U.adminDuration "Dauer der Phasen"
    MenuItemQuorum
        -> MenuLink "tab-qourum" U.adminQuorum "Quorum"
    MenuItemFreeze
        -> MenuLink "tab-freeze" U.adminFreeze "Ferienmodus"
    MenuItemUsers
        -> MenuLink "tab-groups-perms-user"  U.adminViewUsers "Nutzer"
    MenuItemClasses
        -> MenuLink "tab-groups-perms-class" U.adminViewClasses "Klasse"
    MenuItemClassesAndUsers
        -> MenuLink "tab-groups-perms"       U.adminViewUsers "Gruppen & Nutzer"
    MenuItemEventsProtocol
        -> MenuLink "tab-events"             U.adminEvent "Protokolle"
    MenuItemPhaseChange
        -> MenuLink "tab-phase-change" U.adminChangePhase "Phasen verschieben"

instance FormPage PageAdminSettingsDurations where
    type FormPagePayload PageAdminSettingsDurations = Durations

    formAction _ = U.adminDuration
    redirectOf _ _ = U.adminDuration

    makeForm (PageAdminSettingsDurations dur) =
        Durations <$> ("elab-duration" .: period (pNam PhaseRefinement) elaborationPhase)
                  <*> ("vote-duration" .: period (pNam PhaseVoting)     votingPhase)
      where
        period name getter = validate
            name
            (DurationDays <$> inRangeV Constant.minElabPeriod Constant.maxElabPeriod)
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

    formAction _   = U.adminQuorum
    redirectOf _ _ = U.adminQuorum

    makeForm (PageAdminSettingsQuorum q) =
        Quorums
        <$> ("school-quorum" .: percentage "Schulquorum"    schoolQuorumPercentage)
        <*> ("class-quorum"  .: percentage "Klassenquorum"  classQuorumPercentage)
      where
        percentage name getter = validate
            name
            (inRangeV 1 100)
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

    formAction _   = U.adminFreeze
    redirectOf _ _ = U.adminFreeze

    makeForm (PageAdminSettingsFreeze current) =
        "freeze" .: DF.choice ((id &&& showOption) <$> [minBound..]) (Just current)
      where
        showOption NotFrozen = "Normalbetrieb (aufgetaut)"
        showOption Frozen    = "Ferienbetrieb (eingefroren)"

    formPage v form p = adminFrame p . semanticDiv p . form $ do
        div_ [class_ "container-info"] $ do
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
        (\_ f _ ->
            case f of
                Frozen    -> "Das System wurde eingefroren (Ferienbetrieb)." :: ST
                NotFrozen -> "Das System wurde re-aktiviert (Normalbetrieb).")


-- ** roles and permisisons

instance ToHtml AdminViewUsers where
    toHtml = toHtmlRaw
    toHtmlRaw p@(AdminViewUsers filters (applyFilter filters -> users)) =
        adminFrame p . semanticDiv p $ do
            div_ [class_ "clearfix btn-settings pop-menu"] $ do
                i_ [class_ "icon-sort", title_ "Sortieren nach"] nil
                ul_ [class_ "pop-menu-list"] $ do
                    sequence_
                        [ li_ [class_ "pop-menu-list-item"] $
                            a_ [href_ . U.adminViewUsers' . Just $
                                    filters & usersQueryS .~ by]
                                (uilabel by)
                        | by <- [minBound..] ]
            table_ [class_ "admin-table"] $ do
                thead_ . tr_ $ do
                    th_ nil
                    th_ "Name"
                    th_ "Klasse"
                    th_ "Rolle"
                    th_ $ button_ [class_ "btn-cta", onclick_ U.adminCreateUser] "Nutzer anlegen"
                    th_ $ do
                        div_ [class_ "inline-search-container"] $ do
                            -- The AllUsers here makes sure there is no 'search' query parameter
                            -- initially. The input field is adding it afterward.
                            let filters' = filters & usersQueryF .~ AllUsers
                                placehld = fromMaybe "Nutzersuche" (filters ^? usersQueryF . searchUsers . unSearchUsers)
                            formMethod_ "GET" [class_ "form"]
                                        (U.adminViewUsers' $ Just filters') $ do
                                input_ [name_ "search", type_ "text", class_ "inline-search-input",
                                        placeholder_ placehld]
                                button_ [type_ "submit", class_ "inline-search-button"] $ i_ [class_ "icon-search"] nil

                let renderUserInfoRow :: forall m. (Monad m) => User -> HtmlT m ()
                    renderUserInfoRow user = do
                        td_ $ user ^. userLogin . unUserLogin . html
                        td_ . toHtml $ ST.intercalate "," (user ^.. userSchoolClasses . uilabeled)
                        td_ . toHtml $ ST.intercalate "," (user ^.. userRoles . uilabeled)
                        td_ $ toHtmlRaw nbsp

                let renderUserRow :: forall m. (Monad m) => UserView -> HtmlT m ()
                    renderUserRow (DeletedUser user) = tr_ $ do
                        td_ nil
                        renderUserInfoRow user
                        td_ "[gelöscht]"

                    renderUserRow (ActiveUser user) = tr_ $ do
                        td_ . span_ [class_ "img-container"] $ user ^. userAvatarImg avatarDefaultSize
                        renderUserInfoRow user
                        td_ $ a_ [href_ $ U.adminEditUser user] "bearbeiten"

                tbody_ $ case users of
                    []  -> tr_ $ td_ [class_ "container-not-found"] "(Keine Einträge.)"
                    _:_ -> renderUserRow `mapM_` users

instance FormPage AdminCreateUser where
    type FormPagePayload AdminCreateUser = CreateUserPayload

    formAction _   = U.adminCreateUser
    redirectOf _ _ = U.adminViewUsers

    -- FIXME: Show the user's role and class as default in the selections.
    makeForm (AdminCreateUser classes) =
        CreateUserPayload
            <$> ("firstname"  .: firstName (DF.string Nothing))
            <*> ("lastname"   .: lastName  (DF.string Nothing))
            <*> ("login"      .: loginName (DF.optionalString Nothing))
            <*> optionalEmailField "Email" Nothing
            <*> (Set.singleton <$> roleForm Nothing Nothing classes)
        where
            -- FIXME: Users with more than one name?
            firstName = validate "Vorname"  (fieldParser (UserFirstName . cs <$> many1 letter) "nur Buchstaben")
            lastName  = validate "Nachname" (fieldParser (UserLastName  . cs <$> many1 letter) "nur Buchstaben")
            loginName = validateOptional "Login" (UserLogin <$> usernameV)

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
    toHtmlRaw p@(AdminViewClasses filters (applyFilter filters -> classes)) =
        adminFrame p . semanticDiv p $ do
            table_ [class_ "admin-table"] $ do
                thead_ . tr_ $ do
                    th_ "Klasse"
                    th_ $ button_
                            [ class_ "btn-cta"
                            , onclick_ U.adminCreateClass
                            ]
                            "Klasse anlegen"
                    th_ $ do
                        div_ [class_ "inline-search-container"] $ do  -- see also: ToHtml instance of AdminViewUser
                            formMethod_ "GET" [class_ "form"]
                                        U.adminViewClasses $ do
                                input_ [name_ "search", type_ "text", class_ "inline-search-input",
                                        placeholder_ (fromMaybe "Klassensuche" (filters ^? searchClasses . unSearchClasses))]
                                button_ [type_ "submit", class_ "inline-search-button"] $ i_ [class_ "icon-search"] nil

                tbody_ $ case classes of
                    []  -> tr_ $ td_ [class_ "container-not-found"] "(Keine Einträge.)"
                    _:_ -> forM_ classes $ \clss -> tr_ $ do
                        td_ $ clss ^. className . html
                        td_ $ toHtmlRaw nbsp
                        td_ $ a_ [href_ $ U.adminEditClass clss] "bearbeiten"

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

roleSelectionChoices :: (Monoid s, IsString s) => [(RoleSelection, s)]
roleSelectionChoices = (id &&& uilabel) <$> [minBound..]

roleSelection :: Getter Role RoleSelection
roleSelection = to $ \case
    Student{}    -> RoleSelStudent
    ClassGuest{} -> RoleSelClassGuest
    SchoolGuest  -> RoleSelSchoolGuest
    Moderator    -> RoleSelModerator
    Principal    -> RoleSelPrincipal
    Admin        -> RoleSelAdmin

chooseRole :: (Monad n) => Maybe Role -> Monad m => DF.Form (HtmlT n ()) m RoleSelection
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

instance FormPage AdminAddRole where
    type FormPagePayload AdminAddRole = Role

    formAction (AdminAddRole user _classes)   = U.adminAddRole user
    redirectOf (AdminAddRole user _classes) _ = U.adminEditUser user

    makeForm (AdminAddRole _user classes) = roleForm Nothing Nothing classes

    formPage v form p@(AdminAddRole user _classes) =
        adminFrame p . semanticDiv' [class_ "admin-container"] p . form $ do
            div_ [class_ "col-9-12"] $ do
                h1_ [class_ "admin-main-heading"] $ toHtml (userFullName user :: ST)
                div_ [class_ "clearfix"] $ do
                    label_ [class_ "col-6-12"] $ do
                        span_ [class_ "label-text"] "Nutzerrolle"
                        let jscall = cs $ "toggleShowSchoolClass" <> jsargs
                            jsargs    :: String   = "(event, " <> show showclson <> ")"
                            -- FIXME: we should use digestive-functors to construct the following.
                            showclson :: [String] = (prefix <>) <$> ["0", "1"]
                            prefix    :: String   = "/admin/user/" <> (user ^. _Id . unAUID . showed) <> "/role/add.role."
                        inputSelect_ [class_ "m-stretch", onchange_ jscall, onload_ jscall] "role" v
                    label_ [class_ "col-6-12"] $ do
                        span_ [class_ "label-text"] "Klasse"
                        inputSelect_ [class_ "m-stretch"]  "class" v
                div_ [class_ "admin-buttons"] $ do
                    DF.inputSubmit "Rolle hinzufügen"


-- | This was refactored in 1acd4961b2 to not allow editing of roles any more.  You can only add and
-- remove roles from users.
instance FormPage AdminEditUser where
    -- | (the login must always be provided in the posted data, but it is turned into Nothing in the
    -- validator if it has not changed.)
    type FormPagePayload AdminEditUser = Maybe UserLogin

    formAction (AdminEditUser user) = U.adminEditUser user
    redirectOf _ _ = U.adminViewUsers

    makeForm (AdminEditUser user) = "login" .: validateUserLogin
      where
        validateUserLogin :: (Monad n, ActionM m) => DF.Form (HtmlT n ()) m (Maybe UserLogin)
        validateUserLogin = DF.validateM go . validate "Login" usernameV' $ dfTextField user userLogin _UserLogin

        go :: forall m n. (Monad n, ActionM m) => UserLogin -> m (DF.Result (HtmlT n ()) (Maybe UserLogin))
        go "" = pure $ DF.Error "login darf nicht leer sein"
        go lgin = if lgin == user ^. userLogin
            then pure (DF.Success Nothing)
            else do
                yes <- query $ loginIsAvailable lgin
                if yes then pure . DF.Success $ Just lgin
                       else pure . DF.Error   $ "login ist bereits vergeben"


    formPage v form p@(AdminEditUser user) =
        adminFrame p . semanticDiv' [class_ "admin-container"] p . form $ do
            div_ [class_ "col-9-12"] $ do
                h1_ [class_ "admin-main-heading"] $ do
                    span_ [class_ "label-text"] "Login"
                    inputText_ [class_ "m-stretch"] "login" v
                table_ [class_ "admin-roles"] $ do
                    thead_ . tr_ $ do
                        th_ "Nutzerrolle"
                        th_ "Klasse"
                        th_ nil
                    tbody_ . forM_ (user ^.. userRoles) $ \role -> tr_ $ do
                        td_ $ role ^. uilabeledST . html
                        td_ $ role ^. roleSchoolClass . uilabeledST . html
                        td_ $ postButton_
                                [ class_ "btn-cta"
                                , jsReloadOnClickConfirm "Soll diese Rolle wirklich entfernt werden?"
                                ]
                                (U.adminRemRole user role) "Rolle löschen"
                div_ [class_ "admin-buttons"] $ do
                    a_ [href_ $ U.adminAddRole user, class_ "btn-cta"] "Rolle hinzufügen"
                    br_ []
                    a_ [href_ $ U.adminResetPassword user, class_ "btn-cta"] "Passwort zurücksetzen"
                    br_ []
                    a_ [href_ $ U.adminDeleteUser user, class_ "btn-cta"] "Nutzer löschen"
                    br_ []
                    DF.inputSubmit "Änderungen speichern"

instance ToHtml AdminEditClass where
    toHtml = toHtmlRaw
    toHtmlRaw p@(AdminEditClass schoolClss users) =
        adminFrame p . semanticDiv p $ do
            div_ . h1_ [class_ "admin-main-heading"] $ schoolClss ^. className . html
            div_ $ a_ [class_ "admin-buttons", href_ . U.adminDlPass $ schoolClss]
                "Passwort-Liste"
            table_ [class_ "admin-table"] $ do
                thead_ . tr_ $ do
                    th_ nil
                    th_ "Name"
                    th_ nil
                tbody_ . forM_ (activeUsers users) $ \user -> tr_ $ do
                    td_ . span_ [class_ "img-container"] $ user ^. userAvatarImg avatarDefaultSize
                    td_ $ user ^. userLogin . unUserLogin . html
                    td_ $ a_ [href_ $ U.adminEditUser user] "bearbeiten"


adminViewUsers :: ActionPersist m => Maybe SearchUsers -> Maybe SortUsersBy -> m AdminViewUsers
adminViewUsers qf qs = AdminViewUsers (mkUsersQuery qf qs) <$> query getUserViews

adminCreateUser :: (ActionPersist m, ActionUserHandler m, ActionRandomPassword m,
                    ActionCurrentTimestamp m) => FormPageHandler m AdminCreateUser
adminCreateUser = formPageHandlerCalcMsg
    (AdminCreateUser <$> query getSchoolClasses)
    (\up -> do
        forM_ (up ^.. createUserRoleSet . folded . roleSchoolClass) $
            update . AddIdeaSpaceIfNotExists . ClassSpace
        pwd <- mkRandomPassword
        addWithCurrentUser_ AddUser ProtoUser
            { _protoUserLogin     = up ^. createUserLogin
            , _protoUserFirstName = up ^. createUserFirstName
            , _protoUserLastName  = up ^. createUserLastName
            , _protoUserRoleSet   = up ^. createUserRoleSet
            , _protoUserPassword  = pwd
            , _protoUserEmail     = up ^. createUserEmail
            , _protoUserDesc      = nil
            }
    )
    (\_ u _ -> unwords ["Nutzer", createUserFullName u, "wurde angelegt."])
  where
    -- This is a clone of 'userFullName' for the type we need here.
    createUserFullName :: CreateUserPayload -> String
    createUserFullName u = unwords $ cs <$>
        [u ^. createUserFirstName . _UserFirstName, u ^. createUserLastName . _UserLastName]

adminViewClasses :: ActionPersist m => Maybe SearchClasses -> m AdminViewClasses
adminViewClasses qf = AdminViewClasses (mkClassesQuery qf) <$> query getSchoolClasses

adminAddRole :: ActionM m => AUID User -> FormPageHandler m AdminAddRole
adminAddRole uid = formPageHandlerCalcMsg
    (equery $ AdminAddRole <$> (maybe404 =<< findActiveUser uid) <*> getSchoolClasses)
    (update . AddUserRole uid)
    (\(AdminAddRole u _) _ _ -> unwords ["Rolle wurde Nutzer", userFullName u, "zugewiesen."])

adminRemRole :: ActionM m => AUID User -> Role -> m ()
adminRemRole uid = update . RemUserRole uid

adminEditUser :: ActionM m => AUID User -> FormPageHandler m AdminEditUser
adminEditUser uid = formPageHandlerCalcMsg
    (equery $ AdminEditUser <$> (maybe404 =<< findActiveUser uid))
    (mapM_ $ update . SetUserLogin uid)
    (\(AdminEditUser u) _ _ -> unwords ["Nutzer", userFullName u, "wurde geändert."])

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

data AdminDeleteUserPayload = AdminDeleteUserPayload
  deriving (Eq, Show)

instance FormPage AdminDeleteUser where
    type FormPagePayload AdminDeleteUser = AdminDeleteUserPayload

    formAction (AdminDeleteUser user) = U.adminDeleteUser user
    redirectOf _ _ = U.adminViewUsers

    makeForm _ = pure AdminDeleteUserPayload

    formPage _v form p@(AdminDeleteUser user) =
        adminFrame p . semanticDiv' [class_ "container-confirm"] p . form $ do
            h1_ "Nutzer löschen"
            p_ $ "Wollen Sie " >> toHtml (dangerousUserLongName user) >> " wirklich loschen?"
            div_ [class_ "admin-buttons"] $ do
                DF.inputSubmit "Nutzer löschen"
                a_ [href_ $ U.adminEditUser user, class_ "btn-cta"] "Zurück"

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

    formAction (PageAdminSettingsEventsProtocol _) = U.adminEvent
    redirectOf _ (EventsProtocolFilter mspace)     = U.adminDlEvents mspace

    makeForm (PageAdminSettingsEventsProtocol spaces) = EventsProtocolFilter <$> ("space" .: DF.choice vs Nothing)
      where
        vs :: Monad n => [(Maybe IdeaSpace, HtmlT n ())]
        vs = (Nothing, "(Alle Ideenräume)") : ((Just &&& toHtml . toUrlPiece) <$> spaces)

    formPage v form p@(PageAdminSettingsEventsProtocol _) = adminFrame p . semanticDiv p . form $ do
        label_ $ do
            span_ [class_ "label-text"] "Hier konnen Sie das Event-Protokoll als CSV-Datei herunterladen"
            inputSelect_ [class_ "m-stretch"] "space" v
        div_ [class_ "download-box"] $ do
            header_ [class_ "download-box-header"] $ do
                "Event-Protokoll"
                button_ [type_ "submit", class_ "btn-cta download-box-button"] "Download"
            p_ [class_ "download-box-body"] "Das Event-Protokoll enthält alle Aktivitäten der NutzerInnen auf Aula"

adminEventsProtocol :: (ActionM m) => FormPageHandler m PageAdminSettingsEventsProtocol
adminEventsProtocol = formPageHandler (PageAdminSettingsEventsProtocol <$> query getSpaces) pure

adminEventLogCsv :: ActionM m => Maybe IdeaSpace -> m (CsvHeaders EventLog)
adminEventLogCsv mspc = hdrs . maybe id filterEventLog mspc <$> readEventLog
  where
    hdrs = csvHeaders $ "EventLog " <> maybe "alle Ideenräume" uilabel mspc


-- * Classes Create

data BatchCreateUsersFormData = BatchCreateUsersFormData ST (Maybe FilePath)
  deriving (Eq, Show, Generic)

instance SOP.Generic BatchCreateUsersFormData

instance FormPage AdminCreateClass where
    type FormPagePayload AdminCreateClass = BatchCreateUsersFormData

    formAction _   = U.adminCreateClass
    redirectOf _ _ = U.adminViewClasses

    makeForm _ = BatchCreateUsersFormData
        <$> ("classname" .: classname (DF.string Nothing))
        <*> ("file"      .: DF.file)
      where
        classname = validate
            "Name der Klasse"
            (fieldParser (cs <$> many1 anyChar) "nicht leer")

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
                forM_ records . p . Set.singleton $ Student schoolcl

    p :: Set Role -> CsvUserRecord -> m ()
    p _     (CsvUserRecord _ _ _ _                          (Just _)) = do
        throwError500 "upload FAILED: internal error!"
    p roles (CsvUserRecord firstName lastName mEmail mLogin Nothing) = do
      void $ do
        pwd <- mkRandomPassword
        addWithCurrentUser AddUser ProtoUser
            { _protoUserLogin     = mLogin
            , _protoUserFirstName = firstName
            , _protoUserLastName  = lastName
            , _protoUserRoleSet   = roles
            , _protoUserPassword  = pwd
            , _protoUserEmail     = mEmail
            , _protoUserDesc      = nil
            }

    msgOk :: ST
    msgOk = "Die Klasse wurde angelegt."

adminPhaseChange
    :: forall m . (ActionM m)
    => FormPageHandler m AdminPhaseChange
adminPhaseChange =
    formPageHandler
        (pure AdminPhaseChange)
        (\(AdminPhaseChangeForTopicData tid dir) -> Action.topicForcePhaseChange dir tid)


data AdminPhaseChangeForTopicData = AdminPhaseChangeForTopicData (AUID Topic) PhaseChangeDir
  deriving (Eq, Show)

instance FormPage AdminPhaseChange where
    type FormPagePayload AdminPhaseChange = AdminPhaseChangeForTopicData

    formAction _   = U.adminChangePhase
    redirectOf _ _ = U.adminChangePhase

    -- | Generates a Html view from the given page
    makeForm _ =
        AdminPhaseChangeForTopicData
            <$> ("topic-id" .: topicId (DF.string Nothing))
            <*> ("dir"      .: DF.choice choices Nothing)
      where
        choices = map (id &&& toHtml) [Forward, Backward]
        topicId = validate "ID des Themas"
            (fieldParser (AUID . read . cs <$> many1 digit) "Ziffern von 0-9")

    formPage v form p = adminFrame p . semanticDiv p $ do
        h3_ "Phasen verschieben"
        form $ do
            div_ [class_ "container-info"] $ do
                p_ "ACHTUNG!  GEFAHR!"
                ul_ $ do
                    li_ "Diese Seite erlaubt es, Themen in beide Richtungen (Zukunft und Vergangenheit) zu verschieben."
                    li_ "Dies ist ein experimentelles Feature, und kann zu unerwartetem Verhalten führen.  Bitte nur mit"
                    li_ "gutem Grund und nur nach Rücksprache mit den zuständigen Moderatoren durchführen!"

            div_ $ do
                p_ "ID-Nummer der Themas aus der URL"
                DF.inputText "topic-id" v
            div_ $ do
                p_ "Vorwärts/Rückwärts"
                DF.inputSelect "dir" v
            DF.inputSubmit "Verschieben!"

instance FormPage PageAdminResetPassword where
    type FormPagePayload PageAdminResetPassword = InitialPassword

    formAction (PageAdminResetPassword u _p) = U.adminResetPassword u
    redirectOf (PageAdminResetPassword u _p) _ = U.adminEditUser u

    makeForm (PageAdminResetPassword _u p) =
        InitialPassword <$> ("new-pwd" .: DF.text (p ^. unInitialPassword . to Just))

    formPage v form p@(PageAdminResetPassword usr pwd) = adminFrame p . semanticDiv p $ do
        h3_ $ "Passwort zurücksetzen für Nutzer #" <> usr ^. _Id . unAUID . showed . html
        form $ do
            div_ $ do
                table_ [class_ "admin-table", style_ "padding: 30px"] $ do
                    tr_ $ do
                        td_ "Login:"
                        td_ $ usr ^. userLogin . unUserLogin . html
                    tr_ $ do
                        td_ "New password:"
                        td_ $ pwd ^. unInitialPassword . html
                p_ "Soll diesees Passwort gesetzt werden?"
            div_ $ do
                DF.inputHidden "new-pwd" v
                DF.inputSubmit "Ja!"
                a_ [class_ "btn", href_ $ redirectOf p ()] "Zurück"

adminResetPassword :: ActionM m => AUID User -> FormPageHandler m PageAdminResetPassword
adminResetPassword userId = formPageHandlerWithMsg
    (PageAdminResetPassword <$> mquery (findActiveUser userId) <*> mkRandomPassword)
    (Action.resetPassword userId)
    ("Das Password von Nutzer #" <> userId ^. unAUID . showed . csi <> " wurde geändert!")


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

instance Page InitialPasswordsCsv where
    isAuthorized = adminPage
    isResponsive _ = False


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
        parseName mxLength i
            | length v < i + 1
                = fail $ "user record too short: " <> show v
            | ST.length (v !! i) > mxLength
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
    csvHeaders ("Passwortliste " <> clss ^. uilabeled) .
    InitialPasswordsCsv . catMaybes . fmap mk <$> query (getUsersInClass clss)
  where
    mk u = case u ^. userPassword of
        UserPassInitial (InitialPassword ps) -> Just $ CsvUserRecord
              (u ^. userFirstName)
              (u ^. userLastName)
              (u ^. userEmail)
              (Just $ u ^. userLogin)
              (Just ps)
        _ -> Nothing
