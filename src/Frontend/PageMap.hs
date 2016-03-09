{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.PageMap
where

import Frontend.Page


type family Reachable from to :: *

-- Mapped from click-dummy: https://marvelapp.com/ehhb43
-- FIXME: Header, Menu, Footer are on every page, but we just map it for PageRoomsOverview.
-- FIXME: move page map to separate module?


-- * 1. Rooms Overview

-- Header
type instance Reachable PageRoomsOverview PageRoomsOverview = () -- Aula icon, Ideenraume

-- Menu
type instance Reachable PageRoomsOverview PageUserProfileDelegatedVotes = () -- Profil anzeigen
type instance Reachable PageRoomsOverview PageUserSettings              = () -- Einstellungen
type instance Reachable PageRoomsOverview PageAdminSettingsDurations    = () -- Prozessverwaltung
type instance Reachable PageRoomsOverview PageLogout                    = () -- Logout

-- Footer
type instance Reachable PageRoomsOverview PageStaticImprint    = () -- Imprint
type instance Reachable PageRoomsOverview PageStaticTermsOfUse = () -- Terms of use

-- Content
type instance Reachable PageRoomsOverview PageIdeasOverview = () -- Klasse


-- * 2. Ideas Overview

type instance Reachable PageIdeasOverview ViewTopic  = () -- Ideen auf dem Tisch def Klasse 7a (Topic Overview)
type instance Reachable PageIdeasOverview CreateIdea = () -- Neue Idee (Create Idea)
type instance Reachable PageIdeasOverview ViewIdea   = () -- Titel einer Idee (Idea detail page: New Ideas)


-- * 3. Ideas in discussion (Topics overview)

-- TODO


-- * 4. Topic Overview

-- TODO

-- type instance Reachable ViewTopic = ()

type instance Reachable ViewTopic ViewIdea = () -- Wilde Ideen der Klasse 7a (5.1 Idea detail pages)
type instance Reachable ViewTopic ViewTopic = () -- ausarbeitungphase (4.1 Topic Overview: refinement phase)
-- type instance Reachable ViewTopic = ()-- prufungphase (4.2 Topic Overview: assignment phase)
-- type instance Reachable ViewTopic = ()-- ergebnishphase (4.4 Topic Overview: Result phase)
-- type instance Reachable ViewTopic = ()-- abstimmungphase (4.3 Topic Overview: Voting phase)
-- ergennishphase ??? (duplicated) (4.4 Topic Overview: Result phase 3)
-- ausarbeitungphase ??? (duplicated) (4.1 Topic Overview: refinement phase)


-- * 4.1 Topic overview: Refinement phase

type instance Reachable ViewTopic MoveIdeasToTopic = () -- Bearbiten (10.2 Edit Topic: Refinement)
type instance Reachable ViewTopic CreateIdea = () -- Neue idee (6. Create Idea: Refinement phase)
type instance Reachable ViewTopic PageDelegateVote = () -- Stimme beauftragen (12. Delegate Vote: Ausarbeitungphase)
-- type instance Reachable ViewTopic ViewTopic = () -- beauftragen stimmen (4.1 Topic Overview: Refinemnt phase 2) !!!
type instance Reachable ViewTopic ViewIdea = () -- Title der idee (5.2 Idea Detail Page: Refinement phase)
type instance Reachable ViewTopic PageUserProfileDelegatedVotes = () -- Avatar, VorNam (8.2 User Profile: Delegated Votes)

-- * 4.2 Topic overview: Jury (assessment) phase
-- * 4.3 Topic overview: Voting phase
-- * 4.4 Topic overview: Result phase
-- * 4.5 Topic overview: Delegations

-- ...


-- * 5 Idea detail pages:

-- TODO

-- * 5.1 Idea detail pages: New Ideas

type instance Reachable ViewIdea EditIdea = () -- bearbieten (7. Edit idea)
-- type instance Reachable ViewIdea ViewIdea = () -- Idee verschieben (5.1 Idea Detail Page)
type instance Reachable ViewIdea PageIdeasOverview = () -- Zu den wilden ideen
type instance Reachable ViewIdea PageUserProfileDelegatedVotes = () -- VorNam (8.2 User Profile Delegated Votes)
type instance Reachable ViewIdea ViewTopic = () -- Idee auf den tisch bringen (4. Topic Overview)
type instance Reachable ViewIdea PageDelegateVote = () -- Stimme beauftragen (12. Delegate - Wilde Idee)
-- Neuer verbesserungsvorschlag -- New comment???

-- * 5.1 Idea detail page: New ideas
-- * 5.2 Idea detail page: Refinement phase
-- * 5.3 Idea detail page: Jury (assessment) phase
-- * 5.4 Idea detail page: Voting phase
-- * 5.6 Idea detail page: Feasible / not feasible
-- * 5.7 Idea detail page: Winner


-- * 6. Create Idea

-- TODO

-- On the create dummy it is an overlay
-- type instance Reachable CreateIdea PageLogout = ()
-- type instance Reachable CreateIdea PageStaticImprint = () -- Imprint
-- type instance Reachable CreateIdea PageStaticTermsOfUse = () -- Terms of use

type instance Reachable CreateIdea PageIdeasOverview = () -- X, Cancel, Idee veroffentlichen
-- type instance Reachable CreateIdea IdeeErsteltLayout = () -- Idee veroffentlichen ???


-- * 7. Create Idea

-- TODO


-- * 8 User Profile

-- * 8.1 User Profile: Created Ideas

-- TODO

-- type instance Reachable PageUserProfileCreateIdeas = ()

-- type instance Reachable PageUserProfileCreateIdeas PageUserProfileDelegatedVotes = () -- Erhaltene Stimmen
-- type instance Reachable PageUserProfileCreateIdeas ViewIdea = () -- Titel einer Idee (5.1 Idea Detail Page: New Ideas)


-- * 9 User Settings

-- TODO


-- * 10 Create Topic

-- TODO


-- * 11. Admin pages

-- ** 11.1 Admin settings: Durations

-- side bar
type instance Reachable PageAdminSettingsDurations PageAdminSettingsQuorum         = ()
type instance Reachable PageAdminSettingsDurations PageAdminSettingsGaPUsersView   = ()
type instance Reachable PageAdminSettingsDurations PageAdminSettingsEventsProtocol = ()

-- page
type instance Reachable PageAdminSettingsDurations PageAdminSettingsDurations = ()

-- ** 11.2 Admin settings: Quorum

-- side bar
type instance Reachable PageAdminSettingsQuorum PageAdminSettingsDurations      = ()
type instance Reachable PageAdminSettingsQuorum PageAdminSettingsGaPUsersView   = ()
type instance Reachable PageAdminSettingsQuorum PageAdminSettingsEventsProtocol = ()

-- page
type instance Reachable PageAdminSettingsQuorum PageAdminSettingsQuorum = ()


-- ** 11.3 Admin settings: Manage groups & permissions

-- side bar
type instance Reachable PageAdminSettingsGaPUsersView PageAdminSettingsDurations      = ()
type instance Reachable PageAdminSettingsGaPUsersView PageAdminSettingsQuorum         = ()
type instance Reachable PageAdminSettingsGaPUsersView PageAdminSettingsGaPUsersView   = ()
type instance Reachable PageAdminSettingsGaPUsersView PageAdminSettingsGaPClassesView = ()
type instance Reachable PageAdminSettingsGaPUsersView PageAdminSettingsEventsProtocol = ()

-- page
type instance Reachable PageAdminSettingsGaPUsersView PageAdminSettingsGaPUsersCreate = ()
type instance Reachable PageAdminSettingsGaPUsersView PageAdminSettingsGaPUsersEdit    = ()

data PageAdminSettingsGaPUsersEdit  -- FIXME: "bearbeiten"
-- FIXME: side bar for PageAdminSettingsGaPUsersEdit
-- FIXME: search field
-- FIXME: reset password


-- side bar
type instance Reachable PageAdminSettingsGaPUsersCreate PageAdminSettingsDurations      = ()
type instance Reachable PageAdminSettingsGaPUsersCreate PageAdminSettingsQuorum         = ()
type instance Reachable PageAdminSettingsGaPUsersCreate PageAdminSettingsGaPUsersView   = ()
type instance Reachable PageAdminSettingsGaPUsersCreate PageAdminSettingsGaPClassesView = ()
type instance Reachable PageAdminSettingsGaPUsersCreate PageAdminSettingsEventsProtocol = ()

-- page
-- (reset password is handled under users-view.)
-- ("save" goes to PageAdminSettingsGaPUsersView.)


-- side bar
type instance Reachable PageAdminSettingsGaPClassesView PageAdminSettingsDurations      = ()
type instance Reachable PageAdminSettingsGaPClassesView PageAdminSettingsQuorum         = ()
type instance Reachable PageAdminSettingsGaPClassesView PageAdminSettingsGaPUsersView   = ()
type instance Reachable PageAdminSettingsGaPClassesView PageAdminSettingsGaPClassesView = ()
type instance Reachable PageAdminSettingsGaPClassesView PageAdminSettingsEventsProtocol = ()

-- page
type instance Reachable PageAdminSettingsGaPClassesView PageAdminSettingsGaPClassesCreate = ()
type instance Reachable PageAdminSettingsGaPClassesView PageAdminSettingsGaPClassesEdit   = ()
-- FIXME: search field.


-- side bar
type instance Reachable PageAdminSettingsGaPClassesCreate PageAdminSettingsDurations      = ()
type instance Reachable PageAdminSettingsGaPClassesCreate PageAdminSettingsQuorum         = ()
type instance Reachable PageAdminSettingsGaPClassesCreate PageAdminSettingsGaPUsersView   = ()
type instance Reachable PageAdminSettingsGaPClassesCreate PageAdminSettingsGaPClassesView = ()
type instance Reachable PageAdminSettingsGaPClassesCreate PageAdminSettingsEventsProtocol = ()

-- page
-- ("save" goes back PageAdminSettingsGaPClassesView.)
type instance Reachable PageAdminSettingsGaPClassesCreate PageAdminSettingsGaPClassesEdit = ()
type instance Reachable PageAdminSettingsGaPClassesCreate PageAdminSettingsGaPClassesEdit = ()

data PageAdminSettingsGaPClassesEdit  -- FIXME: "bearbeiten"
-- FIXME: side bar for PageAdminSettingsGaPClassesEdit
-- FIXME: can we edit class name?  (this would be really bad.)
-- NOTE: editing user returns to where it came from (either "edit class" or "edit users").

type instance Reachable PageAdminSettingsGaPClassesEdit PageAdminSettingsGaPUsersEdit     = ()


-- side bar
type instance Reachable PageAdminSettingsEventsProtocol PageAdminSettingsDurations      = ()
type instance Reachable PageAdminSettingsEventsProtocol PageAdminSettingsQuorum         = ()
type instance Reachable PageAdminSettingsEventsProtocol PageAdminSettingsGaPUsersView   = ()
type instance Reachable PageAdminSettingsEventsProtocol PageAdminSettingsGaPClassesView = ()
type instance Reachable PageAdminSettingsEventsProtocol PageAdminSettingsEventsProtocol = ()

-- page
type instance Reachable PageAdminSettingsEventsProtocol PageAdminSettingsEventsProtocol = ()


-- * 12. Delegate Votes

-- FIXME


-- * 13. Delegation Network

-- FIXME

-- * 14. Static Page: Imprint

type instance Reachable PageStaticImprint PageStaticImprint       = ()
type instance Reachable PageStaticImprint PageStaticTermsOfUse    = ()
type instance Reachable PageStaticImprint PageHomeWithLoginPrompt = ()

-- * 12. Static Page: Terms of use

type instance Reachable PageStaticTermsOfUse PageStaticTermsOfUse    = ()
type instance Reachable PageStaticTermsOfUse PageStaticImprint       = ()
type instance Reachable PageStaticTermsOfUse PageHomeWithLoginPrompt = ()

-- * 16. Home With Login Prompt

type instance Reachable PageHomeWithLoginPrompt PageStaticImprint    = () -- Imprint
type instance Reachable PageHomeWithLoginPrompt PageStaticTermsOfUse = () -- Terms of use
type instance Reachable PageHomeWithLoginPrompt PageRoomsOverview    = ()



----------------------------------------------------------------------
-- FIXME

{-

- Who is going to create classes and how?
- Can classes be changed during the semester?
- Why are there two "ergebnisphase" on Topics Overview?
- Why are there two "ausarbeitungphase" on Topics Overview?

-}
