{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Page
    ( module P
    , Reachable
    )
where

import Frontend.Page.Admin      as P
import Frontend.Page.Comment    as P
import Frontend.Page.Delegation as P
import Frontend.Page.FileUpload as P
import Frontend.Page.Idea       as P
import Frontend.Page.Login      as P
import Frontend.Page.Overview   as P
import Frontend.Page.Static     as P
import Frontend.Page.Topic      as P
import Frontend.Page.User       as P

----------------------------------------------------------------------
-- Page Map

type family Reachable from to :: *

-- Mapped from click-dummy: https://marvelapp.com/ehhb43

-- * 16. Home With Login Prompt

-- Static
type instance Reachable PageHomeWithLoginPrompt PageStaticImprint = () -- Imprint
type instance Reachable PageHomeWithLoginPrompt PageStaticTermsOfUse = () -- Terms of use

-- Content
type instance Reachable PageHomeWithLoginPrompt PageRoomsOverview = ()


-- * 1. Rooms Overview

-- Header
type instance Reachable PageRoomsOverview PageRoomsOverview = () -- Aula icon, Ideenraume

-- Menu
type instance Reachable PageRoomsOverview PageUserProfileDelegatedVotes = () -- Profil anzeigen
type instance Reachable PageRoomsOverview PageUserSettings = () -- Einstellungen
type instance Reachable PageRoomsOverview PageAdminSettingsDurationsAndQuorum = () -- Prozessverfaultung
type instance Reachable PageRoomsOverview PageLogout = ()

-- Footer
type instance Reachable PageRoomsOverview PageStaticImprint = () -- Imprint
type instance Reachable PageRoomsOverview PageStaticTermsOfUse = () -- Terms of use

-- Content
type instance Reachable PageRoomsOverview PageIdeasOverview = () -- Klasse


-- * 2. Ideas Overview

-- Header
type instance Reachable PageIdeasOverview PageRoomsOverview = () -- Aula icon, Ideenraume

-- Menu
type instance Reachable PageIdeasOverview PageUserProfileDelegatedVotes = () -- Profil anzeigen
type instance Reachable PageIdeasOverview PageUserSettings = () -- Einstellungen
type instance Reachable PageIdeasOverview PageAdminSettingsDurationsAndQuorum = () -- Prozessverfaultung
type instance Reachable PageIdeasOverview PageLogout = ()

-- Footer
type instance Reachable PageIdeasOverview PageStaticImprint = () -- Imprint
type instance Reachable PageIdeasOverview PageStaticTermsOfUse = () -- Terms of use

-- Content
type instance Reachable PageIdeasOverview ViewTopic = () -- Ideen auf dem Tisch def Klasse 7a (Topic Overview)
type instance Reachable PageIdeasOverview CreateIdea = () -- Neue Idee (Create Idea)
type instance Reachable PageIdeasOverview ViewIdea = () -- Titel einer Idee (Idea detail page: New Ideas)


-- * 6. Create Idea

-- On the create dummy it is an overlay
-- type instance Reachable CreateIdea PageLogout = ()
-- type instance Reachable CreateIdea PageStaticImprint = () -- Imprint
-- type instance Reachable CreateIdea PageStaticTermsOfUse = () -- Terms of use

type instance Reachable CreateIdea PageIdeasOverview = () -- X, Cancel, Idee veroffentlichen
-- type instance Reachable CreateIdea IdeeErsteltLayout = () -- Idee veroffentlichen ???


-- * 5.1 Idea detail pages: New Ideas

-- Header
type instance Reachable ViewIdea PageRoomsOverview = () -- Aula icon, Ideenraume

-- Menu
type instance Reachable ViewIdea PageUserProfileDelegatedVotes = () -- Profil anzeigen
type instance Reachable ViewIdea PageUserSettings = () -- Einstellungen
type instance Reachable ViewIdea PageAdminSettingsDurationsAndQuorum = () -- Prozessverfaultung
type instance Reachable ViewIdea PageLogout = ()

-- Footer
type instance Reachable ViewIdea PageStaticImprint = () -- Imprint
type instance Reachable ViewIdea PageStaticTermsOfUse = () -- Terms of use

-- Content
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

-- * 4. Topic Overview

-- type instance Reachable ViewTopic = ()

-- Header
type instance Reachable ViewTopic PageRoomsOverview = () -- Aula icon, Ideenraume

-- Menu
type instance Reachable ViewTopic PageUserProfileDelegatedVotes = () -- Profil anzeigen
type instance Reachable ViewTopic PageUserSettings = () -- Einstellungen
type instance Reachable ViewTopic PageAdminSettingsDurationsAndQuorum = () -- Prozessverfaultung
type instance Reachable ViewTopic PageLogout = ()

-- Footer
type instance Reachable ViewTopic PageStaticImprint = () -- Imprint
type instance Reachable ViewTopic PageStaticTermsOfUse = () -- Terms of use

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

-- * 8.1 User Profile: Created Ideas

-- type instance Reachable PageUserProfileCreateIdeas = ()

-- Header
type instance Reachable PageUserProfileCreateIdeas PageRoomsOverview = () -- Aula icon, Ideenraume

-- Menu
type instance Reachable PageUserProfileCreateIdeas PageUserProfileDelegatedVotes = () -- Profil anzeigen
type instance Reachable PageUserProfileCreateIdeas PageUserSettings = () -- Einstellungen
type instance Reachable PageUserProfileCreateIdeas PageAdminSettingsDurationsAndQuorum = () -- Prozessverfaultung
type instance Reachable PageUserProfileCreateIdeas PageLogout = ()

-- Footer
type instance Reachable PageUserProfileCreateIdeas PageStaticImprint = () -- Imprint
type instance Reachable PageUserProfileCreateIdeas PageStaticTermsOfUse = () -- Terms of use

-- type instance Reachable PageUserProfileCreateIdeas PageUserProfileDelegatedVotes = () -- Erhaltene Stimmen
type instance Reachable PageUserProfileCreateIdeas ViewIdea = () -- Titel einer Idee (5.1 Idea Detail Page: New Ideas)

----------------------------------------------------------------------
-- Questions
{-
 * Who is going to create classes and how?
 * Can classes be changed during the semester?
 * Why are there two "ergebnishphase" on Topics Overview?
 * Why are there two "ausarbeitungphase" on Topics Overview?
-}
