{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE Rank2Types                  #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module EventLog
where

import Control.Lens
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.String.Conversions
import GHC.Generics (Generic)
import Servant

import qualified Data.Csv as CSV
import qualified Data.Text as ST
import qualified Generics.SOP as SOP

import Data.UriPath
import Frontend.Path as U
import Types


-- * event logs

data EventLog = EventLog URL [EventLogItem]
  deriving (Eq, Ord, Show, Read, Generic)

data EventLogItem = EventLogItem IdeaSpace Timestamp User EventLogItemValue
  deriving (Eq, Ord, Show, Read, Generic)

data URLEventLogItem = URLEventLogItem URL EventLogItem

data EventLogItemValue =
    EventLogUserCreates           (Either3 Topic Idea Comment)
  | EventLogUserEdits             (Either3 Topic Idea Comment)
  | EventLogUserMarksIdeaFeasible Idea IdeaJuryResultType
  | EventLogUserVotesOnIdea       Idea IdeaVoteValue
  | EventLogUserVotesOnComment    Idea Comment (Maybe Comment) UpDown
  | EventLogUserDelegates         ST User
  | EventLogTopicNewPhase         Topic Phase Phase PhaseTransitionTriggeredBy
  | EventLogIdeaNewTopic          Idea (Maybe Topic) (Maybe Topic)
  | EventLogIdeaReachesQuorum     Idea
  deriving (Eq, Ord, Show, Read, Generic)

data PhaseTransitionTriggeredBy =
    PhaseTransitionTriggeredBy User
  | PhaseTransitionTriggeredByTimeout
  | PhaseTransitionTriggeredByAllIdeasMarked
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic EventLog
instance SOP.Generic EventLogItem
instance SOP.Generic EventLogItemValue
instance SOP.Generic PhaseTransitionTriggeredBy


filterEventLog :: Maybe IdeaSpace -> EventLog -> EventLog
filterEventLog mspc (EventLog domainUrl rows) = EventLog domainUrl $ filter f rows
  where
    f (EventLogItem spc' _ _ _) = maybe True (== spc') mspc


eventLogItemCsvHeaders :: [String]
eventLogItemCsvHeaders = ["Ideenraum", "Zeitstempel", "Login", "Event", "Link"]


instance MimeRender CSV EventLog where
    mimeRender Proxy (EventLog _ []) = "[Keine Daten]"
    mimeRender Proxy (EventLog domainUrl rows) =
        cs (intercalate "," eventLogItemCsvHeaders <> "\n")
        <> CSV.encode (URLEventLogItem domainUrl <$> rows)


instance CSV.ToRecord URLEventLogItem where
    toRecord (URLEventLogItem domainUrl (EventLogItem ispace timestamp user ev)) = CSV.toRecord
        [ showIdeaSpace ispace
        , showTimestamp timestamp
        , user ^. userLogin . unUserLogin . csi
        ] <> f ev
      where
        objDesc :: Either3 Topic Idea Comment -> ST
        objDesc (Left3   t) = "Thema " <> t ^. topicTitle . showed . csi
        objDesc (Middle3 i) = "Idee "  <> i ^. ideaTitle  . showed . csi
        objDesc (Right3  c) = "Verbesserungsvorschlag "
                           <> ST.take 30 (c ^. commentText . to unMarkdown . csi) <> "..."

        objLink :: Either3 Topic Idea Comment -> ST
        objLink = (domainUrl <>) . absoluteUriPath . relPath . objLink'

        objLink' :: Either3 Topic Idea Comment -> U.Main
        objLink' (Left3   t) = U.listTopicIdeas t
        objLink' (Middle3 i) = U.IdeaPath (i ^. ideaLocation) (U.ViewIdea (i ^. _Id))
        objLink' (Right3 _c) = U.Broken

        f (EventLogUserCreates obj) = CSV.toRecord
            [ "legt " <> objDesc obj <> " an.", objLink obj ]

        f (EventLogUserEdits obj) = CSV.toRecord
            [ "bearbeitet " <> objDesc obj <> ".", objLink obj ]

        f (EventLogUserMarksIdeaFeasible (Middle3 -> idea) isFeasible) = CSV.toRecord
            [ "bewertet Idee als " <> what, objLink idea ]
          where
            what = case isFeasible of
                     IdeaFeasible    -> "durchführbar."
                     IdeaNotFeasible -> "nicht durchführbar."

        f (EventLogUserVotesOnIdea (Middle3 -> idea) voteValue) = CSV.toRecord
            [ "stimmt " <> how <> " " <> objDesc idea <> ".", objLink idea ]
          where
            how = case voteValue of
                    Yes     -> "für"
                    No      -> "gegen"
                    Neutral -> "neutral zu"

        f (EventLogUserVotesOnComment (Middle3 -> idea) comment mcomment updown) = CSV.toRecord
            [ "stimmt " <> how <> " " <> what <> ".", objLink idea ]
          where
            how = case updown of
                    Up   -> "für"
                    Down -> "gegen"
            what = objDesc (Right3 $ fromMaybe comment mcomment)

        f (EventLogUserDelegates ctxDesc toUser) = CSV.toRecord
            [ "delegiert in " <> ctxDesc <> " an " <> toUser ^. userLogin . _UserLogin . csi
            , "(kein Link verfügbar)"
            ]

        f (EventLogTopicNewPhase (Left3 -> topic) fromPhase toPhase trigger) = CSV.toRecord
            [ objDesc topic <> " geht von " <> p1 <> " nach " <> p2 <> showTrigger trigger
            , objLink topic
            ]
          where
            p1 = phaseName fromPhase
            p2 = phaseName toPhase

            showTrigger (PhaseTransitionTriggeredBy _)           = " (von Hand ausgelöst)"
            showTrigger PhaseTransitionTriggeredByTimeout        = " (Zeit ist abgelaufen)"
            showTrigger PhaseTransitionTriggeredByAllIdeasMarked = " (alle Ideen sind geprüft)"

        f (EventLogIdeaNewTopic (Middle3 -> idea) mt1 mt2) = CSV.toRecord
            [ "verschiebt " <> objDesc idea <> " von " <> show_ mt1 <> " nach " <> show_ mt2 <> "."
            , objLink idea
            ]
          where
            show_ mt = maybe "wilde Ideen" (view topicTitle) mt ^. showed . csi

        f (EventLogIdeaReachesQuorum (Middle3 -> idea)) = CSV.toRecord
            [ objDesc idea <> " erreicht das Quorum.", objLink idea ]
