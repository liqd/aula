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

module Logger.EventLog
where

import Control.Lens
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.String.Conversions
import GHC.Generics (Generic)
import Servant

import qualified Data.Aeson as Aeson
import qualified Data.Csv as CSV
import qualified Data.Text as ST
import qualified Generics.Generic.Aeson as Aeson
import qualified Generics.SOP as SOP

import Data.UriPath
import Frontend.Path as U
import Types


-- * types

data EventLog = EventLog URL [EventLogItemWarm]
  deriving (Generic)

-- | This type is migration-critial: we may need to support parting old values if we change it in
-- production.  See 'CSV.ToRecord' instance(s) below.
data EventLogItem user topic idea comment =
    EventLogItem IdeaSpace Timestamp user (EventLogItemValue user topic idea comment)
  deriving (Eq, Show, Generic)

data EventLogItemValue user topic idea comment =
    EventLogUserCreates           (Either3 topic idea comment)
  | EventLogUserEdits             (Either3 topic idea comment)
  | EventLogUserMarksIdeaFeasible idea (Maybe IdeaJuryResultType)
  | EventLogUserVotesOnIdea       idea IdeaVoteValue
  | EventLogUserVotesOnComment    idea comment (Maybe comment) UpDown
      -- FIXME: this should just be a comment key resp. comment, but following the type errors
      -- reveals some things that are not trivial to refactor.
  | EventLogUserDelegates         DelegationContext user
  | EventLogTopicNewPhase         topic Phase Phase
  | EventLogIdeaNewTopic          idea (Maybe topic) (Maybe topic)
  | EventLogIdeaReachesQuorum     idea
  deriving (Eq, Show, Generic)


type EventLogItemCold = EventLogItem (AUID User) (AUID Topic) (AUID Idea) CommentKey
type EventLogItemWarm = EventLogItem User Topic Idea Comment

type EventLogItemValueCold = EventLogItemValue (AUID User) (AUID Topic) (AUID Idea) CommentKey
type EventLogItemValueWarm = EventLogItemValue User Topic Idea Comment

type ContentCold = Either3 (AUID Topic) (AUID Idea) CommentKey
type ContentWarm = Either3 Topic Idea Comment


instance SOP.Generic EventLog
instance SOP.Generic (EventLogItem u t i c)
instance SOP.Generic (EventLogItemValue u t i c)

instance Aeson.ToJSON EventLogItemCold           where toJSON = Aeson.gtoJson
instance Aeson.ToJSON EventLogItemValueCold      where toJSON = Aeson.gtoJson

instance Aeson.FromJSON EventLogItemCold           where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON EventLogItemValueCold      where parseJSON = Aeson.gparseJson


-- * delivering the event log

filterEventLog :: IdeaSpace -> EventLog -> EventLog
filterEventLog spc (EventLog domainUrl rows) = EventLog domainUrl $ filter f rows
  where
    f (EventLogItem spc' _ _ _) = spc' == spc


eventLogItemCsvHeaders :: [String]
eventLogItemCsvHeaders = ["Ideenraum", "Zeitstempel", "Login", "Event", "Link"]


data WithURL a = WithURL URL a

instance MimeRender CSV EventLog where
    mimeRender Proxy (EventLog _ []) = "[Keine Daten]"
    mimeRender Proxy (EventLog domainUrl rows) =
        cs (intercalate "," eventLogItemCsvHeaders <> "\n")
        <> CSV.encode (WithURL domainUrl <$> rows)

instance CSV.ToRecord (WithURL EventLogItemWarm) where
    toRecord (WithURL domainUrl (EventLogItem ispace timestamp user ev)) = CSV.toRecord
        [ showIdeaSpace ispace
        , showTimestamp timestamp
        , user ^. userLogin . unUserLogin . csi
        ] <> f ev
      where
        objDesc :: ContentWarm -> ST
        objDesc (Left3   t) = "Thema " <> t ^. topicTitle . showed . csi
        objDesc (Middle3 i) = "Idee "  <> i ^. ideaTitle  . showed . csi
        objDesc (Right3  c) =
            chop $ "Verbesserungsvorschlag " <> (c ^. commentText . _Markdown . csi)

        chop :: ST -> ST
        chop s = if ST.length s <= 60 then s else ST.take 57 s <> "..."

        objLink :: ContentWarm -> ST
        objLink = (domainUrl <>) . absoluteUriPath . relPath . objLink'

        objLink' :: ContentWarm -> U.Main
        objLink' (Left3   t) = U.listTopicIdeas t
        objLink' (Middle3 i) = U.IdeaPath (i ^. ideaLocation) (U.ViewIdea (i ^. _Id) Nothing)
        objLink' (Right3  c) = U.IdeaPath iloc (U.ViewIdea iid (Just $ c ^. _Id))
          where
            iloc = c ^. _Key . ckIdeaLocation
            iid = c ^. _Key . ckIdeaId

        f (EventLogUserCreates obj) = CSV.toRecord
            [ "legt " <> objDesc obj <> " an.", objLink obj ]

        f (EventLogUserEdits obj) = CSV.toRecord
            [ "bearbeitet " <> objDesc obj <> ".", objLink obj ]

        f (EventLogUserMarksIdeaFeasible (Middle3 -> idea) mIsFeasible) = case mIsFeasible of
            Nothing           -> CSV.toRecord [ "löscht Durchführbarkeitsbewertung", objLink idea ]
            (Just isFeasible) -> let what = case isFeasible of
                                              IdeaFeasible    -> "durchführbar."
                                              IdeaNotFeasible -> "nicht durchführbar."
                                 in CSV.toRecord [ "bewertet Idee als " <> what, objLink idea ]

        f (EventLogUserVotesOnIdea (Middle3 -> idea) voteValue) = CSV.toRecord
            [ "stimmt " <> how <> " " <> objDesc idea <> ".", objLink idea ]
          where
            how = case voteValue of
                    Yes     -> "für"
                    No      -> "gegen"

        f (EventLogUserVotesOnComment (Middle3 -> idea) comment mcomment updown) = CSV.toRecord
            [ "stimmt " <> how <> " " <> what <> ".", objLink idea ]
          where
            how = case updown of
                    Up   -> "für"
                    Down -> "gegen"
            what = objDesc (Right3 $ fromMaybe comment mcomment)

        f (EventLogUserDelegates ctxDesc toUser) = CSV.toRecord
            [ "delegiert in " <> show ctxDesc <> " an " <> toUser ^. userLogin . _UserLogin . csi
            , "(kein Link verfügbar)"
            -- FIXME: there should be a link, and 'show ctxDesc' needs to be polished.
            ]

        f (EventLogTopicNewPhase (Left3 -> topic) fromPhase toPhase) = CSV.toRecord
            [ objDesc topic <> " geht von " <> uilabel fromPhase
                            <> " nach "     <> uilabel toPhase
            , objLink topic
            ]

        f (EventLogIdeaNewTopic (Middle3 -> idea) mt1 mt2) = CSV.toRecord
            [ "verschiebt " <> objDesc idea <> " von " <> show_ mt1 <> " nach " <> show_ mt2 <> "."
            , objLink idea
            ]
          where
            show_ :: Maybe Topic -> ST
            show_ mt = maybe "wilde Ideen" (view topicTitle) mt ^. showed . csi

        f (EventLogIdeaReachesQuorum (Middle3 -> idea)) = CSV.toRecord
            [ objDesc idea <> " erreicht das Quorum.", objLink idea ]
